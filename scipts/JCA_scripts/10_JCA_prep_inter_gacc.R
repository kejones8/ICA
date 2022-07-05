#this script intersects the footprints with gaccs and counts the number of gaccs each footprints intersects and threatens
library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)

#read in gacc
gacc<-read_sf("data\\National_GACC_Boundaries-shp\\National_GACC_Current_20200226.shp")
gacc_proj<-st_transform(gacc,5070)
gacc_buf<-st_buffer(gacc_proj,0)

#read in mtbs burned area our sample footprints
mtbs_burn<-read_sf(select_mtbs_in)
burn_proj<-st_transform(mtbs_burn,5070)
#read in mtbs threatened area our sample 
mtbs_threat<-read_sf(threat_work_out)
threat_proj<-st_transform(mtbs_threat,5070)
threat_buf<-st_buffer(threat_proj,0)


#first do burned intersection with gaccs & burned footprints
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

burn_ids<-unique(burn_proj$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_gacc<-foreach(i=burn_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burn_proj[burn_proj$Event_ID==i,]
  gacc_forburns<-st_intersection(fp,gacc_buf)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm



#second do burned intersection with gaccs & threatened
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

threat_ids<-unique(threat_buf$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_gacc<-foreach(i=threat_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  threat_fp<-threat_buf[threat_buf$Event_ID==i,]
  gacc_forthreat<-st_intersection(threat_fp,gacc_buf)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm




#now, want to join the threat/burn gacc data to the mtbs_id & incident ids
#and remove threatened gaccs that are already represented in burned gaccs 
mtbs_incid<-read.csv(jca_samp_in)

threat_gacc_incid<-merge(threat_gacc,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")
burn_gacc_incid<-merge(burn_gacc,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")

threat_gacc_incid$geometry<-NULL
burn_gacc_incid$geometry<-NULL


#now that the above operations have been run, we want to remove any ids from the threatened that already appear in burned
threat_gacc_incid$torm_inburn <- ifelse(is.na(match(paste0(threat_gacc_incid$incident_id,threat_gacc_incid$GACCAbbrev), 
                                                   paste0(burn_gacc_incid$incident_id,burn_gacc_incid$GACCAbbrev))),FALSE, TRUE)


#now, remove juris from threatened bia & blm that already appear in burned
#keep records where torm_inburn==FALSE

threat_gacc_rmburn<-threat_gacc_incid[threat_gacc_incid$torm_inburn==FALSE,]

threat_gacc_unique<-unique(threat_gacc_rmburn[,c("incident_id","GACCAbbrev")])


threat_blm_cnt<-threat_gacc_unique %>% group_by(incident_id) %>% dplyr::summarize(cnt_gacc_threat=n_distinct(GACCAbbrev))


write.csv(threat_blm_cnt,threat_gacc_count_out)

burn_gacc_unique<-unique(burn_gacc_incid[,c("incident_id","GACCAbbrev")])

#write this out for final plotting code
write.csv(burn_gacc_unique,"data\\JCA\\incident_gaccburned.csv")

burn_gacc_cnt<-burn_gacc_incid %>% group_by(incident_id) %>% dplyr::summarize(cnt_gacc_burn=n_distinct(GACCAbbrev))


write.csv(burn_gacc_cnt,burn_gacc_count_out)


