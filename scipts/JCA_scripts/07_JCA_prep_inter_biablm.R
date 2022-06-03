###this script reads in blm & bia shapefile data 
library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)

bia<-read_sf("data\\BIA_Regions.shp\\BIA_Regions.shp")
bia_proj<-st_transform(bia,5070)
bia_buf<-st_buffer(bia_proj,0)

get_ids<-bia_buf[11:23,c("RegionID")]
get_ids$geometry<-NULL
bia_sel<-bia_buf[bia_buf$RegionID %in% get_ids$RegionID,]
###READ in the correct file
blm<-read_sf("data\\BLM_National_Administrative_Unit_B.gdb\\blm_district_clean2\\blm_district_clean2.shp")
blm_proj<-st_transform(blm,5070)
#blm_buf<-st_buffer(blm_proj,0)

#read in spatial file that grabs all surfman that intersect burned and threatened areas
surfman_int_burnarea<-read_sf("data\\JCA\\burn_surfman_inter.shp")
surfman_int_threatarea<-read_sf("data\\JCA\\threat_surfman_inter.shp")

#get the bia & blm areas out of those shapefiles
surfman_burn_bia<-surfman_int_burnarea[surfman_int_burnarea$JrsdcUA=="BIA",]
surfman_threat_bia<-surfman_int_threatarea[surfman_int_threatarea$JrsdcUA=="BIA",]

surfman_burn_blm<-surfman_int_burnarea[surfman_int_burnarea$JrsdcUA=="BLM",]
surfman_threat_blm<-surfman_int_threatarea[surfman_int_threatarea$JrsdcUA=="BLM",]


#now, intersect the surfman_burn_bia with the BIA shapefile - this will put the correct BIA regions into the surfman data
surfman_burn_bia_regions<-st_intersection(surfman_burn_bia,bia_sel)
surfman_threat_bia_regions<-st_intersection(surfman_threat_bia,bia_sel)


surfman_burn_blm_regions<-st_intersection(surfman_burn_blm,blm_proj)
surfman_threat_blm_regions<-st_intersection(surfman_threat_blm,blm_proj)

#need to intersect blm & bia with threatened and burned data - not many bia/blm regions, so no st_intersects for now

#read in mtbs burned area our sample footprints
mtbs_burn<-read_sf("data\\JCA\\mtbs_match_jcasamp.shp")
burn_proj<-st_transform(mtbs_burn,5070)
#read in mtbs threatened area our sample 
mtbs_threat<-read_sf("data\\JCA\\mtbs_match_jcasamp_threat.shp")
threat_proj<-st_transform(mtbs_threat,5070)
threat_buf<-st_buffer(threat_proj,0)


#first do burned intersection with state
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

burn_ids<-unique(burn_proj$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_bia<-foreach(i=burn_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burn_proj[burn_proj$Event_ID==i,]
  bia_forburns<-st_intersection(fp,surfman_burn_bia_regions)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm


#then threat intersections with bia
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

threat_ids<-unique(threat_buf$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_bia<-foreach(i=threat_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp_threat<-threat_buf[threat_buf$Event_ID==i,]
  bia_forthreat<-st_intersection(fp_threat,surfman_threat_bia_regions)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm


#then burn intersections with blm

registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_blm<-foreach(i=burn_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burn_proj[burn_proj$Event_ID==i,]
  blm_forbrn<-st_intersection(fp,surfman_burn_blm_regions)#5 miles = 8047 meters

}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm





#then burn intersections with blm
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_blm<-foreach(i=burn_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  threat_fp<-threat_buf[threat_buf$Event_ID==i,]
  blm_forthreat<-st_intersection(threat_fp,surfman_threat_blm_regions)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm




#now, want to join the threat/burn bia/blm data to the mtbs_id & incident ids
mtbs_incid<-read.csv("data\\JCA\\JCAsamp_inc_mtbsid.csv")

threat_blm_incid<-merge(threat_blm,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")
burn_blm_incid<-merge(burn_blm,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")

threat_blm_incid$geometry<-NULL
burn_blm_incid$geometry<-NULL



threat_bia_incid<-merge(threat_bia,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")
burn_bia_incid<-merge(burn_bia,mtbs_incid,by.x="Event_ID",by.y="mtbs_ids")

threat_bia_incid$geometry<-NULL
burn_bia_incid$geometry<-NULL

#now that the above operations have been run, we want to remove any ids from the threatened that already appear in burned
threat_blm_incid$torm_inburn <- ifelse(is.na(match(paste0(threat_blm_incid$incident_id, threat_blm_incid$PARENT_NAM), 
                                                  paste0(burn_blm_incid$incident_id,burn_blm_incid$PARENT_NAM))),FALSE, TRUE)

threat_bia_incid$torm_inburn <- ifelse(is.na(match(paste0(threat_bia_incid$incident_id, threat_bia_incid$RegionAbbv), 
                                                   paste0(burn_bia_incid$incident_id, burn_bia_incid$RegionAbbv))),FALSE, TRUE)


#now, remove juris from threatened bia & blm that already appear in burned
#keep records where torm_inburn==FALSE

threat_blm_rmburn<-threat_blm_incid[threat_blm_incid$torm_inburn==FALSE,]
threat_bia_rmburn<-threat_bia_incid[threat_bia_incid$torm_inburn==FALSE,]

threat_blm_unique<-unique(threat_blm_rmburn[,c("incident_id","PARENT_NAM")])
threat_bia_unique<-unique(threat_bia_rmburn[,c("incident_id","RegionAbbv")])

threat_blm_cnt<-threat_blm_unique %>% group_by(incident_id) %>% summarize(cnt_blm_threat=n_distinct(PARENT_NAM))
threat_bia_cnt<-threat_bia_unique %>% group_by(incident_id) %>% summarize(cnt_bia_threat=n_distinct(RegionAbbv))

write.csv(threat_blm_cnt,"data\\JCA\\threat_blm_cnt.csv")
write.csv(threat_bia_cnt,"data\\JCA\\threat_bia_cnt.csv")

burn_blm_unique<-unique(burn_blm_incid[,c("incident_id","PARENT_NAM")])
burn_bia_unique<-unique(burn_bia_incid[,c("incident_id","RegionAbbv")])

burn_blm_cnt<-burn_blm_unique %>% group_by(incident_id) %>% summarize(cnt_blm_burn=n_distinct(PARENT_NAM))
burn_bia_cnt<-burn_bia_unique %>% group_by(incident_id) %>% summarize(cnt_bia_burn=n_distinct(RegionAbbv))

write.csv(burn_blm_cnt,"data\\JCA\\burn_blm_cnt.csv")
write.csv(burn_bia_cnt,"data\\JCA\\burn_bia_cnt.csv")
