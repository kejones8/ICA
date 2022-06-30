#this script intersects states and counties ONLY for incident_ids that burned OR threatened that left federal land 
#it then counts the states and counties involved in each incident 

library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)

#read in burn & threat intersections
surfman_int_burnarea<-read_sf(burn_surfman_inter_out)

surfman_int_threatarea<-read_sf(threat_surfman_inter_out)

#where JrsdcUK is NA, make census
surfman_int_burnarea$JrsdcUK[is.na(surfman_int_burnarea$JrsdcUK)]<-"census"
surfman_int_threatarea$JrsdcUK[is.na(surfman_int_threatarea$JrsdcUK)]<-"census"

nonfed_surfman_burn<-surfman_int_burnarea[surfman_int_burnarea$JrsdcUK!="Federal",]
nonfed_surfman_burn_buf<-st_buffer(nonfed_surfman_burn,0)
colnames(nonfed_surfman_burn_buf)[1]<-"Event_ID"
#nonfed_surfman_burn_buf<-nonfed_surfman_burn_buf[!is.na(nonfed_surfman_burn_buf$Event_ID),]

nonfed_burn_diss <- nonfed_surfman_burn_buf %>% group_by(Event_ID) %>% summarize()
write_sf(nonfed_burn_diss,nonfed_burn_diss_out)

#nonfed_surfman_burn_union<-st_union(nonfed_surfman_burn)
#write_sf(nonfed_surfman_burn_union,"data\\JCA\\nonfed_burn_stcntyuse.shp")
#nonfed_surfman_burn_union<-read_sf("data\\JCA\\nonfed_burn_stcntyuse.shp")

nonfed_surfman_threat<-surfman_int_threatarea[surfman_int_threatarea$JrsdcUK!="Federal",]
nonfed_surfman_threat_buf<-st_buffer(nonfed_surfman_threat,0)
colnames(nonfed_surfman_threat_buf)[1]<-"Event_ID"
#nonfed_surfman_threat_buf<-nonfed_surfman_threat_buf[!is.na(nonfed_surfman_threat_buf$Evnt_ID),]
nonfed_threat_diss <- nonfed_surfman_threat_buf %>% group_by(Event_ID) %>% summarize()
write_sf(nonfed_threat_diss,nonfed_threat_diss_out)
#nonfed_surfman_threat_union<-st_union(nonfed_surfman_threat)
#write_sf(nonfed_surfman_threat_union,"data\\JCA\\nonfed_threat_stcntyuse.shp")
#nonfed_surfman_threat_union<-read_sf("data\\JCA\\nonfed_threat_stcntyuse.shp")
#nonfed_threat_union_buf<-st_buffer(nonfed_surfman_threat_union,0)


#intersect each of these with state and county files

#read in national county data
cnty<-read_sf("data\\county\\tl_2017_us_county\\tl_2017_us_county.shp")
cnty_proj<-st_transform(cnty,5070)
cnty_buf<-st_buffer(cnty_proj,0)
#read in national state date
state<-read_sf("data\\states\\tl_2017_us_state\\tl_2017_us_state.shp")
state_proj<-st_transform(state,5070)
state_buf<-st_buffer(state_proj,0)

# #does this work wtih single multipolygon geom
# nonfed_sfman_burn_st<-st_intersects(nonfed_surfman_burn_union,state_proj)
# nonfed_sfman_threat_st<-st_intersects(nonfed_surfman_threat_union,state_proj)
# 
# nonfed_sfman_burn_cnty<-st_intersects(nonfed_surfman_burn_union,cnty_proj)
# nonfed_sfman_threat_cnty<-st_intersects(nonfed_surfman_threat_union,cnty_proj)


# 
#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv(jca_samp_in)

#read in mtbs burned area our sample footprints
mtbs_burn<-read_sf(select_mtbs_out)
burn_proj<-st_transform(mtbs_burn,5070)
burn_buf<-st_buffer(burn_proj,0)
#read in mtbs threatened area our sample
mtbs_threat<-read_sf(threat_work_out)
threat_proj<-st_transform(mtbs_threat,5070)
threat_buf<-st_buffer(threat_proj,0)
# 
# # from the spatial data of our sample, only want to intersect with states and counties
# #the mtbs ids/incidentids that are represented in the burned & threatened nonfed csvs csv
# burn_incid_nonfed<-read.csv("data\\JCA\\burn_incid_nonfed.csv")
# burn_incid_nonfed$X<-NULL
# colnames(burn_incid_nonfed)[1]<-"incident_id"
# threat_incid_nonfed<-read.csv("data\\JCA\\threat_incid_nonfed.csv")
# threat_incid_nonfed$X<-NULL
# colnames(threat_incid_nonfed)[1]<-"incident_id"
# 
# #get the mtbs ids for the incidents we want to tally state & county for
# get_burn_st_cnt<-merge(burn_incid_nonfed,mtbs_withincid,by="incident_id")#,all.x = TRUE)
# 
# get_threat_st_cnt<-merge(threat_incid_nonfed,mtbs_withincid,by="incident_id")#,all.x = TRUE)
# 
# 
# #now get the footprints for the ids confirmed we want above
# #burn
# burn_tointer_stcnty<-burn_proj[burn_proj$Event_ID %in% unique(get_burn_st_cnt$mtbs_ids),]
# 
# #threat
# threat_tointer_stcnty<-threat_proj[threat_proj$Event_ID %in% unique(get_threat_st_cnt$mtbs_ids),]


##now do the actual intersecting so we can get to counting

test<-rbind(burn_proj,threat_proj)

#pair down counties & states
cnty_proj$indicator <- st_intersects(cnty_proj,test)%>% lengths > 0
state_proj$indicator <- st_intersects(state_proj,test)%>% lengths > 0

cnty_tointer<-cnty_proj[cnty_proj$indicator==TRUE,]
state_tointer<-state_proj[state_proj$indicator==TRUE,]


#now intersect, states & counties with nonfed lands
# nonfed_state_inter<-st_intersection(nonfed_surfman_burn_union,state_tointer)
# write_sf(nonfed_state_inter,"data\\JCA\\nonfed_burn_st.shp")
# burn_nonfed_st_inter<-nonfed_state_inter
# nonfed_cnty_inter<-st_intersection(nonfed_surfman_threat_union,cnty_tointer)
# write_sf(nonfed_cnty_inter,"data\\JCA\\nonfed_threat_cnty.shp")
# threat_nonfed_cnty_inter<-nonfed_cnty_inter

#parallelize these too...
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#is this right??
non_fed_threat_mtbsids<-unique(nonfed_threat_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_nonfed_cnty_inter<-foreach(i=non_fed_threat_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_threat_diss[nonfed_threat_diss$Event_ID==i,]
  cnty_forthreat<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm


registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#is this right??
non_fed_threat_mtbsids<-unique(nonfed_threat_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
threat_nonfed_st_inter<-foreach(i=non_fed_threat_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_threat_diss[nonfed_threat_diss$Event_ID==i,]
  st_forthreat<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm



registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#is this right??
non_fed_burn_mtbsids<-unique(nonfed_burn_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_nonfed_cnty_inter<-foreach(i=non_fed_burn_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_burn_diss[nonfed_burn_diss$Event_ID==i,]
  cnty_forburn<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#is this right??
non_fed_burn_mtbsids<-unique(nonfed_burn_diss$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_nonfed_st_inter<-foreach(i=non_fed_burn_mtbsids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-nonfed_burn_diss[nonfed_burn_diss$Event_ID==i,]
  st_forburn<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm











# 
# ######Change what files are going into the final intersection beneath this
# 
# #first do burned intersection with state
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# burned_ids<-unique(burn_tointer_stcnty$Event_ID)
# 
# #for every burned area mtbs footprint, intersect with surface management 
# #write out combined sf object with all intersections
# burn_st<-foreach(i=burned_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
#   
#   fp<-burn_tointer_stcnty[burn_tointer_stcnty$Event_ID==i,]
#   st_forburns<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
#   
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm
# 
# #now, join back with incident ids
# burn_st_withincid<-merge(burn_st,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
# 
# burn_st_trim<-burn_st_withincid[,c("incident_id","Event_ID","GEOID")]
# burn_st_trim$geometry<-NULL
# 
# burn_st_uni<-unique(burn_st_trim)
# 
# 
# #burned intersect with counties
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# 
# #intersecting burned data with counties
# burn_cnty<-foreach(i=burned_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
#   
#   fp<-burn_tointer_stcnty[burn_tointer_stcnty$Event_ID==i,]
#   cnty_forburns<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
#   
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm
# 
# #now, join back with incident ids
# burn_cnty_withincid<-merge(burn_cnty,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
# 
# burn_cnty_trim<-burn_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
# burn_cnty_trim$geometry<-NULL
# 
# burn_cnty_uni<-unique(burn_cnty_trim)
# 
# ##now do threatened
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# threat_ids<-unique(threat_tointer_stcnty$Event_ID)
# buf_threat<-st_buffer(threat_tointer_stcnty,0)
# #intersecting burned data with counties
# threat_st<-foreach(i=threat_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
#   
#   threat_fp<-buf_threat[buf_threat$Event_ID==i,]
#   st_forthreat<-st_intersection(threat_fp,state_tointer)#5 miles = 8047 meters
#   
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm


#now, join back with incident ids
burn_st_withincid<-merge(burn_nonfed_st_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
burn_st_trim<-burn_st_withincid[,c("incident_id","Event_ID","GEOID")]
burn_st_trim$geometry<-NULL

burn_st_uni<-unique(burn_st_trim)


#now, join back with incident ids
burn_cnty_withincid<-merge(burn_nonfed_cnty_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
burn_cnty_trim<-burn_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
burn_cnty_trim$geometry<-NULL

burn_cnty_uni<-unique(burn_cnty_trim)


#now, join back with incident ids
threat_st_withincid<-merge(threat_nonfed_st_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_st_trim<-threat_st_withincid[,c("incident_id","Event_ID","GEOID")]
threat_st_trim$geometry<-NULL

threat_st_uni<-unique(threat_st_trim)


#now, join back with incident ids
threat_cnty_withincid<-merge(threat_nonfed_cnty_inter,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_cnty_trim<-threat_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
threat_cnty_trim$geometry<-NULL

threat_cnty_uni<-unique(threat_cnty_trim)



#NEED TO MAKE SURE WE REMOVE STATE/COUNTIES from threatened if they occur in burned!
#if geoid exists for that incident id for burned, mark it to remove
threat_cnty_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_cnty_uni$incident_id, threat_cnty_uni$GEOID), 
                                paste0(burn_cnty_uni$incident_id, burn_cnty_uni$GEOID))),FALSE, TRUE)

threat_st_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_st_uni$incident_id, threat_st_uni$GEOID), 
                                                  paste0(burn_st_uni$incident_id, burn_st_uni$GEOID))),FALSE, TRUE)


threat_cnt_jurs_tokeep<-threat_cnty_uni[threat_cnty_uni$torm_inburn==FALSE,]
threat_st_jurs_tokeep<-threat_st_uni[threat_st_uni$torm_inburn==FALSE,]

##now write out tables for counting
threat_cnty_cnt<-threat_cnt_jurs_tokeep %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))
threat_st_cnt<-threat_st_jurs_tokeep %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))

write.csv(threat_cnty_cnt,threat_county_count_out)
write.csv(threat_st_cnt,threat_state_count_out)

burn_cnty_cnt<-burn_cnty_uni %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))
burn_st_cnt<-burn_st_uni %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))

write.csv(burn_cnty_cnt,burn_county_count_out)
write.csv(burn_st_cnt,burn_state_count_out)
