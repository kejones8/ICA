#this script intersects states and counties ONLY for incident_ids that burned OR threatened that left federal land 
#it then counts the states and counties involved in each incident 

library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)

#read in national county data
cnty<-read_sf("data\\county\\tl_2017_us_county\\tl_2017_us_county.shp")
cnty_proj<-st_transform(cnty,5070)
#read in national state date
state<-read_sf("data\\states\\tl_2017_us_state\\tl_2017_us_state.shp")
state_proj<-st_transform(state,5070)

#read in incidents that should count county/stae (i.e. burned or threatened non-federal lands)
burn_stcty<-read.csv("data\\JCA\\burn_incid_count_cntyst.csv")
burn_stcty$X<-NULL
colnames(burn_stcty)<-"INCIDENT_ID"

threat_stcty<-read.csv("data\\JCA\\threat_incid_count_cntyst.csv")
threat_stcty$X<-NULL
colnames(threat_stcty)<-"INCIDENT_ID"

#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv("data\\JCA\\JCAsamp_inc_mtbsid.csv")

#read in mtbs burned area our sample footprints
mtbs_burn<-read_sf("data\\JCA\\mtbs_match_jcasamp.shp")
burn_proj<-st_transform(mtbs_burn,5070)
#read in mtbs threatened area our sample 
mtbs_threat<-read_sf("data\\JCA\\mtbs_match_jcasamp_threat.shp")
threat_proj<-st_transform(mtbs_threat,5070)

#from the spatial data of our sample, only want to intersect with states and counties
#the mtbs ids/incidentids that are represented in the burned & threatened csv

#get the mtbs ids for the incidents we want to tally state & county for
get_burn_st_cnt<-merge(burn_stcty,mtbs_withincid,by.x="INCIDENT_ID",by.y="incident_id")#,all.x = TRUE)

get_threat_st_cnt<-merge(threat_stcty,mtbs_withincid,by.x="INCIDENT_ID",by.y="incident_id")#,all.x = TRUE)


#now get the footprints for the ids confirmed we want above
#burn
burn_tointer_stcnty<-burn_proj[burn_proj$Event_ID %in% unique(get_burn_st_cnt$mtbs_ids),]

#threat
threat_tointer_stcnty<-threat_proj[threat_proj$Event_ID %in% unique(get_threat_st_cnt$mtbs_ids),]


##now do the actual intersecting so we can get to counting

test<-rbind(burn_proj,threat_proj)

#pair down counties
cnty_proj$indicator <- st_intersects(cnty_proj,test)%>% lengths > 0
state_proj$indicator <- st_intersects(state_proj,test)%>% lengths > 0

cnty_tointer<-cnty_proj[cnty_proj$indicator==TRUE,]
state_tointer<-state_proj[state_proj$indicator==TRUE,]



#first do burned intersection with state
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

burned_ids<-unique(burn_tointer_stcnty$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_st<-foreach(i=burned_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burn_tointer_stcnty[burn_tointer_stcnty$Event_ID==i,]
  st_forburns<-st_intersection(fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#now, join back with incident ids
burn_st_withincid<-merge(burn_st,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

burn_st_trim<-burn_st_withincid[,c("incident_id","Event_ID","GEOID")]
burn_st_trim$geometry<-NULL

burn_st_uni<-unique(burn_st_trim)


#burned intersect with counties
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


#intersecting burned data with counties
burn_cnty<-foreach(i=burned_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burn_tointer_stcnty[burn_tointer_stcnty$Event_ID==i,]
  cnty_forburns<-st_intersection(fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#now, join back with incident ids
burn_cnty_withincid<-merge(burn_cnty,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

burn_cnty_trim<-burn_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
burn_cnty_trim$geometry<-NULL

burn_cnty_uni<-unique(burn_cnty_trim)

##now do threatened
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

threat_ids<-unique(threat_tointer_stcnty$Event_ID)
buf_threat<-st_buffer(threat_tointer_stcnty,0)
#intersecting burned data with counties
threat_st<-foreach(i=threat_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  threat_fp<-buf_threat[buf_threat$Event_ID==i,]
  st_forthreat<-st_intersection(threat_fp,state_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#now, join back with incident ids
threat_st_withincid<-merge(threat_st,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_st_trim<-threat_st_withincid[,c("incident_id","Event_ID","GEOID")]
threat_st_trim$geometry<-NULL

threat_st_uni<-unique(threat_st_trim)

#threat counties
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

threat_ids<-unique(threat_tointer_stcnty$Event_ID)
buf_threat<-st_buffer(threat_tointer_stcnty,0)
#intersecting burned data with counties
threat_cnty<-foreach(i=threat_ids, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  threat_fp<-buf_threat[buf_threat$Event_ID==i,]
  cnty_forthreat<-st_intersection(threat_fp,cnty_tointer)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#now, join back with incident ids
threat_cnty_withincid<-merge(threat_cnty,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
threat_cnty_trim<-threat_cnty_withincid[,c("incident_id","Event_ID","GEOID")]
threat_cnty_trim$geometry<-NULL

threat_cnty_uni<-unique(threat_cnty_trim)



#NEED TO MAKE SURE WE REMOVE STATE/COUNTIES from threatened if they occur in burned!
#if geoid exists for that incident id for burned, mark it to remove
threat_cnty_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_cnty_uni$incident_id, threat_cnty_uni$Event_ID,threat_cnty_uni$GEOID), 
                                paste0(burn_cnty_uni$incident_id, burn_cnty_uni$Event_ID,burn_cnty_uni$GEOID))),FALSE, TRUE)

threat_st_uni$torm_inburn <- ifelse(is.na(match(paste0(threat_st_uni$incident_id, threat_st_uni$Event_ID,threat_st_uni$GEOID), 
                                                  paste0(burn_st_uni$incident_id, burn_st_uni$Event_ID,burn_st_uni$GEOID))),FALSE, TRUE)


threat_cnt_jurs_tokeep<-threat_cnty_uni[threat_cnty_uni$torm_inburn==FALSE,]
threat_st_jurs_tokeep<-threat_st_uni[threat_st_uni$torm_inburn==FALSE,]

##now write out tables for counting
threat_cnty_cnt<-threat_cnt_jurs_tokeep %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))
threat_st_cnt<-threat_st_jurs_tokeep %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))

write.csv(threat_cnty_cnt,"data\\JCA\\threat_cnty_count.csv")
write.csv(threat_cnty_cnt,"data\\JCA\\threat_st_count.csv")

burn_cnty_cnt<-burn_cnty_uni %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))
burn_st_cnt<-burn_st_uni %>% group_by(incident_id) %>% summarize(count=n_distinct(GEOID))

write.csv(burn_cnty_cnt,"data\\JCA\\burn_cnty_count.csv")
write.csv(burn_st_cnt,"data\\JCA\\burn_st_count.csv")
