#this is the script that intersects the surface management dataset 
#with both the burned & threatened footprints 

#it writes out: 

#1. shapefile of surfman polygons that intersect any burned areas
#2 shapefile of surfman polygons that intersect any threatened areas
#3. shapefile of surfman intersected with individual burned areas
#4. shapefile of surfman intersected with individual threatened areas
#5. a csv list of burned jurisdictions by incident id (effectively same as 3, just cleaned)
#6. a csv list of threatened jurisdictions by incident id (excluding those already burned)
#7. a csv list of incidents that should include state/county lands in burned jur total (i.e. burned non-federal land)
#8. a csv list of incidents that should include state/county lands in threatened jur total (i.e. threatened non-federal land)


#load libraries
library(sf)
library(dplyr)
library(foreach) #for parallelizing intersection
library(doParallel)


#read in cleaned surf man dataset
#this shapefile is not the same a written out in 5a, cleaning performed in qgis & explained in "data\\qaqc_cleaning\\
# surfman<-read_sf("data\\JCA\\surfman_clean_0531.shp")
# surfman_buf<-st_buffer(surfman,0)
# write_sf(surfman_buf,"data\\JCA\\surfman_buf0_0531_usethis.shp")
surfman_buf<-st_read("data\\JCA\\surfman_fin.shp")
surfman_buf<-st_transform(surfman_buf,5070)

#reading this in now for use later in summarizing information by incident
mtbs_withincid<-read.csv(jca_samp_in)


#reading in the mtbs w/ 5 mile buffer (no cut outs for this first st_intersects)
buf_nodonut<-st_read(buffer_nodonuts_fortinter_out)
buf_nodonut<-st_transform(buf_nodonut,5070)

#identifies which polygons in surface management intersect any one of the mtbs w/ buf
#adds T/F to indicator column
surfman_buf$indicator <- st_intersects(surfman_buf, buf_nodonut) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
surf_dointersect<-surfman_buf[surfman_buf$indicator==TRUE,]


#for every year we have burned and threatened footprints
#read in burned
burned<-st_read(select_mtbs_out)
burned_proj<-st_transform(burned,5070)

#do the st_intersects again and write out and indicator column
surf_dointersect$burn_inter<-st_intersects(surf_dointersect, burned_proj) %>% lengths > 0
surf_inters_burn<-surf_dointersect[surf_dointersect$burn_inter==TRUE,]

#write out just the surfman polygons that will get intersected with burned areas
#write_sf(surf_inters_burn,"data\\surf_thatintersect_burn.shp")

#read in threatened
threat<-st_read(threat_work_out)
threat_proj<-st_transform(threat,5070)

#do the st_intersects again and write out and indicator column
surf_dointersect$threat_inter<-st_intersects(surf_dointersect, threat_proj) %>% lengths > 0
surf_inters_threat<-surf_dointersect[surf_dointersect$threat_inter==TRUE,]
#write out just the surfman polygons that will get intersected with threatened areas
#write_sf(surf_inters_threat,"data\\surf_thatintersect_thr.shp")

#then for each of those surf_man TRUE polygons, subset and do the actual intersection with those

#first do burned intersection
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

burned_tointer<-unique(burned_proj$Event_ID)

#for every burned area mtbs footprint, intersect with surface management 
#write out combined sf object with all intersections
burn_intersected<-foreach(i=burned_tointer, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp<-burned_proj[burned_proj$Event_ID==i,]
  mang_forburns<-st_intersection(fp,surf_inters_burn)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#write_sf

#toconnectincandmtbs<-read_sf(burn_surfman_inter_out)

whatitdo_burn<-merge(burn_intersected,mtbs_withincid,by.x="Event_ID","mtbs_ids")

write_sf(whatitdo_burn,burn_surfman_inter_out,overwrite=TRUE)

#writes out a shapefile with all mtbs footprints and the surf management polygons they intersect
#write_sf(burn_intersected,burn_surfman_inter_out,overwrite=TRUE)

#now want to join this with a list of mtbsids by INCIDENT_IDs
#this links all jurisdictional data to INCIDENT_ID
#whatitdo_burn<-merge(burn_intersected,mtbs_withincid,by.x="Event_ID","mtbs_ids")

#grab the columns we need for jurisdictional work 
burn_inter_trimmed<-whatitdo_burn[,c("incident_id","Event_ID","START_YEAR","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC","burn_inter")]
st_geometry(burn_inter_trimmed)<-NULL #get rid of the geom column 
#do unique across incident_id, mtbs_id, and jurisdictional information
burned_jurs_unique<-unique(burn_inter_trimmed[,c("incident_id","Event_ID","START_YEAR","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC")])

#write out a list of burned, unique jurisdictions
write.csv(burned_jurs_unique,burn_juris_byincid_out)

#then do threatened intersection

#just for good measure because these shapes were made with additional geometeric operations
#i.e. cutting donut holes
threat_buf<-st_buffer(threat_proj,0)


registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


threat_tointer<-unique(threat_buf$Event_ID)

threat_intersected<-foreach(i=threat_tointer, .combine = rbind, .packages=c('sf')) %dopar%  {
  
  fp_threat<-threat_buf[threat_buf$Event_ID==i,]
  mang_forthreat<-st_intersection(fp_threat,surf_inters_threat)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

whatitdo_threat<-merge(threat_intersected,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

#threat_intersected<-read_sf(threat_surfman_inter_out)
#write out all surfman polygons intersected wtih individual threatened areas
write_sf(whatitdo_threat,threat_surfman_inter_out,overwrite=TRUE)
#threat_intersected<-read_sf(threat_surfman_inter_out)
#threat_intersected<-read_sf(threat_surfman_inter_out)

#now want to join this with a list of mtbsids by INCIDENT_IDs
#this links all jurisdictional data to INCIDENT_IDbove
#whatitdo_threat<-merge(threat_intersected,mtbs_withincid,by.x="Evnt_ID",by.y="mtbs_ids")

#same steps as above with burned data
threat_inter_trimmed<-whatitdo_threat[,c("incident_id","Event_ID","START_YEAR","JrsdcUK","JrsdcUA","JrsdcUN","JrsdcUI","LndwnrK","LndwnrC","burn_inter","threat_inter")]
threat_inter_trimmed$geometry<-NULL
threatened_jurs_unique<-unique(threat_inter_trimmed)

##EXTRA step required for threatened data becauase we want mutually exclusive lists
#only include in threatened list jurisdictions that were not already burned
only_threat<-threatened_jurs_unique[threatened_jurs_unique$burn_inter==FALSE,]
write.csv(only_threat,threat_juris_byincid_out)

# #removed this stuff because it doesn't capture what i need for sates/counties
# # #now, make list of incidents that have NON federal lands burned 
# # #not searching for the absence of federal lands, but rather, the presence of non-fed
# jurs_burned_ontononfed<-burned_jurs_unique[is.na(burned_jurs_unique$JrsdcUK)| burned_jurs_unique$JrsdcUK=="Other"| burned_jurs_unique$JrsdcUK=="Private",]
# 
# # #get indicident ids that have NA, other, or private lands burned
# burn_incid_count_stcnty<-unique(jurs_burned_ontononfed$incident_id)
# 
# #do it for threatened
# jurs_threat_ontononfed<-threatened_jurs_unique[is.na(threatened_jurs_unique$JrsdcUK)| threatened_jurs_unique$JrsdcUK=="Other"| threatened_jurs_unique$JrsdcUK=="Private",]
# 
# #get indicident ids that have NA, other, or private lands burned
# threat_incid_count_stcnty<-unique(jurs_threat_ontononfed$incident_id)
# 
# ###don't actually think i use/need these....
# write.csv(burn_incid_count_stcnty,burn_incid_count_stcnty_out)
# write.csv(threat_incid_count_stcnty,threat_incid_count_stcnty_out)
