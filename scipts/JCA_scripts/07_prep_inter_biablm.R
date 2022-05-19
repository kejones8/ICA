###this script reads in blm & bia shapefile data 
library(sf)

bia<-read_sf("data\\BIA_Regions.shp\\BIA_Regions.shp")
bia_proj<-st_transform(bia,5070)
bia_buf<-st_buffer(bia_proj,0)

st_layers("data\\BLM_National_Administrative_Unit_B.gdb\\BLM_National_Administrative_Unit_B.gdb")
blm<-read_sf(layer="blm_natl_admu_state_poly_webpub",dsn="data\\BLM_National_Administrative_Unit_B.gdb\\BLM_National_Administrative_Unit_B.gdb")
blm_proj<-st_transform(blm,5070)
blm_buf<-st_buffer(blm_proj,0)


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
  st_forburns<-st_intersection(fp,bia_buf)#5 miles = 8047 meters
  
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm
