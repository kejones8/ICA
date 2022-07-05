#prepping census place data to get only Functional State Areas = A


library(sf) #working w/ shapefiles
library(foreach) #for parallelizing intersection
library(doParallel)
library(dplyr)


#read in state file, extract func_stat column & #, append to long list to merge to national file, then close shapefile
folder_path<-"raw_data\\census_place\\states"


file_list<-list.files(folder_path,full.names=TRUE,pattern="*.zip")

geoids_funcstat_a<-c()

for (i in 1:length(file_list)){
  yup<-unzip(zipfile=file_list[i], exdir = tempdir(),setTimes = FALSE)
  shp_toread<-list.files(tempdir(),recursive=TRUE,full.names=TRUE,pattern=".shp")[1]
  state_shp<-read_sf(shp_toread) #read that .shp file
  
  unlink(tempdir())
  file.remove(yup)
  
  a_funcstat<-state_shp[state_shp$FUNCSTAT=="A",]
  geoids<-as.vector(a_funcstat$GEOID)
  geoids_funcstat_a<-append(geoids_funcstat_a,geoids)
}


nat_cnpl<-read_sf("raw_data\\census_place\\cb_2021_us_place_500k\\cb_2021_us_place_500k.shp")

cenpl_funstata<-nat_cnpl[nat_cnpl$GEOID %in% geoids_funcstat_a,]

write_sf(cenpl_funstata,"raw_data\\census_place\\national_funcstata.shp",overwrite=TRUE)
cenpl_funstata<-read_sf("raw_data\\census_place\\national_funcstata.shp")

###now, want to intersect cenpl data with mtbs footprints 

#read in mtbs footprints & threatened buffers for jca subsample
jca_mtbs<-read_sf(select_mtbs_in)

jca_alb<-st_transform(jca_mtbs, 5070)
jca_buf<-st_buffer(jca_alb,0)

mtbs_threat<-read_sf(threat_work_out)

threat_alb<-st_transform(mtbs_threat, 5070)
threat_buf<-st_buffer(threat_alb,0)



cenpl_alb<-st_transform(cenpl_funstata, 5070)
cenpl_buf<-st_buffer(cenpl_alb,0)

#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

mtbs_list<-unique(jca_buf$Event_ID)

cenpl_burn<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp <- jca_buf[jca_buf$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter<-sf::st_intersection(fp,cenpl_buf)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

st_geometry(cenpl_burn)<-NULL

#now, need to intersect threatened & also remove cenpl that occur in burned 
#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

mtbs_threat<-unique(threat_buf$Event_ID)

cenpl_threat<-foreach(i=mtbs_threat, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp_threat <- threat_buf[threat_buf$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter_threat<-sf::st_intersection(fp_threat,cenpl_buf)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

st_geometry(cenpl_threat)<-NULL


#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv(jca_samp_in)


#count burned cenpl
cenpl_burn_incid<-merge(cenpl_burn,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
count_cenpl_burn<-cenpl_burn_incid %>% group_by(incident_id) %>% dplyr::summarize(cnt_cenpl_burn=n_distinct(GEOID))

cenpl_threat_incid<-merge(cenpl_threat,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

#remove from threatened censusplaces that intersect burns
#now that the above operations have been run, we want to remove any ids from the threatened that already appear in burned
cenpl_threat_incid$torm_inburn <- ifelse(is.na(match(paste0(cenpl_threat_incid$incident_id,cenpl_threat_incid$GEOID), 
                                                    paste0(cenpl_burn_incid$incident_id,cenpl_burn_incid$GEOID))),FALSE, TRUE)


#now, remove juris from threatened cenpl that already appear in burned
#keep records where torm_inburn==FALSE
threat_cenpl_rmburn<-cenpl_threat_incid[cenpl_threat_incid$torm_inburn==FALSE,]

threat_cenpl_unique<-unique(threat_cenpl_rmburn[,c("incident_id","GEOID")])


count_cenpl_threat<-threat_cenpl_unique %>% group_by(incident_id) %>% dplyr::summarize(cnt_cenpl_threat=n_distinct(GEOID))


write.csv(count_cenpl_burn,count_cenpl_burn_out)
write.csv(count_cenpl_threat,count_cenpl_threat_out)
