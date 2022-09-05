#prepping census place data to get only Functional State Areas = A


library(sf) #working w/ shapefiles
library(foreach) #for parallelizing intersection
library(doParallel)
library(dplyr)


#read in state file, extract func_stat column & #, append to long list to merge to national file, then close shapefile
folder_path_2010<-"raw_data\\census_place\\states\\2010"


file_list<-list.files(folder_path_2010,full.names=TRUE,pattern="*.zip")

geoids_funcstat_a<-c()

for (i in 1:length(file_list)){
  yup<-unzip(zipfile=file_list[i], exdir = tempdir(),setTimes = FALSE)
  shp_toread<-list.files(tempdir(),recursive=TRUE,full.names=TRUE,pattern=".shp")[1]
  state_shp<-read_sf(shp_toread) #read that .shp file
  
  unlink(tempdir())
  file.remove(yup)
  
  a_funcstat<-state_shp[state_shp$FUNCSTAT10=="A",]
  geoids<-as.vector(a_funcstat$GEOID10) #geoid equivalent...sort of, have to use PLACEFP because 2000 doesn't have GEOID
  geoids_funcstat_a<-append(geoids_funcstat_a,geoids)
  
}


#use the list of geoids_functstat_a to unzip 2000 & grab only those geoids, then append them together
folder_path_2000<-"raw_data\\census_place\\states\\2000"


file_list<-list.files(folder_path_2000,full.names=TRUE,pattern="*.zip")

funca_2000<-st_sf(st_sfc())

for (i in 1:length(file_list)){
  yup<-unzip(zipfile=file_list[i], exdir = tempdir(),setTimes = FALSE)
  shp_toread<-list.files(tempdir(),recursive=TRUE,full.names=TRUE,pattern=".shp")[1]
  state_shp<-read_sf(shp_toread) #read that .shp file
  state_shp$GEOID_00<-paste0(state_shp$STATE,state_shp$PLACEFP)
  
  unlink(tempdir())
  file.remove(yup)
  
  #a_funcstat<-state_shp[state_shp$FUNCSTAT=="A",]
  #geoids<-as.vector(a_funcstat$GEOID)
  selected_a<-state_shp[state_shp$GEOID_00 %in% geoids_funcstat_a,c("GEOID_00","STATE","PLACEFP","geometry")]
  
  #this will need to be a spatial object
  funca_2000<-rbind(funca_2000,selected_a)
  
}

nat_cnpl_2010<-read_sf("raw_data\\census_place\\national\\2010\\Place_2010Census_Dp1.shp")

cenpl_funstata_2010<-nat_cnpl_2010[nat_cnpl_2010$GEOID10 %in% geoids_funcstat_a,]

cenpl_funcstata_2010_5070<-st_transform(cenpl_funstata_2010,5070)

write_sf(cenpl_funcstata_2010_5070,"raw_data\\census_place\\national_funcstata_2010.shp")

st_crs(funca_2000)<-st_crs(cenpl_funstata_2010)
cenpl_funcstata_2000_5070<-st_transform(funca_2000,5070)
write_sf(cenpl_funcstata_2000_5070,"raw_data\\census_place\\national_funcstata_2000.shp")


#reading in without rerunning above
cenpl_funcstata_2000_5070<-read_sf("raw_data\\census_place\\national_funcstata_2000.shp")
cenpl_funcstata_2010_5070<-read_sf("raw_data\\census_place\\national_funcstata_2010.shp")




#write_sf(cenpl_funstata,"raw_data\\census_place\\national_funcstata.shp",overwrite=TRUE)
#cenpl_funstata<-read_sf("raw_data\\census_place\\national_funcstata.shp")

###now, want to intersect cenpl data with mtbs footprints 

#read in mtbs footprints & threatened buffers for jca subsample
jca_mtbs<-read_sf(select_mtbs_in)

jca_alb<-st_transform(jca_mtbs, 5070)
jca_buf<-st_buffer(jca_alb,0)

mtbs_threat<-read_sf(threat_work_out)

threat_alb<-st_transform(mtbs_threat, 5070)
threat_buf<-st_buffer(threat_alb,0)


cenpl_buf_2000<-st_buffer(cenpl_funcstata_2000_5070,0)


cenpl_buf_2010<-st_buffer(cenpl_funcstata_2010_5070,0)

#for burned
jca_buf$date<-as.Date(jca_buf$Ig_Date)
jca_buf$year<-format(jca_buf$date, format="%Y")#jca_buf$(date

jca_buf_2000<-jca_buf[jca_buf$year<2010,]
jca_buf_2010<-jca_buf[jca_buf$year>=2010,]

mtbs_list_2000<-unique(jca_buf_2000$Event_ID)

mtbs_list_2010<-unique(jca_buf_2010$Event_ID)

#for threatened
threat_buf$date<-as.Date(threat_buf$Ig_Date)
threat_buf$year<-format(threat_buf$date, format="%Y")#jca_buf$(date

threat_buf_2000<-threat_buf[threat_buf$year<2010,]
threat_buf_2010<-threat_buf[threat_buf$year>=2010,]

threat_list_2000<-unique(threat_buf_2000$Event_ID)

threat_list_2010<-unique(threat_buf_2010$Event_ID)

#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#mtbs_list<-unique(jca_buf$Event_ID)

cenpl_burn<-foreach(i=mtbs_list_2000, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp <- jca_buf_2000[jca_buf_2000$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter<-sf::st_intersection(fp,cenpl_buf_2000)
  
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


cenpl_threat<-foreach(i=threat_list_2000, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp_threat <- threat_buf_2000[threat_buf_2000$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_2000)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#just testing
#write_sf(cenpl_threat,"K8_Chpt\\data\\scratch\\cenpl_2000_threat_inter.shp")
st_geometry(cenpl_threat)<-NULL




#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

#mtbs_list<-unique(jca_buf$Event_ID)

cenpl_burn_10<-foreach(i=mtbs_list_2010, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp <- jca_buf_2010[jca_buf_2010$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter<-sf::st_intersection(fp,cenpl_buf_2010)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

st_geometry(cenpl_burn_10)<-NULL






#now, need to intersect threatened & also remove cenpl that occur in burned 
#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


cenpl_threat_10<-foreach(i=threat_list_2010, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp_threat <- threat_buf_2010[threat_buf_2010$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_2010)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

#just testing
#write_sf(cenpl_threat,"K8_Chpt\\data\\scratch\\cenpl_2000_threat_inter.shp")
st_geometry(cenpl_threat_10)<-NULL


#need to take cenpl_threat_10 + cenpl_burn_10 & cenpl_burn + cenpl_threat, get same columsn for procesing below

cenpl_burn_00<-cenpl_burn[,c("Event_ID","GEOID_00")]
cenpl_burn_00$b_t<-"burn"
cenpl_threat_00<-cenpl_threat[,c("Event_ID","GEOID_00")]
cenpl_threat_00$b_t<-"threat"

cenpl_counts_00<-rbind(cenpl_burn_00,cenpl_threat_00)

cenpl_burn_10_trim<-cenpl_burn_10[,c("Event_ID","GEOID10")]
cenpl_burn_10_trim$b_t<-"burn"
cenpl_threat_10_trim<-cenpl_threat_10[,c("Event_ID","GEOID10")]
cenpl_threat_10_trim$b_t<-"threat"

cenpl_counts_10<-rbind(cenpl_burn_10_trim,cenpl_threat_10_trim)

colnames(cenpl_counts_00)<-c("Event_ID","GEOID","b_t")
colnames(cenpl_counts_10)<-c("Event_ID","GEOID","b_t")

#for creating maps of cenplaces burned by year...
cp_00<-cenpl_burn[,c("Event_ID","GEOID_00","year")]
cp_10<-cenpl_burn_10[,c("Event_ID","GEOID10","year")]
colnames(cp_00)<-c("Event_ID","GEOID","year")
colnames(cp_10)<-c("Event_ID","GEOID","year")
cenpl_burn_00_10<-rbind(cp_00,cp_10)


merge_cp00<-merge(cenpl_funcstata_2000_5070,cp_00,by.x="GEOID_00",by.y="GEOID")
write_sf(merge_cp00,"K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyear00.shp")
merge_cp10<-merge(cenpl_funcstata_2010_5070,cp_10,by.x="GEOID10",by.y="GEOID")
write_sf(merge_cp10,"K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyear10.shp")
####above for maps

cenpl_counts_burn_threat<-rbind(cenpl_counts_00,cenpl_counts_10)

cenpl_counts_burn<-cenpl_counts_burn_threat[cenpl_counts_burn_threat$b_t=="burn",]
cenpl_counts_threat<-cenpl_counts_burn_threat[cenpl_counts_burn_threat$b_t=="threat",]



#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv(jca_samp_in)


#count burned cenpl
cenpl_burn_incid<-merge(cenpl_counts_burn,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

count_cenpl_burn<-cenpl_burn_incid %>% dplyr::group_by(incident_id)%>% dplyr::summarize(cnt_cenpl_burn=n_distinct(GEOID))

cenpl_threat_incid<-merge(cenpl_counts_threat,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")

#remove from threatened censusplaces that intersect burns
#now that the above operations have been run, we want to remove any ids from the threatened that already appear in burned
cenpl_threat_incid$torm_inburn <- ifelse(is.na(match(paste0(cenpl_threat_incid$incident_id,cenpl_threat_incid$GEOID), 
                                                    paste0(cenpl_burn_incid$incident_id,cenpl_burn_incid$GEOID))),FALSE, TRUE)


#now, remove juris from threatened cenpl that already appear in burned
#keep records where torm_inburn==FALSE
threat_cenpl_rmburn<-cenpl_threat_incid[cenpl_threat_incid$torm_inburn==FALSE,]

threat_cenpl_unique<-unique(threat_cenpl_rmburn[,c("incident_id","GEOID")])


count_cenpl_threat<-threat_cenpl_unique %>% group_by(incident_id) %>% dplyr::summarize(cnt_cenpl_threat=n_distinct(GEOID))

#create count list for each geoid
cenpl_threat_incid$torm_inburn<-NULL
countthis<-rbind(cenpl_burn_incid,cenpl_threat_incid)
geoid_tab<-countthis %>% group_by(GEOID,incident_id) %>% dplyr::summarize(Count = n())

countsofgeoids<-as.data.frame(table(geoid_tab$GEOID))
colnames(countsofgeoids)<-c("GEOID10","num_times_eng") #use 10 becuase im being lazy and only joining it to that..
eh<-merge(cenpl_funcstata_2010_5070,countsofgeoids,by="GEOID10",all.y=TRUE) #don't seem to  lose any with eh
write_sf(eh,"K8_Chpt\\data_figures\\census_place_related_figs\\cp_bynumtimes_eng.shp")

#till need to get it into mapped form
#merge with 

write.csv(count_cenpl_burn,count_cenpl_burn_out)
write.csv(count_cenpl_threat,count_cenpl_threat_out)

# cp00_incid<-merge(merge_cp00,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids",all.x=TRUE)
# cp10_incid<-merge(merge_cp10,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids",all.x=TRUE)
# 
# cp00_incid %>% group_by(GEOID_00,incident_id) %>% dplyr::summarize(count_eng)

# #for making census place burned counts by state
# table(merge_cp00$STATE)
# #need to get substring of GEOID for merge 2010..because state isn't single column
# test<-substr(merge_cp10$GEOID10,1,2)
# 
# count_this<-c(test,merge_cp00$STATE)
# to_join<-as.data.frame(table(count_this))
# 
# write.csv(to_join,"K8_Chpt\\data_figures\\census_place_related_figs\\table_state_cnt.csv")
# 
# #go ahead and join and write out
# st<-read_sf("raw_data\\tl_2017_us_state\\tl_2017_us_state.shp")
# 
# st_cnt<-merge(st, to_join,by.x="STATEFP",by.y="count_this",all.x=TRUE)
# 
# write_sf(st_cnt,"K8_Chpt\\data_figures\\census_place_related_figs\\state_cnt_cp.shp")
# #want to test number vs. repeat burns...even thought many census places burned, how many repeats?
