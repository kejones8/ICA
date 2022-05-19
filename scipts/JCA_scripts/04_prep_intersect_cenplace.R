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

#read in mtbs footprints for jca subsample
jca_mtbs<-read_sf("data\\JCA\\mtbs_match_jcasamp.shp")

jca_alb<-st_transform(jca_mtbs, 5070)
jca_buf<-st_buffer(jca_alb,0)
cenpl_alb<-st_transform(cenpl_funstata, 5070)
cenpl_buf<-st_buffer(cenpl_alb,0)

#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

mtbs_list<-unique(jca_buf$Event_ID)

didthiswork<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {
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

st_geometry(didthiswork)<-NULL

#mtbs_withcenpl<-didthiswork[,c("Event_ID","GEOID")]

mtbs_count_cenpl<-didthiswork %>% group_by(Event_ID) %>% summarize(cnt_cenpl=n_distinct(GEOID))


write.csv(mtbs_count_cenpl,"data\\JCA\\count_cenpl.csv")
