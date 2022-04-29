#prepping census place data to get only Functional State Areas = A


library(sf)
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
