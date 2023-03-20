#prepping census place data to get only Functional State Areas = A


library(sf) #working w/ shapefiles
library(foreach) #for parallelizing intersection
library(doParallel)
library(dplyr)

`%notin%` <- Negate(`%in%`)


#read in funcstat a census places
file_list_years<-list.files("raw_data\\census_place\\yearly",full.names=TRUE,pattern="*.shp")

cp_2000<-read_sf(file_list_years[1]) #year2000
cp_2007<-read_sf(file_list_years[2]) #year2007

geoid2007<-cp_2007$GEOID

#FIRST, handle that 2000 needs to use geoids from 2007 to limit it

cp_2000_funca<-cp_2000[cp_2000$GEOID %in% geoid2007,]

cenpl_2000_proj<-st_transform(cp_2000_funca,5070)
cenp_2000_buf<-st_buffer(cenpl_2000_proj,0)

###now, want to intersect cenpl data with mtbs footprints 

#read in mtbs footprints & threatened buffers for jca subsample
jca_mtbs<-read_sf(select_mtbs_in)

jca_alb<-st_transform(jca_mtbs, 5070)
jca_buf_orig<-st_buffer(jca_alb,0)

#for burned
jca_buf_orig$date<-as.Date(jca_buf_orig$Ig_Date)
jca_buf_orig$year<-format(jca_buf_orig$date, format="%Y")#jca_buf$(date

mtbs_threat<-read_sf(threat_work_out)

threat_alb<-st_transform(mtbs_threat, 5070)
threat_buf_orig<-st_buffer(threat_alb,0)

#for threatened
threat_buf_orig$date<-as.Date(threat_buf_orig$Ig_Date)
threat_buf_orig$year<-format(threat_buf_orig$date, format="%Y")#jca_buf$(date


#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv(jca_samp_in)


#year<-seq(2000,2020,1)
year<-seq(1999,2020,1)

file_list_years_no2000<-file_list_years[-1]#for teh else statement

for (i in 1:length(year)){
  
  
  
  if (i<9){ #for years 2000-2006, need to run with 2000 census data
    
 
    jca_buf<-st_make_valid(jca_buf_orig[jca_buf_orig$year==year[i],])
    
    jca_buf$indicator <- st_intersects(jca_buf, cenp_2000_buf) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    jca_buf_dointersect<-jca_buf[jca_buf$indicator==TRUE,]
    
    mtbs_list<-unique(jca_buf_dointersect$Event_ID)
    
    #make list of cenplaces
    cenp_2000_buf$indicator <- st_intersects(cenp_2000_buf,jca_buf_dointersect) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    cenpl_buf2000_dointersect<-cenp_2000_buf[cenp_2000_buf$indicator==TRUE,]
    
    
    threat_buf<-st_make_valid(threat_buf_orig[threat_buf_orig$year==year[i],])
    print('made valid threat polys')
    #threat_buf_2010<-st_make_valid(threat_buf[threat_buf$year>=2010,])
    
    threat_buf$indicator <- st_intersects(threat_buf, cenp_2000_buf) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    threat_buf_dointersect<-threat_buf[threat_buf$indicator==TRUE,]
    
    #make a list of mtbs events
    threat_list<-unique(threat_buf_dointersect$Event_ID)
    
    #make list of cenplaces
    cenp_2000_buf$indicator <- st_intersects(cenp_2000_buf,threat_buf_dointersect) %>% lengths > 0
    
    
    #make surfman smaller by just grabbing those we know we'll want to work with 
    cenpl_buf_threat_dointersect<-cenp_2000_buf[cenp_2000_buf$indicator==TRUE,]
    
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    
    #mtbs_list<-unique(jca_buf$Event_ID)
    
    cenpl_burn<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {
      #myvars = c("LONG_X","LAT_Y")
      #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
      fp <- jca_buf[jca_buf$Event_ID==i,]
      # veg_buf <- st_buffer(veg_type,0)
      # veg_val<-st_make_valid(veg_buf)
      inter<-sf::st_intersection(fp,cenpl_buf2000_dointersect)
      
      #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
      #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      #clip1 = point.in.poly(spdf, all_polygons)
      #getValues(clip1)
    }
    print(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm
    
    head(cenpl_burn)
    
    #st_geometry(cenpl_burn)<-NULL
    
    
    
    #now, need to intersect threatened & also remove cenpl that occur in burned 
    #now intersect jca_mtbs footprints with census places of func stat a
    registerDoParallel(makeCluster(12))
    ptm <- proc.time()
    print(Sys.time())
    
    
    cenpl_threat<-foreach(i=threat_list, .combine = rbind, .packages='sf')  %dopar%  {
      #myvars = c("LONG_X","LAT_Y")
      #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
      fp_threat <- threat_buf[threat_buf$Event_ID==i,]
      # veg_buf <- st_buffer(veg_type,0)
      # veg_val<-st_make_valid(veg_buf)
      inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_threat_dointersect)
      
      #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
      #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      #clip1 = point.in.poly(spdf, all_polygons)
      #getValues(clip1)
    }
    print(Sys.time())
    stopImplicitCluster()
    proc.time() - ptm
    
    head(cenpl_threat)
    
    #just testing
    #write_sf(cenpl_threat,"K8_Chpt\\data\\scratch\\cenpl_2000_threat_inter.shp")
    
    
    cenpl_burn<-cenpl_burn[,c("Event_ID","GEOID",'year')]
    cenpl_burn$b_t<-"burn"
    cenpl_threat<-cenpl_threat[,c("Event_ID","GEOID","year")]
    cenpl_threat$b_t<-"threat"
    
    cenpl_counts<-rbind(cenpl_burn,cenpl_threat)
    
    print("rbind burn & threat for this year")
    
    st_geometry(cenpl_burn)<-NULL
    st_geometry(cenpl_threat)<-NULL
    
    merge_cpburn<-merge(cenp_2000_buf,cenpl_burn,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpburn,paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyr",year[i],".shp"))
    
    merge_cpthreat<-merge(cenp_2000_buf,cenpl_threat,by.x="GEOID",by.y="GEOID")
    write_sf(merge_cpthreat,paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_engag_byyr",year[i],".shp"))
    
    print(paste0("finished ",year[i]))
    
    
    
  } else{

cenpl_yr<-read_sf(file_list_years_no2000[i-8])
cenpl_proj<-st_transform(cenpl_yr,5070)
cenpl_buf<-st_buffer(cenpl_proj,0)
print("buffered cenplaces for this year")




jca_buf<-st_make_valid(jca_buf_orig[jca_buf_orig$year==year[i],])

jca_buf$indicator <- st_intersects(jca_buf, cenpl_buf) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
jca_buf_dointersect<-jca_buf[jca_buf$indicator==TRUE,]

mtbs_list<-unique(jca_buf_dointersect$Event_ID)

#make list of cenplaces
cenpl_buf$indicator <- st_intersects(cenpl_buf,jca_buf_dointersect) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
cenpl_buf_dointersect<-cenpl_buf[cenpl_buf$indicator==TRUE,]


#mtbs_list_2010<-unique(jca_buf_2010$Event_ID)



threat_buf<-st_make_valid(threat_buf_orig[threat_buf_orig$year==year[i],])
print('made valid threat polys')
#threat_buf_2010<-st_make_valid(threat_buf[threat_buf$year>=2010,])

threat_buf$indicator <- st_intersects(threat_buf, cenpl_buf) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
threat_buf_dointersect<-threat_buf[threat_buf$indicator==TRUE,]

#make a list of mtbs events
threat_list<-unique(threat_buf_dointersect$Event_ID)

#make list of cenplaces
cenpl_buf$indicator <- st_intersects(cenpl_buf,threat_buf_dointersect) %>% lengths > 0


#make surfman smaller by just grabbing those we know we'll want to work with 
cenpl_buf_threat_dointersect<-cenpl_buf[cenpl_buf$indicator==TRUE,]

#threat_list_2010<-unique(threat_buf_2010$Event_ID)

#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


cenpl_burn<-foreach(i=mtbs_list, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp <- jca_buf[jca_buf$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter<-sf::st_intersection(fp,cenpl_buf_dointersect)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

head(cenpl_burn)

#st_geometry(cenpl_burn)<-NULL

#now, need to intersect threatened & also remove cenpl that occur in burned 
#now intersect jca_mtbs footprints with census places of func stat a
registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())


cenpl_threat<-foreach(i=threat_list, .combine = rbind, .packages='sf')  %dopar%  {
  #myvars = c("LONG_X","LAT_Y")
  #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
  fp_threat <- threat_buf[threat_buf$Event_ID==i,]
  # veg_buf <- st_buffer(veg_type,0)
  # veg_val<-st_make_valid(veg_buf)
  inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_threat_dointersect)
  
  #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
  #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #clip1 = point.in.poly(spdf, all_polygons)
  #getValues(clip1)
}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

head(cenpl_threat)

#just testing
#write_sf(cenpl_threat,"K8_Chpt\\data\\scratch\\cenpl_2000_threat_inter.shp")


cenpl_burn<-cenpl_burn[,c("Event_ID","GEOID",'year')]
cenpl_burn$b_t<-"burn"
cenpl_threat<-cenpl_threat[,c("Event_ID","GEOID","year")]
cenpl_threat$b_t<-"threat"

cenpl_counts<-rbind(cenpl_burn,cenpl_threat)

print("rbinded burn & threat for this year")

st_geometry(cenpl_burn)<-NULL
st_geometry(cenpl_threat)<-NULL

merge_cpburn<-merge(cenpl_buf,cenpl_burn,by.x="GEOID",by.y="GEOID")
write_sf(merge_cpburn,paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyr",year[i],".shp"))


merge_cpthreat<-merge(cenpl_buf,cenpl_threat,by.x="GEOID",by.y="GEOID")
write_sf(merge_cpthreat,paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_engag_byyr",year[i],".shp"))

print(paste0("finished ",year[i]))
#from these objects that are written out, could change how it runs
#or just read htem back in, but essentially need to tally appearance of each geoid?
#check code below for how tallies are made for final map!

###and make sure I have the right output for the final 
  }
 }


# 
# #now intersect jca_mtbs footprints with census places of func stat a
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# #mtbs_list<-unique(jca_buf$Event_ID)
# 
# cenpl_burn_10<-foreach(i=mtbs_list_2010, .combine = rbind, .packages='sf')  %dopar%  {
#   #myvars = c("LONG_X","LAT_Y")
#   #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
#   fp <- jca_buf_2010[jca_buf_2010$Event_ID==i,]
#   # veg_buf <- st_buffer(veg_type,0)
#   # veg_val<-st_make_valid(veg_buf)
#   inter<-sf::st_intersection(fp,cenpl_buf_2010)
#   
#   #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
#   #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#   
#   #clip1 = point.in.poly(spdf, all_polygons)
#   #getValues(clip1)
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm
# 
# st_geometry(cenpl_burn_10)<-NULL
# 
# 
# 
# 
# 
# 
# #now, need to intersect threatened & also remove cenpl that occur in burned 
# #now intersect jca_mtbs footprints with census places of func stat a
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# 
# cenpl_threat_10<-foreach(i=threat_list_2010, .combine = rbind, .packages='sf')  %dopar%  {
#   #myvars = c("LONG_X","LAT_Y")
#   #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
#   fp_threat <- threat_buf_2010[threat_buf_2010$Event_ID==i,]
#   # veg_buf <- st_buffer(veg_type,0)
#   # veg_val<-st_make_valid(veg_buf)
#   inter_threat<-sf::st_intersection(fp_threat,cenpl_buf_2010)
#   
#   #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
#   #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#   
#   #clip1 = point.in.poly(spdf, all_polygons)
#   #getValues(clip1)
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm
# 
# #just testing
# #write_sf(cenpl_threat,"K8_Chpt\\data\\scratch\\cenpl_2000_threat_inter.shp")
# st_geometry(cenpl_threat_10)<-NULL


#need to take cenpl_threat_10 + cenpl_burn_10 & cenpl_burn + cenpl_threat, get same columsn for procesing below

# cenpl_burn_00<-cenpl_burn[,c("Event_ID","GEOID_00")]
# cenpl_burn_00$b_t<-"burn"
# cenpl_threat_00<-cenpl_threat[,c("Event_ID","GEOID_00")]
# cenpl_threat_00$b_t<-"threat"
# 
# cenpl_counts_00<-rbind(cenpl_burn_00,cenpl_threat_00)
# 
# cenpl_burn_10_trim<-cenpl_burn_10[,c("Event_ID","GEOID10")]
# cenpl_burn_10_trim$b_t<-"burn"
# cenpl_threat_10_trim<-cenpl_threat_10[,c("Event_ID","GEOID10")]
# cenpl_threat_10_trim$b_t<-"threat"
# 
# cenpl_counts_10<-rbind(cenpl_burn_10_trim,cenpl_threat_10_trim)
# 
# colnames(cenpl_counts_00)<-c("Event_ID","GEOID","b_t")
# colnames(cenpl_counts_10)<-c("Event_ID","GEOID","b_t")

# #for creating maps of cenplaces burned by year...
# cp_00<-cenpl_burn[,c("Event_ID","GEOID_00","year")]
# cp_10<-cenpl_burn_10[,c("Event_ID","GEOID10","year")]
# colnames(cp_00)<-c("Event_ID","GEOID","year")
# colnames(cp_10)<-c("Event_ID","GEOID","year")
# cenpl_burn_00_10<-rbind(cp_00,cp_10)


# merge_cp00<-merge(cenpl_funcstata_2000_5070,cp_00,by.x="GEOID_00",by.y="GEOID")
# write_sf(merge_cp00,"K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyear00.shp")
# merge_cp10<-merge(cenpl_funcstata_2010_5070,cp_10,by.x="GEOID10",by.y="GEOID")
# write_sf(merge_cp10,"K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyear10.shp")
# ####above for maps

#cenpl_counts_burn_threat<-rbind(cenpl_counts_00,cenpl_counts_10)
#reading this in to link list of incidents with mtbs_ids
mtbs_withincid<-read.csv(jca_samp_in)

cp_2020<-read_sf("raw_data\\census_place\\yearly\\funca_2020.shp")

#counting numbers of times wildfires engage census places
all_year_geoid_tabs<-data.frame()

#counting number of census places burned/ engaged in a given year (but first by incident)
count_cenpl_burn_all<-data.frame()
count_cenpl_threat_all<-data.frame()

for (i in 1:length(year)){
  
  file_to_burn<-paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_burn_byyr",year[i],".shp")
  file_to_threat<-paste0("K8_Chpt\\data_figures\\census_place_related_figs\\cenpl_engag_byyr",year[i],".shp")
  
  cenpl_counts_burn<-read_sf(file_to_burn)
  cenpl_counts_threat<-read_sf(file_to_threat)
  
  #count burned cenpl
  cenpl_burn_incid<-merge(cenpl_counts_burn,mtbs_withincid,by.x="Event_ID",by.y="mtbs_ids")
  
  count_cenpl_burn<-cenpl_burn_incid %>% dplyr::group_by(incident_id)%>% dplyr::summarize(cnt_cenpl_burn=n_distinct(GEOID))
  st_geometry(count_cenpl_burn)<-NULL
  
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
  st_geometry(count_cenpl_threat)<-NULL
  
  #create count list for each geoid
  cenpl_threat_incid$torm_inburn<-NULL
  
  #rbind these BECAUSE we want both burned and "threat" to reflect engaged
  countthis<-rbind(cenpl_burn_incid,cenpl_threat_incid)
  geoid_tab<-countthis %>% group_by(GEOID,incident_id) %>% dplyr::summarize(Count = n())
  countsofgeoids<-as.data.frame(table(geoid_tab$GEOID))
  colnames(countsofgeoids)<-c("GEOID","num_times_eng") 
  countsofgeoids$year<-rep(year[i],nrow(countsofgeoids))
  all_year_geoid_tabs<-rbind(all_year_geoid_tabs,countsofgeoids)
  
  count_cenpl_burn_all<-rbind(count_cenpl_burn_all,count_cenpl_burn)
  count_cenpl_threat_all<-rbind(count_cenpl_threat_all,count_cenpl_threat)
  
}


#see how many geoids aren't in 2020!!!

checkforthese<-all_year_geoid_tabs$GEOID

length(unique(checkforthese[checkforthese %notin% cp_2020$GEOID]))

####summarize all years geoids counts by geoid
range_of_this<-all_year_geoid_tabs %>% group_by(GEOID) %>% summarize(count_engaged = sum(num_times_eng))

#creates probability of engagement census place map
eh<-merge(cp_2020,range_of_this,by="GEOID",all.y=TRUE) #don't seem to  lose any with eh

#write_sf(eh,"K8_Chpt\\data_figures\\census_place_related_figs\\cp_bynumtimes_eng.shp")

#till need to get it into mapped form
#merge with 

write.csv(count_cenpl_burn_all,count_cenpl_burn_out)
write.csv(count_cenpl_threat_all,count_cenpl_threat_out)

# write.csv(count_cenpl_burn,count_cenpl_burn_out)
# write.csv(count_cenpl_threat,count_cenpl_threat_out)

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
