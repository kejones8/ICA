#this will read through and get the correct functional types, merge, etc.

library(sf)
library(stringr)
`%notin%` <- Negate(`%in%`)

dir<-"raw_data\\census_place\\sept_censusplace"

year_dirs<-list.dirs(path = dir, full.names = TRUE, recursive = TRUE)[-1] #get rid of the first one because its a parent directory

year<-c(2000,2007,2008,2009,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

#####for each year, i need to get the unique geoid list
geoid_2007<-c()
geoid_2008<-c()
geoid_2009<-c()
#geoid_2010<-c()
geoid_2011<-c()
geoid_2012<-c()
geoid_2013<-c()
geoid_2014<-c()
geoid_2015<-c()
geoid_2016<-c()
geoid_2017<-c()
geoid_2018<-c()
geoid_2019<-c()
geoid_2020<-c()

geoid_master<-list(geoid_2007,geoid_2008, geoid_2009, geoid_2011, geoid_2012, geoid_2013, geoid_2014, geoid_2015, geoid_2016, geoid_2017, geoid_2018, geoid_2019, geoid_2020)
names(geoid_master)<-year


for (x in 1:length(year_dirs)){


file_list<-list.files(year_dirs[x],full.names=TRUE,pattern="*.zip")

yearly_funca_allstates<-st_sf(st_sfc())
st_crs(yearly_funca_allstates)<-5070
print("created master sf obj")

for (i in 1:length(file_list)){
  
  yup<-unzip(zipfile=file_list[i], exdir = tempdir(),setTimes = FALSE)
  shp_toread<-list.files(tempdir(),recursive=TRUE,full.names=TRUE,pattern=".shp")[1]
  state_shp_noproj<-read_sf(shp_toread) #read that .shp file
  st_crs(state_shp_noproj)<-4269
  print(st_crs(state_shp_noproj))
  state_shp<-st_transform(state_shp_noproj,5070)
  print("projected stateshp")
  
  unlink(tempdir())
  file.remove(yup)
  print (year[x])
  

  
  if (as.integer(year[x])<2005){
    a_funcstat<-state_shp[,c("STATE","PLACEFP","geometry")]
    a_funcstat$GEOID<-paste0(as.character(a_funcstat$STATE),as.character(a_funcstat$PLACEFP))
    a_funcstat_states<-a_funcstat[a_funcstat$STATE %notin% c(60,66,69,72,78),]
  } 
 
  
  else{
  
  a_funcstat<-state_shp[state_shp$FUNCSTAT=="A",c("STATEFP","PLACEFP","geometry")]
  a_funcstat$GEOID<-paste0(as.character(a_funcstat$STATEFP),as.character(a_funcstat$PLACEFP))
  a_funcstat_states<-a_funcstat[a_funcstat$STATEFP %notin% c(60,66,69,72,78),]}

  print("made geoid") 
  
  
  ###MAKE THE GEOID COLUMN FOR ALL OF THEM
  #geoids<-as.vector(a_funcstat$GEOID) #geoid equivalent...sort of, have to use PLACEFP because 2000 doesn't have GEOID
  yearly_funca_allstates<-rbind(yearly_funca_allstates,a_funcstat_states)
  print("successfully rbind to master sf obj")
  print(paste0("finished ",file_list[i]))
  
  #this will need to be a spatial object
  #funca_2000<-rbind(funca_2000,selected_a)
  
  
}

st_write(yearly_funca_allstates,paste0("raw_data\\census_place\\yearly\\funca_",year[x],".shp"))
print(paste0("finished ",year[x]))

}

#do 2010 national file here...
cp_2010<-read_sf("C:\\Users\\thebrain\\Dropbox\\FireChasers\\ICA_repo\\ICA\\raw_data\\census_place\\national\\2010\\Place_2010Census_DP1.shp")
cp_2010$geoid_char<-as.character(cp_2010$GEOID10)
cp_2010$STATEFP<-substr(cp_2010$geoid_char,1,2)
cp_2010$PLACEFP<-substr(cp_2010$geoid_char,3,7)

funcstat10<-cp_2010[cp_2010$FUNCSTAT10=="A",c("STATEFP","PLACEFP","GEOID10")]
a_funcstat_states10<-funcstat10[funcstat10$STATEFP %notin% c(60,66,69,72,78),]
colnames(a_funcstat_states10)<-c("STATEFP","PLACEFP","GEOID","geometry")

st_write(a_funcstat_states10,"raw_data\\census_place\\yearly\\funca_2010.shp")


#then, the processed data can get read directly into the actual census place intersection script

#broken up year to year, except everything prior to 2007 gets associated to 2000 (this is for the 03_ script structure)