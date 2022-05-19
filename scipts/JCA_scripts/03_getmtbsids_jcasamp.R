#this script pulls in the downloaded mtbs data (needs to be downloaded manually) & matched to the ids in our sample
library(sf)
library(rmapshaper)
library(foreach) #for parallelizing intersection
library(doParallel)

#creates the notin function without importing packages
`%notin%` <- Negate(`%in%`)

#read in output from 01_getk8_sample.R that is a csv of incident_ids & their associated mtbs footprints
jca_samp<-read.csv("data\\JCA\\JCAsamp_inc_mtbsid.csv")

#read in mtbs data
all_mtbs<-read_sf("data\\mtbs_perimeter_data\\mtbs_perims_DD.shp")

colstocheck<-(all_mtbs[,c("Event_ID","Incid_Name","Ig_Date")])
colstocheck$geometry<-NULL
#write.csv(colstocheck,"JCA_K8_Chpt\\data\\scratch\\checking_mtbs_fortypos.csv")

#get a list of my mtbs ids
mtbs_toget<-jca_samp$mtbs_ids

#extract mtbs footprints that match our sample
select_mtbs<-all_mtbs[all_mtbs$Event_ID %in% mtbs_toget,]

write_sf(select_mtbs,"data\\JCA\\mtbs_match_jcasamp.shp")
select_mtbs<-read_sf("data\\JCA\\mtbs_match_jcasamp.shp")

#in order to buffer for threatened, need to project
sel_mtbs_proj<-st_transform(select_mtbs, 5070)

#function to buffer by 5 miles, then erase inside
outerBuffer<-function(x, dist){
  buff<-st_buffer(x, dist - 1, dissolve = T) #5 miles = 8047 meters
  e<-rmapshaper::ms_erase(buff,x)
  return(e)
}

registerDoParallel(makeCluster(12))
ptm <- proc.time()
print(Sys.time())

mtbs_tobuf<-unique(sel_mtbs_proj$Event_ID)

threat_work<-foreach(i=mtbs_tobuf, .combine = rbind, .packages=c('sf','rmapshaper')) %dopar%  {

  to_buf<-sel_mtbs_proj[sel_mtbs_proj$Event_ID==i,]
  threat<-outerBuffer(to_buf,8048)#5 miles = 8047 meters

}
print(Sys.time())
stopImplicitCluster()
proc.time() - ptm

write_sf(threat_work,"data\\JCA\\mtbs_match_jcasamp_threat.shp",overwrite=TRUE)


#now make a filled in buffer for using in teh intersects
buffer_nodonuts_forstinter<-st_buffer(sel_mtbs_proj,8048)

write_sf(buffer_nodonuts_forstinter,"data\\JCA\\mtbsbuf_nodonuts.shp")


###some diagnostics on mtbs availability relative to entire sample


#how many couldn't be found in mtbs data
howmanynomatch<-length(unique(mtbs_toget))-nrow(select_mtbs)

#which footprint weren't found
notfound<-mtbs_toget[mtbs_toget %notin% all_mtbs$Event_ID]

#which incidents drop out of my sample becuase no footprints were found
removedfromsamp<-mysample[mysample$mtbs_ids %in% notfound,] #208 footprints not found
length(unique(removedfromsamp$incident_id)) #207 incident ids removed

#one duplicate is
removedfromsamp$incident_id[duplicated(removedfromsamp$incident_id)]

#can i do a visual check to understand if there are just typos between all-hazards/mtbs
#checked the duplicate of the 2011 umpqua complex & its seems that both MTBS are typos?


#probably want to do an analysis of footprints dropped out by year
hist(removedfromsamp$START_YEAR,breaks=20)
#this is for my sample now, but might this patern hold for JCA?
#do i need to be able to explain why more fires drop out in some years than others?

#now plot out number of incidents per year in our jca_samp
getonlyinc<-jca_samp[jca_samp$mtbs_ids %notin% notfound,]

length(unique(getonlyinc$incident_id))

#for some reason, have way more events in 1999 - like twice as many as other years? this is the same for 
#mt chapter 1 sample, too
hist(getonlyinc$START_YEAR,breaks=20)
