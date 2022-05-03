#this script pulls in the downloaded mtbs data (needs to be downloaded manually) & matched to the ids in our sample
library(sf)

#creates the notin function without importing packages
`%notin%` <- Negate(`%in%`)

#read in output from 01_getk8_sample.R that is a csv of incident_ids & their associated mtbs footprints
mysample<-read.csv("JCA_K8_Chpt\\data\\k8_incids_mtbsids_notmergedwithmtbsfootprintdownload.csv")

#read in mtbs data
all_mtbs<-read_sf("data\\mtbs_perimeter_data\\mtbs_perims_DD.shp")

#get a list of my mtbs ids
mtbs_toget<-mysample$mtbs_ids

#extract mtbs footprints that match our sample
select_mtbs<-all_mtbs[all_mtbs$Event_ID %in% mtbs_toget,]

#how many couldn't be found in mtbs data
howmanynomatch<-length(unique(mtbs_toget))-nrow(select_mtbs)

#which footprint weren't found
notfound<-mtbs_toget[mtbs_toget %notin% all_mtbs$Event_ID]

#which incidents drop out of my sample becuase no footprints were found
removedfromsamp<-mysample[mysample$mtbs_ids %in% notfound,]


#can i do a visual check to understand if there are just typos between all-hazards/mtbs

#probably want to do an analysis of footprints dropped out by year