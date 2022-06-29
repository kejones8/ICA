#this script is calculating percent and total area threatened & burned for 
#federal, state, tribal, other, private
#because I have already intersected surface management w/ burned & threatened footprints
#going to use surface management designation of Jrsdc UA to designate federal, state, tribal, local, private


library(sf)
library(rgeos)
library(sp)
library(raster)
require(UScensus2010)
library(stringr)
library(tidyverse)

#read in data with mtbs_ids & incident_ids
mysamp<-read.csv(jca_samp_in)

burn_surfman<-st_read(burn_surfman_inter_out)
threat_surfman<-st_read(threat_surfman_inter_out)
burn_surfman$JrsdcUA[is.na(burn_surfman$JrsdcUA)]<-"CenPriv"
threat_surfman$JrsdcUA[is.na(threat_surfman$JrsdcUA)]<-"CenPriv"

#split out designations for tabulating area
fed<-c("BLM","DOD","USFS","USFWS","NPS","BOR","DOE","OthFed")
state<-c("State")
trib<-c("BIA","ANCSA","Tribal")
local<-c("City","County","OthLoc")
priv<-c("CenPriv")

colnames(burn_surfman)[1]<-"Evnt_ID"

#merge mtbs_ids & incident_ids
burn_surfman_incid<-merge(burn_surfman,mysamp,by.x="Evnt_ID",by.y="mtbs_ids",all=TRUE)
threat_surfman_incid<-merge(threat_surfman,mysamp,by.x="Evnt_ID",by.y="mtbs_ids",all=TRUE)#note column name change


#now, make new column in burned & threatened that designates area category assignment
burn_surfman_incid$area_cat<-NA
threat_surfman_incid$area_cat<-NA

#assigning areas to categories we want to tally
burn_surfman_incid$area_cat[burn_surfman_incid$JrsdcUA %in% fed]<-"fed"
burn_surfman_incid$area_cat[burn_surfman_incid$JrsdcUA %in% state]<-"state"
burn_surfman_incid$area_cat[burn_surfman_incid$JrsdcUA %in% trib]<-"trib"
burn_surfman_incid$area_cat[burn_surfman_incid$JrsdcUA %in% local]<-"loc"
burn_surfman_incid$area_cat[burn_surfman_incid$JrsdcUA %in% priv]<-"priv"

#assigning areas to categories we want to tally
threat_surfman_incid$area_cat[threat_surfman_incid$JrsdcUA %in% fed]<-"fed"
threat_surfman_incid$area_cat[threat_surfman_incid$JrsdcUA %in% state]<-"state"
threat_surfman_incid$area_cat[threat_surfman_incid$JrsdcUA %in% trib]<-"trib"
threat_surfman_incid$area_cat[threat_surfman_incid$JrsdcUA %in% local]<-"loc"
threat_surfman_incid$area_cat[threat_surfman_incid$JrsdcUA %in% priv]<-"priv"

#get the nonempty geometries, clean up anything that won't return area
nonemptygeoms_burn <- burn_surfman_incid[!st_is_empty(burn_surfman_incid),,drop=FALSE]
nonemptygeoms_burn$bort<-"burn"
nonemptygeoms_burn$brn_ntr<-NULL
nonemptygeoms_burn$indicator<-NULL #only if kate's samp
nonemptygeoms_burn$burn_inter<-NULL #only if kate's samp
nonemptygeoms_threat <- threat_surfman_incid[!st_is_empty(threat_surfman_incid),,drop=FALSE]
nonemptygeoms_threat$bort<-"threat"
nonemptygeoms_threat$brn_ntr<-NULL
nonemptygeoms_threat$thrt_nt<-NULL
nonemptygeoms_threat$indictr<-NULL #only if kate's samp

colnames(nonemptygeoms_threat)<-colnames(nonemptygeoms_burn)

#get everything all together so just need to sort in in the calc_area function
nonemptygeoms<-rbind(nonemptygeoms_burn,nonemptygeoms_threat)
#figure out which/if any geoms drop out?
#check in qgis on of the mtbs footprints to see what might be the empty geometry
#these appear to be the mtbs_ids that didn't match in the MTBS full dataset
#when i searched in the intersected burn_surfman_data, nothing returned
# #so, don't think we're worried about these empty geoms
# burn_surfman_incid[st_is_empty(burn_surfman_incid),]
# threat_surfman_incid[st_is_empty(threat_surfman_incid),]


#unique incident_ids burned
# burn_uni_incid<-unique(nonemptygeoms_burn$incident_id)
# threat_uni_incid<-unique(nonemptygeoms_threat$incident_id)
uni_incids<-unique(nonemptygeoms$incident_id)


#to get unique incident_ids for threatened, want to remove any incident_ids where zero jurisdictions were threatened
#read in final jur table & sort on 0 jur_threatened
#then, remove those incidents from the threat_surfman_incid layer
#saves computations time 

#run burned and threatened separately!
#make function out of the operations below, store in another script
#then call in for threatened & burned separately!
jur_counts<-read.csv(final_out)
#threat_uni_incid<-jur_counts$incident_id[jur_counts$jur_threatened!=0]


#colnames to pass as argument to 13_area_calc_func
# burn_perc_colnames<-c("incident_id","Federal_PercBurn","Other_PercBurn","Private_PercBurn","State_PercBurn","Tribal_PercBurn")
# burn_acre_colnames<-c("incident_id","Federal_AcreBurn","Other_AcreBurn","Private_AcreBurn","State_AcreBurn","Tribal_AcreBurn")
# engag_perc_colnames<-c("incident_id","Federal_PercEngag","Other_PercEngag","Private_PercEngag","State_PercEngag","Tribal_PercEngag")
# engag_acre_colnames<-c("incident_id","Federal_AcreEngag","Other_AcreEngag","Private_AcreEngag","State_AcreEngag","Tribal_AcreEngag")


#jur_areas_vars are paths from 00_K8s_paths.R
#gonna try to make one function call for burn/engage
burn_engag_tab<-calc_areas(uni_incids,nonemptygeoms)#,burn_perc_colnames,burn_acre_colnames,engag_perc_colnames,engag_acre_colnames)


# burned_areas_tab<-calc_areas(burn_uni_incid,nonemptygeoms_burn,burn_perc_colnames,burn_acre_colnames)
# threat_areas_tab<-calc_areas(threat_uni_incid,nonemptygeoms_threat,threat_perc_colnames,threat_acre_colnames)
#threat_jump<-calc_areas("1999_AZ-ASD-C142_JUMP COMPLEX",jump_threat,threat_perc_colnames,threat_acre_colnames)
#threat_rainbow<-calc_areas("1999_AZ-FTA-172_RAINBOW",rb_threat,threat_perc_colnames,threat_acre_colnames)

#jump_rb_br_threat_out<-calc_areas(c("1999_AZ-ASD-C142_JUMP COMPLEX","1999_AZ-FTA-172_RAINBOW","1999_AR-BUP-99025_BUFFALO RIVER COMPLEX"),jump_rb_br_threat,threat_perc_colnames,threat_acre_colnames)


#burn_rainbow<-calc_areas("1999_AZ-FTA-172_RAINBOW",rb_burn,burn_perc_colnames,burn_acre_colnames)

#burn_threat_perc_area_tab<-merge(burned_areas_tab,threat_areas_tab,by="incident_id",all=TRUE)

#possible_final_columns<-merge(burned_areas_tab,threat_areas_tab,by="incident_id",all=TRUE)

#IMPORTANT FOR THIS WRITE OUT: NAs here are actually NAs, not 0. 
#They occur when no jurisdictions were threatened beyond already burned jurisdictions.
#comment above is old....shouldn't be any NAs now
write.csv(burn_engag_tab,burn_threat_perc_area_tab_out)

