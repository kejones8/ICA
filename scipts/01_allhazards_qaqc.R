library(tidyverse)


#read in both incidents & sit reps for incidents
incidents<-read.csv("data\\ics209-plus-wf_incidents_1999to2020.csv")
sitreps<-read.csv("data\\ics209-plus-wf_sitreps_1999to2020.csv")

#check the nubmer of unique incident IDs for both datasets
length(unique(incidents$INCIDENT_ID))
length(unique(sitreps$INCIDENT_ID))

#check fire use types - we only care about wildfires
unique(incidents$INCTYP_ABBREVIATION)
#get just the wildfires
wf_inc<-incidents[incidents$INCTYP_ABBREVIATION %in% c("WF","WFU"),]
#see how it reduces dataset 
length(wf_inc$INCIDENT_ID)

#smush the incident IDs and sitreps together
wf_inc_sitrep<-merge(wf_inc,sitreps,by="INCIDENT_ID")

#how is type1/2 designated
unique(wf_inc_sitrep$IMT_MGMT_ORG_DESC)

#make list of incident_ids that have type 1 or 2 designations
type12_incid<-wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% c("Type 2 Team","Type 1 Team","Type 2 IC","Type 1 IC")] 
type12_incid_nimo<-wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% c("Type 2 Team","Type 1 Team","Type 2 IC","Type 1 IC","NIMO")] 

#baseline for sample size
type12_wf_inc<-wf_inc[wf_inc$INCIDENT_ID %in% type12_incid,]
type12_wf_inc_nimo<-wf_inc[wf_inc$INCIDENT_ID %in% type12_incid_nimo,]
length(unique(type12_wf_inc$INCIDENT_ID))
length(unique(type12_wf_inc_nimo$INCIDENT_ID))

#incidents by year
inc_peryear<- type12_wf_inc %>% group_by(START_YEAR) %>% tally()

#all incidents
ggplot(inc_peryear, aes(x = START_YEAR , y= n)) +
  geom_bar(position="dodge", stat = "identity")

inc_peryear_comp<- type12_wf_inc %>% group_by(START_YEAR,COMPLEX) %>% tally()

#complex vs. not
ggplot(inc_peryear_comp, aes(x = START_YEAR , y= n, fill=COMPLEX)) +
  geom_bar( stat = "identity")

#fire size
quantile(type12_wf_inc$FINAL_ACRES)
nrow(type12_wf_inc[type12_wf_inc$FINAL_ACRES>500000,])
hist(type12_wf_inc$FINAL_ACRES,breaks=1000,xlim=c(100,500000))

####FIRE LENGTH DAYS

#containment NA
nrow(type12_wf_inc[is.na(type12_wf_inc$FOD_CONTAIN_DOY),])
#disc NA
nrow(type12_wf_inc[is.na(type12_wf_inc$FOD_DISCOVERY_DOY),])
#get non-na DOY data
no_doy_na<-type12_wf_inc[!is.na(type12_wf_inc$FOD_DISCOVERY_DOY) & !is.na(type12_wf_inc$FOD_CONTAIN_DOY),]

nrow(no_doy_na[no_doy_na$LNGTH_FIRE_DAYS<0,])
#length of fires
no_doy_na$LNGTH_FIRE_DAYS<-no_doy_na$FOD_CONTAIN_DOY - no_doy_na$FOD_DISCOVERY_DOY

nrow(no_doy_na[is.na(no_doy_na$LNGTH_FIRE_DAYS),])

hist(no_doy_na$LNGTH_FIRE_DAYS,breaks=365)


type12_wf_inc$LNGTH_FIRE_DAYS[type12_wf_inc$LNGTH_FIRE_DAYS < 0] <- type12_wf_inc$LNGTH_FIRE_DAYS[type12_wf_inc$LNGTH_FIRE_DAYS <0] + 365 




type12_wf_inc$LNGTH_FIRE_DAYS[type12_wf_inc$LNGTH_FIRE_DAYS < 0,] <- type12_wf_inc$LNGTH_FIRE_DAYS + 365

type12_wf_inc$LNGTH_FIRE_DAYS<-if(type12_wf_inc$LNGTH_FIRE_DAYS<0,type12_wf_inc$LNGTH_FIRE_DAYS+365,)
hist(type12_wf_inc$LNGTH_FIRE_DAYS,breaks=100)
