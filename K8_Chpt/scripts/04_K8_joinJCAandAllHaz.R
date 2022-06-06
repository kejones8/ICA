#this script attaches other variables I might want to analyze

#read in the sample
sample<-read.csv(jca_samp_in)
#read in jurisdictional data
juris<-read.csv(final_out)
#read in all-haz data
allhaz_incident<-read.csv("data\\ics209-plus-wf_incidents_1999to2020.csv")
allhaz_sitreps<-read.csv("data\\ics209-plus-wf_sitreps_1999to2020.csv")

#get incidents from all haz & sitreps that are in my sample
incid_id<-unique(sample$incident_id)

allhaz_incid_insamp<-allhaz_incident[allhaz_incident$INCIDENT_ID %in% incid_id,]
sitrep_insamp<-allhaz_sitreps[allhaz_sitreps$INCIDENT_ID %in% incid_id,]


#check fire use types - we only care about wildfires
unique(allhaz_incid_insamp$INCTYP_ABBREVIATION)
#get just the wildfires
wf_inc<-allhaz_incid_insamp[allhaz_incid_insamp$INCTYP_ABBREVIATION %in% c("WF","WFU"),]
#see how it reduces dataset 
num_wf_inc<-length(unique(wf_inc$INCIDENT_ID))

wf_inc_sitrep<-merge(wf_inc,sitrep_insamp,by="INCIDENT_ID")

#how is type1/2 designated
unique(wf_inc_sitrep$IMT_MGMT_ORG_DESC)
table(wf_inc_sitrep$IMT_MGMT_ORG_DESC)

#there are blanks, but hope that those are mostly associated with sitreps for incidents that have some other
#designation across all other sitreps
typ1<-c("FUM1","Type 1 IC","Type 1 Team")
typ2<-c("FUM2","Type 2 IC","Type 2 Team")
typ3<-c("Type 3 IC","Type 3 Team")
typ4<-c("Type 4 IC")
typ5<-c("Type 5 IC")
nimo<-c("NIMO") #need to handle still
area<-c("Area Command") #need to handle still
unified<-c("Unified Command") #need to handle still
other<-c("FUMT","SOPL") #need to handle still

#after searching for these, see how many didn't get assigned to one of the groups above, meaning all sitreps were blank
incid_typ1<-unique(wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% typ1])
incid_typ2<-unique(wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% typ2])

incid_typ2_cln<-incid_typ2[incid_typ2 %notin% incid_typ1]

incid_typ3<-unique(wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% typ3])
incid_typ3_cln<-incid_typ3[incid_typ3 %notin% incid_typ2_cln]

incid_typ4<-unique(wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% typ4])
incid_typ4_cln<-incid_typ4[incid_typ4 %notin% incid_typ3_cln]

incid_typ5<-unique(wf_inc_sitrep$INCIDENT_ID[wf_inc_sitrep$IMT_MGMT_ORG_DESC %in% typ5])
incid_typ5_cln<-incid_typ5[incid_typ5 %notin% incid_typ4_cln]

df_typ1<-as.data.frame(cbind(incid_typ1,rep(1,length(incid_typ1))))
colnames(df_typ1)<-c("incident_id","team_type")
df_typ2<-as.data.frame(cbind(incid_typ2_cln,rep(1,length(incid_typ2_cln))))
colnames(df_typ2)<-c("incident_id","team_type")
df_typ3<-as.data.frame(cbind(incid_typ3_cln,rep(1,length(incid_typ3_cln))))
colnames(df_typ3)<-c("incident_id","team_type")
df_typ4<-as.data.frame(cbind(incid_typ4_cln,rep(1,length(incid_typ4_cln))))
colnames(df_typ4)<-c("incident_id","team_type")
df_typ5<-as.data.frame(cbind(incid_typ5_cln,rep(1,length(incid_typ5_cln))))
colnames(df_typ5)<-c("incident_id","team_type")

incid_teamtype<-rbind(df_typ1,df_typ2,df_typ3,df_typ4,df_typ5)
length(unique(incid_teamtype$incident_id)) #with just type 1-5, we capture 6798 of 8215 (which is the num incidents: length(unique(wf_inc_sitrep$INCIDENT_ID)))

addteamtype<-merge(incid_teamtype,wf_inc,by.x="incident_id",by.y="INCIDENT_ID",all.x=TRUE)
addjuris<-merge(addteamtype,juris,by="incident_id",all.x=TRUE)

cols_we_want<-c("incident_id","team_type","FINAL_ACRES","CAUSE","COMPLEX","DISCOVERY_DATE","FATALITIES","FUEL_MODEL","INJURIES_TOTAL","START_YEAR","FINAL_REPORT_DATE","PEAK_EVACUATIONS","fed_burn_cnt","fed_threat_cnt","trib_burn_cnt","trib_threat_cnt","st_burn_count","st_threat_count","cnty_burn_count","cnty_threat_count","cenpl_burn_count","cenpl_threat_count","gacc_burn_count","gacc_threat_count","jur_burned","jur_threatened")

analyzethis<-addjuris %>% select(cols_we_want)
analyzethis_cln<-analyzethis[!is.na(analyzethis$jur_burned),] #removes incident ids that don't have jursidictional information
#because I think all of these mtbs_ids couldn't be matched with mtbs original data

write.csv(analyzethis_cln,"K8_Chpt\\data\\foranalysis.csv")
