library(tidyverse)
#this script takes outputs of 05b and removes the information we don't want to be consdiering for 
#individual jurisidcitions eitehr because it is not relevant or it is being replaced by "jurisdictions" 
#created in some other script

#this script also fixes an error by where the sumter NF was showing up in Va/WVa

#creates the notin function without importing packages
`%notin%` <- Negate(`%in%`)

#reads in both burned juridictions and threatened jurisdictions for cleaning
#section below (column naming,deletion) might need to get revisited based on structure of csvs written out from 05b
burn<-read.csv(burn_juris_byincid_out)
burn$burn_threat<-"burn"
threat<-read.csv(threat_juris_byincid_out)
end<-ncol(threat)
threat_up<-threat[,-c(end-1,end)] #remove previous columns written out into csv
threat_up$burn_threat<-"threat"
colnames(threat_up)[3]<-c("Event_ID") #make sure spelling is correct


# if same structure, make them into one df for processing
inc_juris<-rbind(burn,threat_up)

###FED
fed<-inc_juris[inc_juris$JrsdcUK=="Federal",]
unique(fed$JrsdcUA)
fed_nona<-fed[!is.na(fed$JrsdcUA),]

#othfed
othfed<-fed_nona[fed_nona$JrsdcUA=="OthFed",]
unique(othfed$JrsdcUN)
othfed_keep<-othfed[othfed$JrsdcUN=="Tennessee Valley Authority",]

freddies<-fed_nona[fed_nona$JrsdcUA %notin% c("BLM","BIA","OthFed"),] #handle BLM & BIA separately, just handled othfed above

fed_keep<-rbind(freddies,othfed_keep)

#do the clean up of federal burn/threatened
#so don't count (even correctly) threatened jursidictions that are captured in burned
fed_burn<-fed_keep[fed_keep$burn_threat=="burn",]
fed_threat<-fed_keep[fed_keep$burn_threat=="threat",]
fed_threat$torm_inburn<-ifelse(is.na(match(paste0(fed_threat$incident_id,fed_threat$JrsdcUI), 
                                                                        paste0(fed_burn$incident_id, fed_burn$JrsdcUI))),FALSE, TRUE)
fed_threat_burnrm<-fed_threat[fed_threat$torm_inburn==FALSE,]
fed_threat_burnrm$torm_inburn<-NULL

###look up handling for BOR, DOE, USFWS, DOD - implement this counting....
#search for any incident that turns up more than one USFS
#more than one DOD, for DOD keep burn and threatened separte BECAUSE
#if any dod is burned, count it as 1 burned, no threatened
#if no DOD is burned, count threatened as 1 
dod_count_burn<-fed_burn %>% filter(JrsdcUA=="DOD") %>% group_by(incident_id) %>% summarize(dod_count=n_distinct(JrsdcUN))
dod_count_burn$dod_cnt_cln<-1
dod_count_burn$dod_count<-NULL

threat_dod<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="DOD",]

fed_threat_dod<-threat_dod[threat_dod$incident_id %notin% dod_count_burn$incident_id,]

dod_count_threat<-fed_threat_dod %>% group_by(incident_id) %>% summarize(dod_count=n_distinct(JrsdcUN))
dod_count_threat$dod_cnt_thrt_cln<-1
dod_count_threat$dod_count<-NULL

write.csv(dod_count_burn,dod_burn_count_out)
write.csv(dod_count_threat,dod_threat_count_out)


#treat doe the same way as DOD
doe_count_burn<-fed_burn %>% filter(JrsdcUA=="DOE") %>% group_by(incident_id) %>% summarize(doe_count=n_distinct(JrsdcUN))
doe_count_burn$doe_cnt_cln<-1
doe_count_burn$doe_count<-NULL

threat_doe<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="DOE",]

fed_threat_doe<-threat_doe[threat_doe$incident_id %notin% doe_count_burn$incident_id,]

doe_count_threat<-fed_threat_doe %>% group_by(incident_id) %>% summarize(doe_count=n_distinct(JrsdcUN))
doe_count_threat$doe_cnt_thrt_cln<-1
doe_count_threat$doe_count<-NULL

write.csv(doe_count_burn,doe_burn_count_out)
write.csv(doe_count_threat,doe_threat_count_out)


#handle BOR the same as dod & doe
bor_count_burn<-fed_burn %>% filter(JrsdcUA=="BOR") %>% group_by(incident_id) %>% summarize(bor_count=n_distinct(JrsdcUN))
bor_count_burn$bor_cnt_cln<-1
bor_count_burn$bor_count<-NULL

threat_bor<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="BOR",]

fed_threat_bor<-threat_bor[threat_bor$incident_id %notin% bor_count_burn$incident_id,]

bor_count_threat<-fed_threat_bor %>% group_by(incident_id) %>% summarize(bor_count=n_distinct(JrsdcUN))
bor_count_threat$bor_cnt_thrt_cln<-1
bor_count_threat$bor_count<-NULL

write.csv(bor_count_burn,bor_burn_count_out)
write.csv(bor_count_threat,bor_threat_count_out)

#TVA
tva_count_burn<-fed_burn %>% filter(JrsdcUI=="TNTVA") %>% group_by(incident_id) %>% summarize(tva_count=n_distinct(JrsdcUN))
tva_count_burn$tva_cnt_cln<-1
tva_count_burn$tva_count<-NULL

threat_tva<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUI=="TNTVA",]

fed_threat_tva<-threat_tva[threat_tva$incident_id %notin% tva_count_burn$incident_id,]

tva_count_threat<-fed_threat_tva %>% group_by(incident_id) %>% summarize(tva_count=n_distinct(JrsdcUN))
tva_count_threat$tva_cnt_thrt_cln<-1
tva_count_threat$tva_count<-NULL


write.csv(tva_count_burn,tva_burn_count_out)
write.csv(tva_count_threat,tva_threat_count_out)


#USFWS
usfws_count_burn<-fed_burn %>% filter(JrsdcUA=="USFWS") %>% group_by(incident_id) %>% summarize(usfws_count=n_distinct(JrsdcUN))
usfws_count_burn$usfws_cnt_cln<-1
usfws_count_burn$usfws_count<-NULL

threat_usfws<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="USFWS",]

fed_threat_usfws<-threat_usfws[threat_usfws$incident_id %notin% usfws_count_burn$incident_id,]

usfws_count_threat<-fed_threat_usfws %>% group_by(incident_id) %>% summarize(usfws_count=n_distinct(JrsdcUN))
usfws_count_threat$usfws_cnt_thrt_cln<-1
usfws_count_threat$usfws_count<-NULL

write.csv(usfws_count_burn,usfws_burn_count_out)
write.csv(usfws_count_threat,usfws_threat_count_out)

#NPS
#want to count unique JrsdcUI codes
nps_count_burn<-fed_burn %>% filter(JrsdcUA=="NPS") %>% group_by(incident_id) %>% summarize(nps_count=n_distinct(JrsdcUN))

nps_count_threat<-fed_threat_burnrm %>% filter(JrsdcUA=="NPS") %>% group_by(incident_id) %>% summarize(nps_count=n_distinct(JrsdcUN))

write.csv(nps_count_burn,nps_burn_count_out)
write.csv(nps_count_threat,nps_threat_count_out)

#USFS
#first, do some cleaning for USFS - the Sumter
usfs<-inc_juris %>% filter(JrsdcUA=="USFS")
usfs_burn<-fed_burn %>% filter(JrsdcUA=="USFS")
usfs_threat<-fed_threat_burnrm%>% filter(JrsdcUA=="USFS")


# #fixing sumter events manually now because don't want to reintersecte everything
# sumt<-read.csv("data\\JCA\\marion_sumter_corrections.csv")
# 
# mtbs_tochange<-sumt$MTBS_ID
# 
# usfs_burn_badsumtrm<-usfs_burn[usfs_burn$Event_ID %notin% mtbs_tochange,]
# usfs_threat_badsumtrm<-usfs_threat[usfs_threat$Event_ID %notin% mtbs_tochange,]
# 
# 
# sumt_burn<-sumt[sumt$burn_threat=="burn",]
# sumt_threat<-sumt[sumt$burn_threat=="threat",]

# usfs_burn_intermed<-merge(usfs_burn,sumt_burn,by.x="Event_ID",by.y="MTBS_ID")
# usfs_burn_intermed$JrsdcUN<-usfs_burn_intermed$New_Forest
# usfs_burn_intermed$burn_threat.y<-NULL
# usfs_burn_intermed$New_Forest<-NULL
# colnames(usfs_burn_intermed)[ncol(usfs_burn_intermed)]<-"burn_threat"
# 
# #give it same column order to rbind
# usfs_burn_intermed<-usfs_burn_intermed[names(usfs_burn_badsumtrm)]
# 
# usfs_threat_intermed<-merge(usfs_threat,sumt_threat,by.x="Event_ID",by.y="MTBS_ID")
# usfs_threat_intermed$JrsdcUN<-usfs_threat_intermed$New_Forest
# usfs_threat_intermed$burn_threat.y<-NULL
# usfs_threat_intermed$New_Forest<-NULL
# colnames(usfs_threat_intermed)[ncol(usfs_threat_intermed)]<-"burn_threat"
# 
# #give it same column order to rbind
# usfs_threat_intermed<-usfs_threat_intermed[names(usfs_threat_badsumtrm)]
# 
# countthese_usfs_burn<-rbind(usfs_burn_badsumtrm,usfs_burn_intermed)
# countthese_usfs_threat<-rbind(usfs_threat_badsumtrm,usfs_threat_intermed)


#want to count unique JrsdcUI codes
# usfs_count_burn<-countthese_usfs_burn %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% summarize(usfs_count=n_distinct(JrsdcUN ))
# 
# usfs_count_threat<-countthese_usfs_threat %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% summarize(usfs_count=n_distinct(JrsdcUN ))

usfs_count_burn<-usfs_burn %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% summarize(usfs_count=n_distinct(JrsdcUN))

usfs_count_threat<-usfs_threat %>% filter(JrsdcUA=="USFS") %>% group_by(incident_id) %>% summarize(usfs_count=n_distinct(JrsdcUN))

write.csv(usfs_count_burn,usfs_burn_count_out)
write.csv(usfs_count_threat,usfs_threat_count_out)



###OTHER
other<-inc_juris[inc_juris$JrsdcUK=="Other",]
unique(other$JrsdcUA) 
othloc<-other[other$JrsdcUA=="OthLoc",] #local reserves, cant count them evenly - will be represented by county and census place
#keep tribal 
other_tribal_keep<-other[other$JrsdcUA=="Tribal" & !is.na(other$JrsdcUI) ,] #don't want othlocal, state, conty, city, NA

other_tribal_burn<-other_tribal_keep[other_tribal_keep$burn_threat=="burn",]

other_tribal_threat<-other_tribal_keep[other_tribal_keep$burn_threat=="threat",]
other_tribal_threat$torm_inburn<-ifelse(is.na(match(paste0(other_tribal_threat$incident_id,other_tribal_threat$JrsdcUI), 
                                           paste0(other_tribal_burn$incident_id, other_tribal_burn$JrsdcUI))),FALSE, TRUE)

othtrib_threat_burnrm<-other_tribal_threat[other_tribal_threat$torm_inburn==FALSE,]
othtrib_threat_burnrm$torm_inburn<-NULL

othtrib_count_burn<-other_tribal_burn  %>% group_by(incident_id) %>% summarize(trib_count=n_distinct(JrsdcUN))

othtrib_count_threat<-othtrib_threat_burnrm %>% group_by(incident_id) %>% summarize(trib_count=n_distinct(JrsdcUN))

write.csv(othtrib_count_burn,othtrib_burn_count_out)
write.csv(othtrib_count_threat,othtrib_threat_count_out)



###PRIVATE
priv<-inc_juris[inc_juris$JrsdcUK=="Private",]
unique(priv$JrsdcUA)
ancsa<-priv[priv$JrsdcUA=="ANCSA",] #keep these, make them tribal categorization


ancsa_burn<-ancsa[ancsa$burn_threat=="burn",]

ancsa_threat<-ancsa[ancsa$burn_threat=="threat",]
ancsa_threat$torm_inburn<-ifelse(is.na(match(paste0(ancsa_threat$incident_id,ancsa_threat$JrsdcUI), 
                                                    paste0(ancsa_burn$incident_id, ancsa_burn$JrsdcUI))),FALSE, TRUE)

ancsa_threat_burnrm<-ancsa_threat[ancsa_threat$torm_inburn==FALSE,]
ancsa_threat_burnrm$torm_inburn<-NULL

#ANCSA will get handled like dod/doe/majority of fed jurisdictions
#treat doe the same way as DOD
ancsa_count_burn<-ancsa_burn  %>% group_by(incident_id) %>% summarize(ancsa_count=n_distinct(JrsdcUN))
ancsa_count_burn$ancsa_cnt_cln<-1
ancsa_count_burn$ancsa_count<-NULL

threat_ancsa<-fed_threat_burnrm[fed_threat_burnrm$JrsdcUA=="ANCSA",]

fed_threat_ancsa<-ancsa_threat_burnrm[ancsa_threat_burnrm$incident_id %notin% ancsa_count_burn$incident_id,]

ancsa_count_threat<-fed_threat_ancsa %>% group_by(incident_id) %>% summarize(ancsa_count=n_distinct(JrsdcUN))
ancsa_count_threat$ancsa_cnt_thrt_cln<-1
ancsa_count_threat$ancsa_count<-NULL

write.csv(ancsa_count_burn,ancsa_burn_count_out)
write.csv(ancsa_count_threat,ancsa_threat_count_out)




###NA - these are census blocks, remove from jurisdictional count
na_cen<-inc_juris[is.na(inc_juris$JrsdcUK),]
unique(na_cen$JrsdcUA)
#no keepers here



