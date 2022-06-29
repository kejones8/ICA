library(sf)

jca<-as.data.frame(st_read(incid_count_area_mtbs_out2))

ica<-read.csv("oursample.csv")
ica$X.1<-NULL
ica$X<-NULL

thebiz<-merge(ica,jca,by.x="INCIDENT_ID",by.y="incident_id",all.x=TRUE)


thebiz$duration_days<-as.Date(thebiz$FINAL_REPORT_DATE)-as.Date(thebiz$DISCOVERY_DATE)

pl<-read.csv("data\\pl_daysat_max.csv")
pl$X<-NULL

woopwoop<-merge(thebiz,pl,by="INCIDENT_ID",all.x=TRUE)


write.csv(woopwoop,"data\\final_ica_jca_tab.csv")