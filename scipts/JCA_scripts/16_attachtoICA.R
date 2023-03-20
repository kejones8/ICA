library(sf)

jca<-as.data.frame(st_read(incid_count_area_mtbs_out2))

ica<-read.csv("data\\oursample.csv")
ica$X.1<-NULL
ica$X<-NULL
ica$x<-NULL
ica$duration_days<-as.Date(ica$final_report_date)-as.Date(ica$discovery_date)

thebiz<-merge(ica,jca,by.x="incident_id",by.y="incident_id",all.x=TRUE)


pl<-read.csv("data\\pl_daysat_max.csv")
pl$X<-NULL

woopwoop<-merge(thebiz,pl,by.x="incident_id",by.y="incident_id",all.x=TRUE)
woopwoop$geom<-NULL
woopwoop$START_YEAR.y<-NULL


write.csv(woopwoop,"data\\final_ica_jca_tab_032023.csv")

#do quick check to see which incidents dropped out

orig<-read.csv("data\\final_ica_jca_tab_beforecompchange.csv")
orig$INCIDENT_ID[orig$INCIDENT_ID %notin% woopwoop$incident_id]
