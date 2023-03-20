#read in csv that contains subset of all hazards fires that might our criteria
#then determine which ones have MTBS footprints

library(stringr)
library(dplyr)
library(tidyr)

#read in output from 01_allhazards_
ica_samp<-read.csv("data\\oursample.csv")


length(unique(ica_samp$incident_id))

jca_samp<-ica_samp
#choose all of the rows that have a value filled for "lrgst_mtbs_fire_info"
#jca_samp<-ica_samp[ica_samp$LRGST_MTBS_FIRE_INFO!="",]
#nrow(jca_samp) #this doesn't seem to work cause this column is removed

#think I need to run functions in line, but they need to be in apply statement
tester<-(str_split(jca_samp$mtbs_fire_list, c("'|-")))
names(tester)<-jca_samp$incident_id



get_just_id<-function(x){
  str_extract(x,"[A-Z]{2}[0-9]{10,}")
}

grab_ids<-lapply(tester,get_just_id)
clean_ids<-lapply(grab_ids,function(x){x[!is.na(x)]})


nomtbs_rm <- clean_ids[lengths(clean_ids)!=0]

# row_nums<-as.integer(names(nomtbs_rm))
# incident_ids<-wfinc_mtbs[row_nums,c("INCIDENT_ID")]
# 
mtbs<-as.vector(nomtbs_rm)

df <- as.data.frame(matrix(0, ncol = 2, nrow = length(nomtbs_rm)))

colnames(df)[1] <- "incident_id"
#colnames(df)[2] <- "year"
colnames(df)[2] <- "mtbs_ids"

#df$row_number_wfincmtbs<-c(row_nums)
df$incident_id<-names(nomtbs_rm)
df$mtbs_ids<-mtbs

incid_years<-jca_samp[jca_samp$incident_id %in% df$incident_id,c("incident_id","start_year")]

hmm<-df %>% 
  unnest(mtbs_ids) %>% 
  group_by(incident_id) %>% 
  dplyr::mutate(key = row_number()) %>% 
  tidyr::spread(key, mtbs_ids)

#melt/reshape df to get list of mtbs ids
doesthiswork<-data.frame(hmm[1], F=unlist(hmm[-1]))

incids_mtbs_nonas<-doesthiswork[!is.na(doesthiswork$F),]
colnames(incids_mtbs_nonas)[2]<-"mtbs_ids"

addyears<-merge(incids_mtbs_nonas,incid_years,by.x="incident_id",by.y="incident_id")

write.csv(addyears,jca_samp_in)

