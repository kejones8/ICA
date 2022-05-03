library(stringr)
library(tidyverse)
library(plyr)
library(reshape2)

#From St. Denis' data, determining my sample
incidents<-read.csv("data\\ics209-plus-wf_incidents_1999to2020.csv")
nrow(incidents)

#choose all of the rows that have a value filled for "lrgst_mtbs_fire_info"
inc_withmtbs<-incidents[incidents$LRGST_MTBS_FIRE_INFO!="",]
nrow(inc_withmtbs)

#make sure wildfires selected
wfinc_mtbs<-inc_withmtbs[inc_withmtbs$INCTYP_ABBREVIATION %in% c("WF","WFU"),]
length(unique(wfinc_mtbs$INCIDENT_ID))

#make sure the yeras i think are there are there
range(wfinc_mtbs$START_YEAR) #definitely stops at 2018

#check total acres burned from this sample
acres_peryear<- wfinc_mtbs %>% group_by(START_YEAR) %>% summarize(sum=sum(FINAL_ACRES))

#####MIGHT WANT TO COMPARE TO ENTIRE POPULATION TO SHOW HOW/IF OUR SAMPLE IS REPRESENTATIVE OF THE DATASET for acreage.

ggplot(acres_peryear, aes(x = START_YEAR , y= sum)) +
  geom_bar( stat = "identity")+ ggtitle("Total Acres Burned per Year")+
  xlab("year")+ ylab("Acres Burned")+ scale_y_continuous(labels = scales::comma)



#think I need to run functions in line, but they need to be in apply statement
tester<-(str_split(wfinc_mtbs$MTBS_FIRE_LIST, c("'|-")))
names(tester)<-wfinc_mtbs$INCIDENT_ID

# get_fire_text<-function(x){
#   as.character(str_extract_all(x[c("FOD_FIRE_LIST")],"'MTBS_ID': '[A-Z]{2}.+?(')"))
# }
# 
# fire_id_dirty<-apply(wfinc_mtbs,1,get_fire_text)
# 
# split_up<-function(x){
#   unlist(str_split(x, c("\\)|\\(|:|'")))
# }

#split_ids<-apply(as.data.frame(fire_id_dirty),1,split_up)

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

incid_years<-wfinc_mtbs[wfinc_mtbs$INCIDENT_ID %in% df$incident_id,c("INCIDENT_ID","START_YEAR")]

hmm<-df %>% 
  unnest(mtbs_ids) %>% 
  group_by(incident_id) %>% 
  dplyr::mutate(key = row_number()) %>% 
  spread(key, mtbs_ids)

#melt/reshape df to get list of mtbs ids
doesthiswork<-data.frame(hmm[1], F=unlist(hmm[-1]))

incids_mtbs_nonas<-doesthiswork[!is.na(doesthiswork$F),]
colnames(incids_mtbs_nonas)[2]<-"mtbs_ids"

addyears<-merge(incids_mtbs_nonas,incid_years,by.x="incident_id",by.y="INCIDENT_ID")

write.csv(addyears,"JCA_K8_Chpt\\data\\k8_incids_mtbsids_notmergedwithmtbsfootprintdownload.csv")



