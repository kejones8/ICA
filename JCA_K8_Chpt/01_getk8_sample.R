library(stringr)

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

#####MIGHT WANT TO COMPARE TO ENTIRE POPULATION TO SHOW HOW/IF OUR SAMPLE IS REPRESENTATIVE OF THE DATASET

ggplot(acres_peryear, aes(x = START_YEAR , y= sum)) +
  geom_bar( stat = "identity")+ ggtitle("Total Acres Burned per Year")+
  xlab("year")+ ylab("Acres Burned")+ scale_y_continuous(labels = scales::comma)

#need to figure out how to extract mtbs footprints from the list...
#look back at other scripts? - couldn't find :/

test_col<-(wfinc_mtbs$FOD_FIRE_LIST)

tester<-wfinc_mtbs$FOD_FIRE_LIST[5]

f1 <- function(tester) {
  as.character(str_extract_all(tester,"'MTBS_ID': '[A-Z]{2}.+?(')"))#know that MTBS doesn't have repeating characters
}


howtobreak<-f1(tester)
split<-unlist(str_split(howtobreak, c("\\)|\\(|:|'")))
getids<-str_extract(split,"[A-Z]{2}[0-9]{10,}")
getids[!is.na(getids)]


#get every other element in list
