#this script takes the outputs of the intersections created in joining_firedata_mtbs3.R
#this script first converts the jurisdictions to be the final juris 2 & other columsn based on shannons' work
#we then group land area burned/affected by private, state, federal, other

library(sf)
library(rgeos)
library(sp)
library(raster)
require(UScensus2010)
library(stringr)
library(tidyverse)



#read in threatened jurisdiction geometries
file_list_inter5mibuf <- list.files(paste0(getwd(),"/mtbs/indiv_yr_5mibuf_inter/"), pattern = "*shp", full.names = TRUE)
buf5mi_list <- lapply(file_list_inter5mibuf, read_sf)
merged_5mi_inter<- do.call(rbind, buf5mi_list)

#read in affected jurisdiction geometries
file_list_inter <- list.files(paste0(getwd(),"/mtbs/indiv_yr_inter/"), pattern = "*shp", full.names = TRUE)
inter_list <- lapply(file_list_inter, read_sf)
merged_inter<- do.call(rbind, inter_list)

#reading in the incidents allows me to group the areas burned by incident_id
incidents<-as.data.frame(read.csv("C:\\Users\\thebrain\\Dropbox\\FireChasers\\new\\data\\Incident_records\\incidents_99_18.csv"))


#in the latest round, changed how MTBS columns were structured - this means I need to create a new table with incideint_ids & a row for every 
#MTBS listed
df <- data.table(incidents)

#get all mtbs ids associated and group by INCIDENT_ID
df <- df[, list(id = unique(c(MTBS_ID_1,MTBS_ID_2,MTBS_ID_3,MTBS_ID_4,
                              MTBS_ID_5,MTBS_ID_6,MTBS_ID_7,MTBS_ID_8,MTBS_ID_9,
                              MTBS_ID_10,MTBS_ID_11,MTBS_ID_12,MTBS_ID_13))), by = INCIDENT_ID]

setkeyv(df, c("INCIDENT_ID", "id"))
#then remove teh na's since it returns na as a unique value
incident_mtbs_tab<-df[!is.na(df$id) & df$id != "" ,]


#for affected & threatened, match up incident_id, jurisdictions,mtbs, and perimeter
poly_withid<-merge(merged_inter,incident_mtbs_tab,by.x="MTBS_ID",by.y="id")
buf_poly_withid<-merge(merged_5mi_inter,incident_mtbs_tab,by.x="MTBS_ID",by.y="id")

#now, need to remove and or edit jurisdictional records
#this is integrating all of shannon's cleaning, so we can then calculate area percentages
#by jurisdictional group AND know which jurisdictions occur on each incident

#shannons' modifications to juris1: i made the changes in the parent table, and i'm not going to remove national public lands
#because it's not getting counted anyways, and I want to keep the geometries for federal area tabulation

#read in shannon's mapping spreadsheets
mapping_juris<-read.csv("jurisdictions_0826\\jurisdiction_masterlist_080821.csv")
#reading in final jurisdiction with the juris2 modifications already in
final_jurs<-read.csv("jurisdictions_0826\\Final_Parent_JurisList_080821_kj.csv")

####poly_withid & buf_poly_withid - need to get uniquely matched with the old jurisdictional table
#based on teh JURIS1, Juris2, etc. columns..
diditwork_aff<-merge(poly_withid,mapping_juris,by=c("Juris","Juris1","Juris2"),all.x=TRUE)
diditwork_threat<-merge(buf_poly_withid,mapping_juris,by=c("Juris","Juris1","Juris2"),all.x=TRUE)
# this will provide a JURIS2_ID mapped to a final_jurisid where we need to reassign jurisdictions
# there will be records where there is no JURIS2_ID (private, etc.)
juris2id_na_aff<-diditwork_aff[is.na(diditwork_aff$JURIS2_ID),]
juris2id_na_threat<-diditwork_threat[is.na(diditwork_threat$JURIS2_ID),]
#these are records that we want to keep, but don't get reassigned to a final countable jurisdiction

#all juris2ids that are not na SHOULD have a final_jurisid
#are row numbers the same? YUP.
juris2id_toassign_newjuris_aff<-diditwork_aff[!is.na(diditwork_aff$JURIS2_ID),]
rows_withfinaljurisid_aff<-juris2id_toassign_newjuris_aff[!is.na(juris2id_toassign_newjuris_aff$FINAL_JURIS_ID),]
#now for threat
juris2id_toassign_newjuris_threat<-diditwork_threat[!is.na(diditwork_threat$JURIS2_ID),]
rows_withfinaljurisid_threat<-juris2id_toassign_newjuris_threat[!is.na(juris2id_toassign_newjuris_threat$FINAL_JURIS_ID),]

inc_geoms_finaljur_aff<-merge(rows_withfinaljurisid_aff,final_jurs,by="FINAL_JURIS_ID")
inc_geoms_finaljur_threat<-merge(rows_withfinaljurisid_threat,final_jurs,by="FINAL_JURIS_ID")

inc_geoms_final_jur_goodcols_aff<-inc_geoms_finaljur_aff[,c("INCIDENT_ID","FINAL_JURIS_ID","MTBS_ID","OBJID","Juris.y","Juris1.y","Juris2.y","Juris3","LndOwnK","LndOwnC","BIA_Region.y","geometry","JURIS_S")]
inc_geoms_final_jur_goodcols_threat<-inc_geoms_finaljur_threat[,c("INCIDENT_ID","FINAL_JURIS_ID","MTBS_ID","OBJID","Juris.y","Juris1.y","Juris2.y","Juris3","LndOwnK","LndOwnC","BIA_Region.y","geometry","JURIS_S")]


#once we've reassigned jurisdictions with a final_juris_id
#these jursidictions (get tabulated by area with all) BUT also get counted alongside the state and/or county
#for jurisdictional count
#all other jurisdictional rows don't get counted, but do get tabulated by area

#now, need to combine the "fixed" jurisdictions that get counted w/ the jurisdictions that also 
#just need to have area tabulated

#gotta get the columns correct
#what columns do i need for the rest of this script? 

juris2id_na_aff_goodcols<-juris2id_na_aff[,c("INCIDENT_ID","FINAL_JURIS_ID","MTBS_ID","OBJID","Juris","Juris1","Juris2","Juris3.x","LndOwnK.y","LndOwnC.y","BIA_Region","geometry","JURIS_S")]
juris2id_na_threat_goodcols<-juris2id_na_threat[,c("INCIDENT_ID","FINAL_JURIS_ID","MTBS_ID","OBJID","Juris","Juris1","Juris2","Juris3.x","LndOwnK.y","LndOwnC.y","BIA_Region","geometry","JURIS_S")]

torename_cols<-c("INCIDENT_ID","FINAL_JURIS_ID","MTBS_ID","OBJID","Juris","Juris1","Juris2","Juris3","LndOwnK","LndOwnC","BIA_Region","geometry","JURIS_S")

colnames(inc_geoms_final_jur_goodcols_aff)<-torename_cols
colnames(inc_geoms_final_jur_goodcols_threat)<-torename_cols

colnames(juris2id_na_aff_goodcols)<-torename_cols
colnames(juris2id_na_threat_goodcols)<-torename_cols

#modified jurisdictions affected - includes city/othloc/county designations
fin_aff<-rbind(inc_geoms_final_jur_goodcols_aff,juris2id_na_aff_goodcols)
fin_aff$aff_or_threat<-"Aff"
#modified jurisdictions threatened - ""
fin_threat<-rbind(inc_geoms_final_jur_goodcols_threat,juris2id_na_threat_goodcols)
fin_threat$aff_or_threat<-"Threat"

all_poss_jurs<-rbind(fin_aff,fin_threat)
all_poss_jurs$geometry<-NULL


#before we go counting federal jurisdictions
#so, we need to correct for the absence of George Washington National Forest & Francis Marion Sumter National Forest
fm<-all_poss_jurs[all_poss_jurs$Juris2=="Francis Marion & Sumter National Forests",]
fm_uni<-unique(fm)

#did this visually from mtbs footprints & surf man & national forest shp
#aff
mtbs_ids_tofind_aff<-c("VA3781207927120020605","VA3759507947420020605","VA3867707856220060430","VA3740607979020060304",
                   "VA3852107903820110222","VA3852107903820110219","VA3738208020020110219","VA3850907865220120408",
                   "WV3892507868820120408",'VA3787807965420120407',"VA3795507978320120411","VA3775808021320120407",
                   "VA3763008006320120407","VA3763008006320120407")


correct_nf_aff<-c("George Washington National Forest","Jefferson National Forest","George Washington National Forest",
              "Jefferson National Forest","George Washington National Forest","George Washington National Forest",
              "Jefferson National Forest","George Washington National Forest","George Washington National Forest",
              "George Washington National Forest","George Washington National Forest","George Washington National Forest",
              "George Washington National Forest","Jefferson National Forest")
final_jurid_aff<-c("1707","1708","1707","1708","1707","1707","1708","1707","1707","1707","1707","1707","1707","1708")
aff_or_threat_aff<-rep("Aff",length(final_jurid_aff))


#also checked for fires that didn't affect the george or jeff, but didn't find any that only threatened
#threat
mtbs_ids_tofind_threat<-c("VA3781207927120020605","VA3759507947420020605","VA3759507947420020605","VA3867707856220060430",
                          "VA3740607979020060304","VA3852107903820110222","VA3852107903820110219","VA3738208020020110219",
                          "VA3850907865220120408","WV3892507868820120408",'VA3787807965420120407',"VA3795507978320120411","VA3775808021320120407",
                          "VA3763008006320120407","VA3763008006320120407","VA3787807965420120407")

correct_nf_threat<-c("George Washington National Forest","Jefferson National Forest","George Washington National Forest",
                     "George Washington National Forest","Jefferson National Forest","George Washington National Forest",
                     "George Washington National Forest","George Washington National Forest","George Washington National Forest",
                     "George Washington National Forest","George Washington National Forest","George Washington National Forest","George Washington National Forest",
                     "George Washington National Forest","Jefferson National Forest","Jefferson National Forest") 
final_jurid_threat<-c("1707","1708","1707","1707","1708","1707","1707","1707","1707","1707","1707","1707","1707","1707","1708","1707")
aff_or_threat_threat<-rep("Threat",length(final_jurid_threat))

#put threat & aff together
mtbs_ids_tofind<-c(mtbs_ids_tofind_aff,mtbs_ids_tofind_threat)
final_jurid<-c(final_jurid_aff,final_jurid_threat)
correct_nf<-c(correct_nf_aff,correct_nf_threat)
aff_threat<-c(aff_or_threat_aff,aff_or_threat_threat)

#make new df to join on 
tocorrect_nf<-cbind(mtbs_ids_tofind,correct_nf,final_jurid,aff_threat)
#name cols of that df so we can do some column filling/deletion in next steps
colnames(tocorrect_nf)<-c("MTBS_ID","Juris2_good","FINAL_JURIS_ID_good","aff_thrt")

#merge the corrected jurisdictions with the old jurisdictional rows
blh<-merge(fm_uni,tocorrect_nf,by="MTBS_ID")
bleh<-blh[blh$aff_or_threat==blh$aff_thrt,] #get rid of weir redundancies based on how the join was done
bleh$Juris2<-bleh$Juris2_good #reassing to original column names so we can rbind back to all data below
bleh$FINAL_JURIS_ID<-bleh$FINAL_JURIS_ID_good
bleh$aff_thrt<-NULL #get rid of additional columns
bleh$Juris2_good<-NULL
bleh$FINAL_JURIS_ID_good<-NULL

#when add these back, only want to remove frncis marion rows from all_poss_jurs that 
#correspond to the MTBS ids in wv,va
no_francis<-all_poss_jurs[all_poss_jurs$Juris2!="Francis Marion & Sumter National Forests",]
sc_francis<-all_poss_jurs[all_poss_jurs$INCIDENT_ID=="2002_SC-FMF-264_MONKEY FACE COMPLEX",]

#created new final dataframe of jurisdictions....now do counting
coulditbe<-rbind(no_francis,sc_francis,bleh)

#need to make new divvy of threat & affected
aff_itis<-coulditbe[coulditbe$aff_or_threat=="Aff",]
threat_itis<-coulditbe[coulditbe$aff_or_threat=="Threat",]

#jurisdictiosn to FLAG YES or LndOwnC STATE - Private, State, OthLoc, City, County
lndownC_trigger_state<-c("Private","State","OthLoc","City","County")


#by state, by threatened & affected, find jurisdictions per incidnet
find_state_cnt<-coulditbe %>% group_by(INCIDENT_ID,JURIS_S,aff_or_threat) %>% summarize(state_y_n=unique(Juris1))
#get rid of the fluff (i.e. duplicates)
uni_find_state_cnt<-unique(find_state_cnt)

#gets non state occurrences
non_state_cnt<-find_state_cnt[find_state_cnt$state_y_n %notin% lndownC_trigger_state,]
non_state_cnt$state_y_n<-NULL
uni_non_state_cnt<-unique(non_state_cnt[,c("INCIDENT_ID","JURIS_S","aff_or_threat")])

#should provide incident, affected/threatened where state count should be maintained
state_cnt<-find_state_cnt[find_state_cnt$state_y_n %in% lndownC_trigger_state,]
state_cnt$state_y_n<-NULL
uni_state_cnt<-unique(state_cnt[,c("INCIDENT_ID","JURIS_S","aff_or_threat")])

#join on the 3 columns & check for matches/non matches
#non matches are where we want to reduce state counts
df1 <- non_state_cnt %>%
  left_join(state_cnt %>% transmute(INCIDENT_ID, JURIS_S,aff_or_threat, check = 'yes')) %>%
  replace_na(list(check = 'no'))

#get rid of fluff, search for unique data rows
uni_df1<-unique(df1)

#get the places where state should be reduced
uni_df1no<-uni_df1[uni_df1$check=='no',]

#shows what counts to reduce states affected & threatened columns by 
tally_to_remove<-uni_df1no %>% group_by(INCIDENT_ID,aff_or_threat) %>% mutate(st_reduce=n_distinct(JURIS_S))

write.csv(tally_to_remove,"reduce_state_cnt_0827.csv")

threat_nopubland<-threat_itis[threat_itis$Juris2!="National Public Lands",]
aff_nopubland<-aff_itis[aff_itis$Juris2!="National Public Lands",]


#count federal & tribal, affected & threatened
threat_trib<-threat_nopubland %>% filter(Juris1 %in% c("BIA","Tribal","ANCSA"))%>% group_by(INCIDENT_ID) %>% summarize(TRIBAL_THREAT=n_distinct(FINAL_JURIS_ID))
aff_trib<-aff_nopubland %>% filter(Juris1 %in% c("BIA","Tribal","ANCSA"))%>% group_by(INCIDENT_ID) %>% summarize(TRIBAL_AFF=n_distinct(FINAL_JURIS_ID))

aff_fed<-aff_nopubland %>% filter(Juris1  %in% c("BLM","BOR","DOD","DOE","NPS","OthFed","USFS","USFWS"))%>% group_by(INCIDENT_ID) %>% summarize(FED_AFF=n_distinct(FINAL_JURIS_ID))
threat_fed<-threat_nopubland %>% filter(Juris1  %in% c("BLM","BOR","DOD","DOE","NPS","OthFed","USFS","USFWS"))%>% group_by(INCIDENT_ID) %>% summarize(FED_THREAT=n_distinct(FINAL_JURIS_ID))


write.csv(aff_fed,"aff_fed_count_0827.csv")
write.csv(aff_trib,"aff_trib_count_0827.csv")

write.csv(threat_fed,"threat_fed_count_0827.csv")
write.csv(threat_trib,"threat_trib_count_0827.csv")


#now, go into loop that is calculating % area affected/threatened by jur_group

#taking state column out to remove unnecessary info
fin_aff1<-subset(aff_itis,select=-c(JURIS_S,aff_or_threat))
fin_threat1<-subset(threat_itis,select=-c(JURIS_S,aff_or_threat))

#now need to create fed, state, priv, other groupings.
#get all possible land owners
juris1<-unique(fin_aff1$Juris1) #same as affected

#make corresponding vector putting the landowners in the categories that we want to calc (fed, state, priv, other)



###THIS WILL NEED TO CHANGE based on the length & elements of lndownc above

level_togroup<-c("Federal","Federal","Federal","Federal","State","Other","Tribal","Tribal","Tribal","Federal","Federal","Federal","Federal","Private","Other","Other","Other")

#put it in a table to join
juris_groupings_tab<-as.data.frame(cbind(juris1,level_togroup))
colnames(juris_groupings_tab)<-c("Juris1","AreaCalc_Group")

###now put the area calc groups in the merged_inter data
merged_inter_groupedjur<-merge(fin_aff1,juris_groupings_tab,by.x="Juris1")
merged_buf_inter_groupedjur<-merge(fin_threat1,juris_groupings_tab,by.x="Juris1")

nonemptygeoms <- merged_inter_groupedjur[!st_is_empty(merged_inter_groupedjur),,drop=FALSE]
nonempty_buf_geoms <- merged_buf_inter_groupedjur[!st_is_empty(merged_buf_inter_groupedjur),,drop=FALSE]

uni_incidid<-unique(nonemptygeoms$INCIDENT_ID) #should be the same incidents across aff/threat
uni_incidid<-uni_incidid[!is.na(uni_incidid)]

where_to_put_it_reg<-data.frame()
where_to_put_it_buf<-data.frame()

count=0
for (i in uni_incidid){
  #i<-"1999_AR-BUP-99025_BUFFALO RIVER COMPLEX" #testing this one because it has 2 mtbs ids
  count=count+1 
  print(count)
  print(i)
#was originally doing it by MTBS id to test the method
#test_grouped_areas<-merged_inter_groupedjur[merged_inter_groupedjur$FIRE_ID=="WA4805112011320150814",]
#now incident id for the correct calculations
test_grouped_areas<-nonemptygeoms[nonemptygeoms$INCIDENT_ID==i,]
print("got nonempty geoms for aff")
test_buf_grouped_areas<-nonempty_buf_geoms[nonempty_buf_geoms$INCIDENT_ID==i,]
print("got nonempty geoms for threat")

test_byjuris= test_grouped_areas %>% 
  st_set_precision(10000) %>% 
  group_by(INCIDENT_ID,MTBS_ID,AreaCalc_Group) %>% 
  mutate(juris_group=paste0(MTBS_ID,"_",AreaCalc_Group))
print("created mtbs_jurgroups aff")

test_buf_byjuris= test_buf_grouped_areas %>% 
  st_set_precision(10000) %>% 
  group_by(INCIDENT_ID,MTBS_ID,AreaCalc_Group) %>% 
  mutate(juris_group=paste0(MTBS_ID,"_",AreaCalc_Group))
print("created mtbs_jurgroups threat")

#this variable gets used later in area calcs
incid_id<-test_byjuris$INCIDENT_ID[1]


#hmm. 
test_bygoodjuris<-test_byjuris[!is.na(test_byjuris$geometry),]
testing<-as_Spatial(test_bygoodjuris, cast = TRUE, IDs = juris_group)
print("making spatial object aff")

test_by_buf_goodjuris<-test_buf_byjuris[!is.na(test_buf_byjuris$geometry),]
testing_buf<-as_Spatial(test_by_buf_goodjuris, cast = TRUE, IDs = juris_group)
print("making spatial object threat")

#SpatialPolygonsDataFrame(testing,)
#test_sp<-SpatialPolygons(test_bygoodjuris)
#test_this<-as(test_bygoodjuris, 'Spatial')

hmm<-gUnaryUnion(testing, id = testing@data$juris_group)
print("unioning aff same juris_group")

hmm_buf<-gUnaryUnion(testing_buf, id = testing_buf@data$juris_group)#, checkValidity=NULL)
print("unioning threat same juris_group")
  # mutate(new_unique_poly=st_area())
  # 
# dev.off()
# plot(hmm[1],col="blue")
# plot(hmm[2],col="green",add=TRUE)
# plot(hmm[3],col="orange",add=TRUE)
# plot(hmm[4],col="purple",add=TRUE)

# Extract polygon ID's
pid <- sapply(slot(hmm, "polygons"), function(x) slot(x, "ID")) 
print("Extracted polygon aff ids")
pid_buf <- sapply(slot(hmm_buf, "polygons"), function(x) slot(x, "ID")) 
 # pid_area<-sapply(slot(hmm, "polygons"), function(x) area(slot(x, "ID")))
print("Extracted polygon threat ids")

# Create dataframe with correct rownames
p.df <- as.data.frame(cbind(1:length(hmm), pid),row.names=pid) 
colnames(p.df)<-c("id","mtbs_jur_group")
# Try coersion again and check class
p <- SpatialPolygonsDataFrame(hmm, p.df)

print("created spatial df for aff")

p.df_buf <- as.data.frame(cbind(1:length(hmm_buf), pid_buf),row.names=pid_buf) 
colnames(p.df_buf)<-c("id","mtbs_jur_group")
# Try coersion again and check class
p_buf <- SpatialPolygonsDataFrame(hmm_buf, p.df_buf)

print("created spatial df for threat")

hopeful<-areaPoly(p)
print("got area of aff poly")
hopeful_buf<-areaPoly(p_buf)
print("got area of threat poly")

p@data$area<-hopeful
p@data$INCIDENT_ID<-rep(incid_id,nrow(p@data))
print("vector of aff incident ids to match data rows")

p_buf@data$area<-hopeful_buf
p_buf@data$INCIDENT_ID<-rep(incid_id,nrow(p_buf@data))
print("vector of threat incident ids to match data rows")


p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
#p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
p@data$mtbs_id<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})

print("assigning juris group + mtbs_id aff")

p_buf@data$jur_group<-sapply(p_buf@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
#p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
p_buf@data$mtbs_id<-sapply(p_buf@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})

print("assigning juris group + mtbs_id threat")

p@data$percent_burned_area<-sapply(p@data$area,function(x){round((x/sum(p@data$area))*100,1)})
 #first real change across the regular and buffered dataset
p_buf@data$percent_threatened_area<-sapply(p_buf@data$area,function(x){round((x/sum(p_buf@data$area))*100,1)})

print("calc & assign percent aff & threat")

p@data$areaburned_acres<-round(p@data$area * 0.000247105) #convert from sq. m. to acres 
p_buf@data$areathreatened_acres<-round(p_buf@data$area * 0.000247105) #convert from sq. m. to acres 

print("calc & assign acres aff & threat")

where_to_put_it_reg<-rbind(where_to_put_it_reg,p@data)
where_to_put_it_buf<-rbind(where_to_put_it_buf,p_buf@data)
print("rbind new data for aff/threat, on to the next incident")

##at this point, need to have NA's or 0's represented for each category...
## or could process both the tables & add it in after the fact based on incident_id?


}


###the factor statement is wrong!! accidentally overwriote - so recreating dataframe - then create NEW factor column


where_to_put_it_reg$jur_group_fac<-as.factor(where_to_put_it_reg$jur_group)

wut_reg<-where_to_put_it_reg %>% group_by(INCIDENT_ID,jur_group_fac,.drop=FALSE) %>% summarize(jur_group_burnperc=sum(percent_burned_area))


where_to_put_it_buf$jur_group_fac<-as.factor(where_to_put_it_buf$jur_group)

wut_buf<-where_to_put_it_buf %>% group_by(INCIDENT_ID,jur_group_fac,.drop=FALSE) %>% summarize(jur_group_threatperc=sum(percent_threatened_area))


wide_reg<-pivot_wider(wut_reg, names_from = jur_group_fac, values_from = jur_group_burnperc)
colnames(wide_reg)<-c("INCIDENT_ID","Federal_PercAffected","Other_PercAffected","Private_PercAffected","State_PercAffected","Tribal_PercAffected")

wide_buf<-pivot_wider(wut_buf, names_from = jur_group_fac, values_from = jur_group_threatperc)
colnames(wide_buf)<-c("INCIDENT_ID","Federal_PercThreat","Other_PercThreat","Private_PercThreat","State_PercThreat","Tribal_PercThreat")


possible_final_columns<-merge(wide_buf,wide_reg,by="INCIDENT_ID",all=TRUE)



write.csv(possible_final_columns,"area_affec_threat_by_jurislevel_0826.csv")



