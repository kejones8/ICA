library(sf)
library(rgeos)
library(sp)
library(raster)
require(UScensus2010)
library(stringr)
library(tidyverse)



calc_areas<-function (uni_incids,nonemptygeoms,burn_or_threatcols_perc,burn_or_threatcols_acre){
  
  
  # nonemptygeoms<-nonemptygeoms_burn
  # i<-"2005_AK-SWS-504153_PILOT POINT"
  colnames(nonemptygeoms)[1]<-"Event_ID"
  
  nonemptygeoms_sel<-nonemptygeoms[,c("Event_ID","incident_id","area_cat")]
  put_calcs_here_rbind<-data.frame()
  put_calcs_perc<-data.frame()
  put_calcs_acre<-data.frame()
  
count=0

for (i in uni_incids){
  #i<-"1999_AR-BUP-99025_BUFFALO RIVER COMPLEX" #testing this one because it has 2 mtbs ids
  count=count+1 
  print(count)
  print(i)
  put_calcs_here<-data.frame()
  #was originally doing it by MTBS id to test the method
  #test_grouped_areas<-merged_inter_groupedjur[merged_inter_groupedjur$FIRE_ID=="WA4805112011320150814",]
  #now incident id for the correct calculations
  test_grouped_areas<-nonemptygeoms_sel[nonemptygeoms_sel$incident_id==i,]
  print("got non-empty geoms for incident_id")
  # test_buf_grouped_areas<-nonempty_buf_geoms[nonempty_buf_geoms$INCIDENT_ID==i,]
  # print("got nonempty geoms for threat")
  
  test_byjuris= test_grouped_areas %>% 
    st_set_precision(10000) %>% 
    group_by(incident_id,Event_ID, area_cat) %>% 
    mutate(juris_group=paste0(Event_ID,"_",area_cat))
  print("created mtbs & juris groups")
  
  # test_buf_byjuris= test_buf_grouped_areas %>% 
  #   st_set_precision(10000) %>% 
  #   group_by(INCIDENT_ID,MTBS_ID,AreaCalc_Group) %>% 
  #   mutate(juris_group=paste0(MTBS_ID,"_",AreaCalc_Group))
  # print("created mtbs_jurgroups threat")
  
  #this variable gets used later in area calcs
  #i think this is just "i" in this case because I adjusted how the code was written...
  #incid_id<-test_byjuris$incident_id[1]
  
  
  #hmm. 
  test_bygoodjuris<-test_byjuris[!is.na(test_byjuris$geometry),] #might not be necessary
  testing<-as_Spatial(test_bygoodjuris, cast = TRUE, IDs = juris_group)
  print("making spatial object")
  
  # test_by_buf_goodjuris<-test_buf_byjuris[!is.na(test_buf_byjuris$geometry),]
  # testing_buf<-as_Spatial(test_by_buf_goodjuris, cast = TRUE, IDs = juris_group)
  # print("making spatial object threat")
  
  #SpatialPolygonsDataFrame(testing,)
  #test_sp<-SpatialPolygons(test_bygoodjuris)
  #test_this<-as(test_bygoodjuris, 'Spatial')
  
  hmm<-gUnaryUnion(testing, id = testing@data$juris_group)
  print("unioning same juris_group")
  
  # hmm_buf<-gUnaryUnion(testing_buf, id = testing_buf@data$juris_group)#, checkValidity=NULL)
  # print("unioning threat same juris_group")
  # mutate(new_unique_poly=st_area())
  # 
  # dev.off()
  # plot(hmm[1],col="blue")
  # plot(hmm[2],col="green",add=TRUE)
  # plot(hmm[3],col="orange",add=TRUE)
  # plot(hmm[4],col="purple",add=TRUE)
  
  # Extract polygon ID's
  pid <- sapply(slot(hmm, "polygons"), function(x) slot(x, "ID")) 
  print("Extracted polygon ids")
  
  # pid_buf <- sapply(slot(hmm_buf, "polygons"), function(x) slot(x, "ID")) 
  # # pid_area<-sapply(slot(hmm, "polygons"), function(x) area(slot(x, "ID")))
  # print("Extracted polygon threat ids")
  
  # Create dataframe with correct rownames
  p.df <- as.data.frame(cbind(1:length(hmm), pid),row.names=pid) 
  colnames(p.df)<-c("id","mtbs_jur_group")
  # Try coersion again and check class
  p <- SpatialPolygonsDataFrame(hmm, p.df)
  
  print("created spatial df")
  
  # p.df_buf <- as.data.frame(cbind(1:length(hmm_buf), pid_buf),row.names=pid_buf) 
  # colnames(p.df_buf)<-c("id","mtbs_jur_group")
  # # Try coersion again and check class
  # p_buf <- SpatialPolygonsDataFrame(hmm_buf, p.df_buf)
  # 
  # print("created spatial df for threat")
  
  hopeful<-areaPoly(p)
  print("got area of poly")
  # hopeful_buf<-areaPoly(p_buf)
  # print("got area of threat poly")
  
  p@data$area<-hopeful
  p@data$incident_id<-rep(i,nrow(p@data))
  print("vector of aff incident ids to match data rows")
  
  # p_buf@data$area<-hopeful_buf
  # p_buf@data$INCIDENT_ID<-rep(incid_id,nrow(p_buf@data))
  # print("vector of threat incident ids to match data rows")
  
  
  p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
  #p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
  p@data$Event_ID<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})
  
  print("assigning juris group + mtbs_id")
  
  # p_buf@data$jur_group<-sapply(p_buf@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
  # #p@data$jur_group<-sapply(p@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][2]})
  # p_buf@data$mtbs_id<-sapply(p_buf@data$mtbs_jur_group,function(x){str_split(x,"_")[[1]][1]})
  # 
  # print("assigning juris group + mtbs_id threat")
  
  p@data$percent_burned_area<-sapply(p@data$area,function(x){round((x/sum(p@data$area))*100,1)})
  #first real change across the regular and buffered dataset
  #p_buf@data$percent_threatened_area<-sapply(p_buf@data$area,function(x){round((x/sum(p_buf@data$area))*100,1)})
  
  print("calc & assign percent")
  
  p@data$areaburned_acres<-round(p@data$area * 0.000247105) #convert from sq. m. to acres 
  # p_buf@data$areathreatened_acres<-round(p_buf@data$area * 0.000247105) #convert from sq. m. to acres 
  # 
  # print("calc & assign acres aff & threat")
  # put_calcs_here<-p@data
  # put_calcs_here$jur_group<-as.factor(put_calcs_here$jur_group)
  p@data$jur_group<-as.factor(p@data$jur_group)
  put_calcs_here<-p@data
  put_calcs_here$jur_group<-as.factor(put_calcs_here$jur_group)
  
  put_calcs_here_rbind<-rbind(put_calcs_here_rbind,p@data)
  #where_to_put_it_buf<-rbind(where_to_put_it_buf,p_buf@data)
  print("rbind new data, on to the next incident")
  
  
  #put_calcs_here$jur_group_fac<-as.factor(put_calcs_here$jur_group)
  
  
  wut_reg_perc<-put_calcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% summarize(jur_group_burnperc=sum(percent_burned_area))
  wut_reg_acre<-put_calcs_here %>% group_by(incident_id,jur_group,.drop=FALSE) %>% summarize(jur_group_burnacres=sum(areaburned_acres))
  
  wide_reg_perc<-as.data.frame(tidyr::pivot_wider(wut_reg_perc, names_from = jur_group, values_from = jur_group_burnperc))
  wide_reg_acre<-as.data.frame(tidyr::pivot_wider(wut_reg_acre, names_from = jur_group, values_from = jur_group_burnacres))
  
  print ("made wide stuff")
  
  if (is.null(wide_reg_perc$fed)) {
    wide_reg_perc$fed<-0
    wide_reg_acre$fed<-0
  } else {
    print("fed col there")
  }
  
  if (is.null(wide_reg_perc$trib)) {
    wide_reg_perc$trib<-0
    wide_reg_acre$trib<-0
    
  } else {
    print("trib col there")
  }
  
  
  if (is.null(wide_reg_perc$state)) {
    wide_reg_perc$state<-0
    wide_reg_acre$state<-0
    
  } else {
    print("state col there")
  }
  
  if (is.null(wide_reg_perc$loc)) {
    wide_reg_perc$loc<-0
    wide_reg_acre$loc<-0
    
  } else {
    print("loc col there")
  }
  
  if (is.null(wide_reg_perc$priv)) {
    wide_reg_perc$priv<-0
    wide_reg_acre$priv<-0
    
  } else {
    print("priv col there")
  }
  
  print("added cols as needed")

  wide_perc_order<-wide_reg_perc[,c("incident_id","fed","loc","priv","state","trib")]
  wide_acre_order<-wide_reg_acre[,c("incident_id","fed","loc","priv","state","trib")]
  
  print ("renamed cols")
  
  put_calcs_acre<-rbind(put_calcs_acre,wide_acre_order)
  put_calcs_perc<-rbind(put_calcs_perc,wide_perc_order)
  
  print("successfully added new records, now onto next incident")
  print(put_calcs_here)
  print(put_calcs_acre)
  print(put_calcs_perc)
}
#return(put_calcs_here)
#put_calcs_here$jur_group_fac<-as.factor(put_calcs_here$jur_group)
#return(put_calcs_here)

# wut_reg_perc<-put_calcs_here %>% group_by(incident_id,jur_group_fac,.drop=FALSE) %>% summarize(jur_group_burnperc=sum(percent_burned_area))
# wut_reg_acre<-put_calcs_here %>% group_by(incident_id,jur_group_fac,.drop=FALSE) %>% summarize(jur_group_burnacres=sum(areaburned_acres))
# 
# wide_reg_perc<-as.data.frame(tidyr::pivot_wider(wut_reg_perc, names_from = jur_group_fac, values_from = jur_group_burnperc))
# wide_reg_acre<-as.data.frame(tidyr::pivot_wider(wut_reg_acre, names_from = jur_group_fac, values_from = jur_group_burnacres))

##at this point, need to have NA's or 0's represented for each category...
## or could process both the tables & add it in after the fact based on incident_id?
#print(put_calcs_here)
# 
# if (is.null(wide_reg_perc$fed)) {
#   wide_reg_perc$fed<-0
#   wide_reg_acre$fed<-0
# } else {
#   print("fed col there")
# }
# 
# if (is.null(wide_reg_perc$trib)) {
#   wide_reg_perc$trib<-0
#   wide_reg_acre$trib<-0
#   
# } else {
#   print("trib col there")
# }
# 
# 
# if (is.null(wide_reg_perc$state)) {
#   wide_reg_perc$state<-0
#   wide_reg_acre$state<-0
#   
# } else {
#   print("state col there")
# }
# 
# if (is.null(wide_reg_perc$loc)) {
#   wide_reg_perc$loc<-0
#   wide_reg_acre$loc<-0
#   
# } else {
#   print("loc col there")
# }
# 
# if (is.null(wide_reg_perc$priv)) {
#   wide_reg_perc$priv<-0
#   wide_reg_acre$priv<-0
#   
# } else {
#   print("priv col there")
# }



###need a way to insert these colnames based on threatened/burned running
#ORDER THE COLUMNS ADDED OR EXISTING
# wide_perc_order<-wide_reg_perc[,c("incident_id","fed","loc","priv","state","trib")]
# wide_acre_order<-wide_reg_acre[,c("incident_id","fed","loc","priv","state","trib")]

#pass them in as argument
#colnames(wide_perc_order)<-burn_or_threatcols_perc
#colnames(wide_acre_order)<-burn_or_threatcols_acre

colnames(put_calcs_perc)<-burn_or_threatcols_perc
colnames(put_calcs_acre)<-burn_or_threatcols_acre

perc_area<-merge(put_calcs_perc,put_calcs_acre,by="incident_id",all=TRUE)
return(perc_area)

#return(wide_reg)

}
