#I have NOT made any changes to this script from how it used to run (October 2021), but I have tried 
#to comment where I think column names will need to change, etc.
#I left them so the script would run with the old table so you could get an idea of outputs
#and then only make the modifications as needed.


#these are the packages required
#if you don't have one, you should just install it (can be done through the interface)
#or saying: install.packages("dplyr")

library(dplyr)
library(data.table)
library(ggplot2)
library(anytime)
library(remotes)
#install_version("timetk", version = "2.8.0", repos = "http://cran.us.r-project.org")
#install_version("hardhat", version = "0.1.6", repos = "http://cran.us.r-project.org")
#library(timetk)
library(lamisc)
library(TTR)
library(sf)

##read in the incident table
incidents_raw<-read.csv("data\\final_ica_jca_tab.csv")

##I believe $Days_Type1_2 is now $duration in the new table
#incidents2<-incidents_raw[incidents_raw$Days_Type1_2<1000,]

##this line shouldn't be needed - $JUR_LEVEL_AFF is now $total_lev_burn
#incidents<-incidents2[!is.na(incidents2$JUR_LEVEL_AFF), ]

##no longer need this
# incidents_no14<-incidents[incidents$YEAR!=2014,]
# incidents_14<-incidents[incidents$YEAR==2014,]
# incidents_14$name<-sapply(strsplit(incidents_14$INCIDENT_ID, "_"), `[`, 3)

#no longer necessary because of new data
#short_1992_2018<-fread("C:\\Users\\thebrain\\Dropbox\\FireChasers\\new\\raw_data\\short_1992_2018.csv")


####NEED TO FIGURE OUT WHAT CODE ASSIGNED GACCS to INCIDENTS previously :/
#code below had to break out 2014 separately, so can ignore line below where this happens


#want to attach gacc back to all incidents
#want_aff_gaccs_0926<-read.csv("data\\affected_jur_0926.csv")
gacc_info<-read.csv("data\\JCA\\incident_gaccburned.csv")

# #sort by 2014 to avoid the incident_id issue
# want_aff_gaccs_0926_14<-want_aff_gaccs_0926[want_aff_gaccs_0926$YEAR==2014,]
# want_aff_gaccs_0926_no14<-want_aff_gaccs_0926[want_aff_gaccs_0926$YEAR!=2014,]

## no longer necessary - create name column to merge by
# aff_gaccs_0926_14<-want_aff_gaccs_0926_14[,c("INCIDENT_ID","GACC_NA")]
# aff_gaccs_0926_14$name<-sapply(strsplit(aff_gaccs_0926_14$INCIDENT_ID, "_"), `[`, 3)


#aff_gaccs_0926_14_select<-aff_gaccs_0926_14[,c("INCIDENT_ID","GACC_NA")]#,"name")]


# #merge 2014 & non-2014 incidents with gacc info
# incidents_multigacc_no2014<-merge(incidents_no14,want_aff_gaccs_0926_no14,by="INCIDENT_ID",all.x=TRUE)
# incidents_multigacc_2014<-merge(incidents_14,aff_gaccs_0926_14_select,by="name",all.x=TRUE)

#fix remaining column names so they can be rbinded together into a dataframe
# incidents_multigacc_2014$INCIDENT_ID<-incidents_multigacc_2014$INCIDENT_ID.x
# inc_2014_select<-incidents_multigacc_2014[,c("INCIDENT_ID","FINAL_ACRES","DAMAG_TOTAL","DEST_TOTAL","MAX_THREAT","CNT_FIRE_PERIMS","Days_PL4","Days_PL5","Days_Type1_2","YEAR","JUR_LEVEL_AFF","TOTAL_JUR_AFF","GACC_NA")]
# inc_no2014_select<-incidents_multigacc_no2014[,c("INCIDENT_ID","FINAL_ACRES.x","DAMAG_TOTAL.x","DEST_TOTAL.x","MAX_THREAT.x","CNT_FIRE_PERIMS","Days_PL4","Days_PL5","Days_Type1_2","YEAR.y","JUR_LEVEL_AFF","TOTAL_JUR_AFF","GACC_NA")]
# 
# to_name<-c("INCIDENT_ID","FINAL_ACRES","DAMAG_TOTAL","DEST_TOTAL","MAX_THREAT","CNT_FIRE_PERIMS","Days_PL4","Days_PL5","Days_Type1_2","YEAR","JUR_LEVEL_AFF","TOTAL_JUR_AFF","GACC_NA")
# 
# colnames(inc_no2014_select)<-to_name
# 
# # boopboop<-incidents_multigacc_2014[,colnames(incidents_multigacc_no2014)]
# 
# #after selecting columns from both and naming htem correctly in order
# #then rbind
# 
# 
# #create final df for working with
# incidents_multigacc_v2<-rbind(inc_2014_select,inc_no2014_select)

#create additional variables we want to plot
# incidents_multigacc_v2$JUR_COMP_SCORE<-incidents_multigacc_v2$TOTAL_JUR_AFF * incidents_multigacc_v2$JUR_LEVEL_AFF
# incidents_multigacc_v2$VALS_AT_RISK<-incidents_multigacc_v2$DAMAG_TOTAL+incidents_multigacc_v2$MAX_THREAT+incidents_multigacc_v2$DEST_TOTAL
# incidents_multigacc_v2$NPL_DAYS_4_5<-incidents_multigacc_v2$Days_PL4+incidents_multigacc_v2$Days_PL5

incidents_raw$JUR_COMP_SCORE_BURN<-incidents_raw$alljur_burn_cnt * incidents_raw$total_lev_burn
incidents_raw$JUR_COMP_SCORE_ENG<-incidents_raw$alljur_engag_cnt * incidents_raw$total_lev_engag
incidents_raw$VALS_AT_RISK<-incidents_raw$STR_DAMAGED_TOTAL+incidents_raw$STR_DESTROYED_TOTAL+incidents_raw$STR_THREATENED_MAX
incidents_raw$NPL_DAYS_4_5<-incidents_raw$Days_PL4+incidents_raw$Days_PL5

#want to get the dates into a consistent format 
# incidents_multigacc_v2$dd<-anydate(incidents_multigacc_v2$DISCOVERY_DATE, "%Y-%m")
# incidents_multigacc_v2$rt<-anydate(incidents_multigacc_v2$start_report_date, "%Y-%m")
# incidents_multigacc_v2$mnth_year_dd<-format(as.Date(incidents_multigacc_v2$dd, "%Y-%m"))
# incidents_multigacc_v2$mnth_year_rt<-format(as.Date(incidents_multigacc_v2$rt, "%Y-%m"))

#variables to extract from the dataframe

#var_names<-c("FINAL_ACRES","CNT_FIRE_PERIMS","Days_Type1_2","JUR_COMP_SCORE_BURN","JUR_COMP_SCORE_ENG","VALS_AT_RISK","NPL_DAYS_4_5")
#ADD DAYS_TYPE1-2 back in when I creat it
var_names<-c("FINAL_ACRES","JUR_COMP_SCORE_BURN","JUR_COMP_SCORE_ENG","VALS_AT_RISK","NPL_DAYS_4_5")

inc_dups_bygacc<-merge(incidents_raw,gacc_info,by.x="INCIDENT_ID",by.y="incident_id",all.y=TRUE)


#select those variable columns
inc_dups_bygacc_sub<-inc_dups_bygacc[,c("INCIDENT_ID","START_YEAR.x","GACCAbbrev",var_names)]
#"mnth_year_dd","mnth_year_rt",

#do complete cases to get rid of NA's
#inc_dups_bygacc_sub_comp<-unique(inc_dups_bygacc_sub[complete.cases(inc_dups_bygacc_sub),])

#use the specifc timetk:: preprocessing function
#inc_dups_bygacc_sub_comp_preproc<-na_preprocessing(inc_dups_bygacc_sub_comp)

write.csv(inc_dups_bygacc_sub,"data\\analysis\\incidents_withgacc.csv")

#group dataframe by GACC
grouped<-inc_dups_bygacc_sub %>% 
 split(f = as.factor(.$GACCAbbrev))

#get a list of gacc names to use in file output naming
gacc_names<-names(grouped)


#### If you want to delete existing plots & rerun #####

# #unlink(paste0("figures\\gacc_1005\\CNT_FIRE_PERIMS"), recursive = TRUE)
# unlink(paste0("data\\analyis\\figures\\reg_bygacc\\VALS_AT_RISK"), recursive = TRUE)
# unlink(paste0("data\\analyis\\figures\\reg_bygacc\\FINAL_ACRES"), recursive = TRUE)
# unlink(paste0("data\\analyis\\figures\\reg_bygacc\\JUR_COMP_SCORE_BURN"), recursive = TRUE)
# unlink(paste0("data\\analyis\\figures\\reg_bygacc\\JUR_COMP_SCORE_ENG"), recursive = TRUE)
# #unlink(paste0("figures\\bygacc\\Days_Type1_2"), recursive = TRUE)
# unlink(paste0("data\\analyis\\figures\\reg_bygacc\\NPL_DAYS_4_5"), recursive = TRUE)
# 
# #dir.create(paste0("figures\\gacc_1005\\CNT_FIRE_PERIMS"))
# dir.create(paste0("data\\analyis\\figures\\reg_bygacc\\VALS_AT_RISK"))
# dir.create(paste0("data\\analyis\\figures\\reg_bygacc\\FINAL_ACRES"))
# dir.create(paste0("data\\analyis\\figures\\reg_bygacc\\JUR_COMP_SCORE_BURN"))
# dir.create(paste0("data\\analyis\\figures\\reg_bygacc\\JUR_COMP_SCORE_ENG"))
# #dir.create(paste0("figures\\bygacc\\Days_Type1_2"))
# dir.create(paste0("data\\analyis\\figures\\reg_bygacc\\NPL_DAYS_4_5"))

######

#make a dataframe to output slopes from the loop & name the columns
gacc_var_slopes<-data.frame(matrix(ncol = 3, nrow = 0))
colnames(gacc_var_slopes)<-c("GACC","variable","slope")

#set counter 
count=0

###plot all gaccs, all variables
for (i in 1:length(gacc_names)){
  
  #define which gacc we're working with
  gacc<-grouped[[i]]
  #gacc<-unique(gacc_bleh[complete.cases(gacc_bleh),])
  gacc_name<-gacc_names[i]
  
  #loop through each variable name to produce plot
  for (i in 1:length(var_names)){
    count=count+1
    
    #define variable
    var_name<-var_names[i]

    #select the columns to pass to timeseries regression plot
    df_to_plot_bleh<-unique(gacc[,c("INCIDENT_ID","START_YEAR.x",var_name)])
    df_to_plot<-unique(df_to_plot_bleh[complete.cases(df_to_plot_bleh),])
    
    #rework those columns to return mean values of the variable
    mean_table<-df_to_plot %>% group_by(START_YEAR.x) %>% dplyr::summarize(mean=mean(.data[[var_name]]))
    
    #plot regressions summarizing average of variables per year
    # to_print<-mean_table %>%
    #   # group_by(INCIDENT_ID) %>%
    #   plot_time_series_regression(
    #     .date_var    = START_YEAR.x,
    #     .formula     = mean ~ as.numeric(START_YEAR.x),# + month(mnth_year_rt, label = TRUE),
    #     #.facet_ncol  = 2,
    #     .interactive = FALSE,
    #     .show_summary = TRUE,
    #     .title = paste0(gacc_name," : ",var_name),
    #   )
    #ploop<-plot(mean_table$START_YEAR.x, mean_table$mean, pch = 16, cex = 1, col = "black", main = paste0(gacc_name," : ",var_name), xlab = "YEAR", ylab = var_name)
    options(scipen=999)
    lm<-lm(mean_table$mean ~ mean_table$START_YEAR.x)
    abline(lm(lm))
    
    #get the slope from the summary statistics
    #slope<-round(to_print$data[4,4]-to_print$data[2,4],4)
    slope<-lm[1][[1]][[2]]
    
    #file for writing plot to
    png(filename=paste0("data\\analysis\\figures\\reg_bygacc\\",var_name,"\\",gacc_name,"_",var_name,".png"))
    
    #open plot, plot the plot, then write the slop text overtop
    plot.new()
    plot(mean_table$START_YEAR.x, mean_table$mean, pch = 16, cex = 1, col = "black", main = paste0(gacc_name," : ",var_name), xlab = "YEAR", ylab = var_name,xlim=c(1998,2018))
    abline(lm(lm))
    mtext(paste0("slope= ",slope), side=3)
    #text(.85,.92,paste0("slope= ",slope),cex=1.25)

    dev.off() #erase that plot from memort
      
    #make a row for teh dataframe to write out and connect to shapefile
    new_row<-c(gacc_name,var_name,slope)
    
    #append it
    gacc_var_slopes[count,]<-new_row
    
  # linmod<-ggplot(df_to_plot, aes(x = YEAR, y = .data[[var_name]])) + 
  #   geom_point() +
  #   stat_smooth(method = "lm", col = "red")+
  #   ggtitle(paste0(var_name,":",gacc_name))
  

  # 
  # png(filename=paste0("figures\\gacc\\",var_name,"\\",gacc_name,"_",var_name,".png"))
  # 
  # plot(to_print)
  # 
  # dev.off()
  }
}

#read in gacc shapefile
shape_of_gaccs<-read_sf("data\\National_GACC_Boundaries-shp\\National_GACC_Current_20200226.shp")

#attach the slope information to the gacc shapefile
merged_gacc<-merge(shape_of_gaccs,gacc_var_slopes,by.x="GACCAbbrev",by.y="GACC")

#create individual shapefiles to make separate maps
#not necessary, but quick and dirty for working 
grouped_shapefile<-merged_gacc %>% 
  split(f = as.factor(.$variable))

#make separate shapefile names
shapefile_names<-names(grouped_shapefile)

#write them out for map creation in qgis
#for (i in 1:length(grouped_shapefile)){
  for (i in 1:length(grouped_shapefile)){
  #write.csv(grouped_shapefile[[i]],paste0("data\\analysis\\",shapefile_names[i],"_incidents.csv"))
  st_write(grouped_shapefile[[i]],paste0("data\\analysis\\figures\\reg_bygacc\\",shapefile_names[i],".shp"),append=FALSE)
}















































# 
# 
# #### at this point, I realized there are indicidents that don't have information - date, mtbs, etc. in the final incident table ####
# #### it is mostly earlier incidents by st. denis that don't match MTBS-wise
# #### except for the 2014 incidents, they had the correct ids - not sure what went wrong
# #### so, i started going back through the table creation, and I realized some of my original shapefiles & csvs were
# #### corrupted, so i reran the initial intersections to get affected/threatened
# #### seeing what i need to from there - currently missing a lot of 2014 data, BUT maybe for the variables 
# #### we need I could hack 2014 stuff w/ affected_0715 table? we'll see. 
# 
# 
# #for now, working on the code
# 
# linmod<-ggplot(incidents, aes(x = YEAR, y = FINAL_ACRES)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")+
#   ggtitle("Incidents by FINAL_ACRES")
# 
# linmod
# 
# incidents75<-incidents[incidents$FINAL_ACRES>quantile(incidents$FINAL_ACRES)[[4]],]
# 
# linmod75<-ggplot(incidents75, aes(x = YEAR, y = FINAL_ACRES)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")+
#   ggtitle("75% (by size) FINAL_ACRES")
# 
# linmod75
# 
# incidents25<-incidents[incidents$FINAL_ACRES<quantile(incidents$FINAL_ACRES)[[2]],]
# 
# linmod25<-ggplot(incidents25, aes(x = YEAR, y = FINAL_ACRES)) + 
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")+
#   ggtitle("25% (by size) FINAL_ACRES")
# 
# linmod25
# 
# incjur_preproc %>%
#   # group_by(INCIDENT_ID) %>%
#   plot_time_series_regression(
#     .date_var    = mnth_year_rt,
#     .formula     = norm_jur ~ as.numeric(mnth_year_rt) + month(mnth_year_rt, label = TRUE),
#     #.facet_ncol  = 2,
#     .interactive = FALSE
#   )
# 
# 
# #want to get the dates into a consistent format 
# incidents25$dd<-anydate(incidents25$DISCOVERY_DATE, "%Y-%m")
# incidents25$rt<-anydate(incidents25$start_report_date, "%Y-%m")
# incidents25$mnth_year_dd<-format(as.Date(incidents25$dd, "%Y-%m"))
# incidents25$mnth_year_rt<-format(as.Date(incidents25$rt, "%Y-%m"))
# 
# incidents25_sub<-incidents25[,c("mnth_year_dd","mnth_year_rt","INCIDENT_ID","FINAL_ACRES")]
# 
# 
# #do complete cases
# incidents25_sub_comp<-incidents25_sub[complete.cases(incidents25_sub),]
# 
# #use the specifc timetk:: preprocessing function
# incidents25_sub_comp_preproc<-na_preprocessing(incidents25_sub_comp)
# 
# ts_25incident_finalac<-incidents25_sub_comp_preproc %>%
#   # group_by(INCIDENT_ID) %>%
#   plot_time_series_regression(
#     .date_var    = mnth_year_rt,
#     .formula     = FINAL_ACRES ~ as.numeric(mnth_year_rt) + month(mnth_year_rt),
#     #.facet_ncol  = 2,
#     .interactive = FALSE
#   )
# 
# ts_25incident_finalac
# 
# 
# 
# 
# #want to get the dates into a consistent format 
# incidents$dd<-anydate(incidents$DISCOVERY_DATE, "%Y-%m")
# incidents$rt<-anydate(incidents$start_report_date, "%Y-%m")
# incidents$mnth_year_dd<-format(as.Date(incidents$dd, "%Y-%m"))
# incidents$mnth_year_rt<-format(as.Date(incidents$rt, "%Y-%m"))
# 
# incidents_sub<-incidents[,c("mnth_year_dd","mnth_year_rt","INCIDENT_ID","FINAL_ACRES")]
# 
# 
# #do complete cases
# incidents_sub_comp<-incidents_sub[complete.cases(incidents_sub),]
# 
# #use the specifc timetk:: preprocessing function
# incidents_sub_comp_preproc<-na_preprocessing(incidents_sub_comp)
# 
# ts_incident_finalac<-incidents_sub_comp_preproc %>%
#   # group_by(INCIDENT_ID) %>%
#   plot_time_series_regression(
#     .date_var    = mnth_year_rt,
#     .formula     = FINAL_ACRES ~ as.numeric(mnth_year_rt) + month(mnth_year_rt),
#     #.facet_ncol  = 2,
#     .interactive = FALSE
#   )
# 
# ts_incident_finalac
# 
# 
# 
# 
# 
# inc_year_ts<-ts(inc_year$mean_size, frequency= 1, start=c(1999))
# 
# library(TTR)
# inc_year_ts
# plot.ts(inc_year_ts)#time series plot
# 
# inc_year_tsSMA3<-SMA(inc_year_ts,n=3)#this code smooths the trend line by using a moving average of order 3
# plot.ts(inc_year_tsSMA3)
# 
# 



