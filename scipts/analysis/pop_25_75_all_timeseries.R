library(anytime)
library(lubridate)
#library(timetk)
library(lamisc)
# library(scales)
library(TTR)

#CHANGE TO THE FOLDER ON YOUR COMPUTER WHERE DATA IS STORED AND WHAT TO WRITE PLOTS TO
#setwd("C:\\Users\\thebrain\\Dropbox\\FireChasers\\JCA_analysis")

#read in up to date incident table
#CHANGE TO NEW DATA NAME
incidents<-read.csv("data\\final_ica_jca_tab.csv")



#creating variables that aren't already in the incident table
#CHANGE variables created below if you want any new variables plotted
#confirm that all variables being used to create new variables exist in the "incidents" dataframe
incidents$JUR_COMP_SCORE_BURN<-incidents$alljur_burn_cnt * incidents$total_lev_burn
incidents$JUR_COMP_SCORE_ENG<-incidents$alljur_engag_cnt * incidents$total_lev_engag
incidents$VALS_AT_RISK<-incidents$STR_DAMAGED_TOTAL+incidents$STR_DESTROYED_TOTAL+incidents$STR_THREATENED_MAX
incidents$NPL_DAYS_4_5<-incidents$Days_PL4+incidents$Days_PL5

#want to get the dates into a consistent format for time series
#not used unless want to try summarizing at unit finer than the year
# incidents$dd<-anydate(incidents$DISCOVERY_DATE, "%Y-%m")
# incidents$rt<-anydate(incidents$REPORT_DATE, "%Y-%m")
# incidents$mnth_year_dd<-format(as.Date(incidents$dd, "%Y-%m"))
# incidents$dd_split_yr<-sapply(str_split(incidents$mnth_year_dd,"-"),`[`, 1)
# incidents$dd_split_mm<-sapply(str_split(incidents$mnth_year_dd,"-"),`[`, 2)
# incidents$year_mon<-paste0(incidents$dd_split_yr,"-",incidents$dd_split_mm)

#specify variables we want to plot
#var_names<-c("FINAL_ACRES","CNT_FIRE_PERIMS","Days_Type1_2","JUR_COMP_SCORE","VALS_AT_RISK","NPL_DAYS_4_5")
#CHANGE $DAYS_Type1_2 -> $duration...not the same, but we don't have a Days_Type1_2 column anymore
#var_names<-c("FINAL_ACRES","Days_Type1_2","JUR_COMP_SCORE","VALS_AT_RISK","NPL_DAYS_4_5")
var_names<-c("FINAL_ACRES","JUR_COMP_SCORE_BURN","JUR_COMP_SCORE_ENG","VALS_AT_RISK","NPL_DAYS_4_5")


#get the columns of interest
#CHANGE  to $START_YEAR
incidents_subset<-incidents[,c("INCIDENT_ID","START_YEAR.x",var_names)]


#create the upper and lower quartile of the population
incidents75<-incidents_subset[incidents_subset$FINAL_ACRES>quantile(incidents_subset$FINAL_ACRES)[[4]],]
incidents25<-incidents_subset[incidents_subset$FINAL_ACRES<quantile(incidents_subset$FINAL_ACRES)[[2]],]


#these 2 lists need to be in same order
pop_samples<-c("all_inc","BottomQuartile","TopQuartile")
table_list<-list(incidents_subset,incidents25,incidents75)

samp_var_slopes<-data.frame(matrix(NA, nrow = 15, ncol = 3))
colnames(samp_var_slopes)<-c("sample","variable","slope")

count<-0
for (i in 1:length(table_list)){
  
  #loop through all incidents, 25 or 75%
  df<-table_list[[i]]
  samp<-pop_samples[[i]]
  
  #   #### If you want to delete existing plots & rerun #####
  
  # unlink(paste0("figures\\",pop_samples[i]), recursive = TRUE)
  # unlink(paste0("figures\\",pop_samples[i]), recursive = TRUE)
  # unlink(paste0("figures\\",pop_samples[i]), recursive = TRUE)
  # 
  # dir.create(paste0("figures\\",pop_samples[i]))
  # dir.create(paste0("figures\\",pop_samples[i]))
  # dir.create(paste0("figures\\",pop_samples[i]))
  
  
  #step through the variables
  for (x in 1:length(var_names)) {
    
    count<-count+1
    # df<-table_list[[i]]
    # samp<-pop_samples[[i]]
    
    #specify variable
    var_name<-var_names[x]
    
    #CHANGE $START_YEAR
    df_uni_bleh<-unique(df[,c("INCIDENT_ID","START_YEAR.x",var_name)])
    df_uni<-df_uni_bleh[complete.cases(df_uni_bleh),]#do complete cases
    
    #take average of variables by year
    mean_table<- df_uni %>% group_by(START_YEAR.x) %>% summarize(mean=mean(.data[[var_name]]))
    
    #make the title for each plot
    #  title_to_plot<-paste0(pop_samples[i],"_",var_name)
    #  
    #  #plot the regression using the mean of the variable and the year
    #  #as the temporal unit
    # to_print <-mean_table %>%
    #    # group_by(INCIDENT_ID) %>%
    #    plot_time_series_regression(
    #      .date_var    = YEAR,
    #      .formula     = mean ~ as.numeric(YEAR),# + month(mnth_year_rt),
    #      #.facet_ncol  = 2,
    #      .interactive = FALSE,
    #      .show_summary = TRUE,
    #      #.title = paste0(pop_samples[i]," : ",var_name),
    #    )
    # 
    #    #get slope from summary table
    #    slope<-round(to_print$data[4,4]-to_print$data[2,4],4)
    #    
    #    #create the file for the plot
    #   
    #    png(filename=paste0("figures\\",pop_samples[i],"\\",pop_samples[i],"_",var_name,".png"))
    #    
    #    #open, plot, and add slope text
    #    plot.new()
    #    plot(to_print)
    #    text(.85,.92,paste0("slope= ",slope),cex=1.25)
    #    
    #    dev.off() #close
    
    #mean_table<-df_to_plot %>% group_by(START_YEAR.x) %>% dplyr::summarize(mean=mean(.data[[var_name]]))
    
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
    #abline(lm(lm))
    
    #get the slope from the summary statistics
    #slope<-round(to_print$data[4,4]-to_print$data[2,4],4)
    slope<-lm[1][[1]][[2]]
    
    #file for writing plot to
    png(filename=paste0("data\\analysis\\figures\\by_dataquartiles_firesize\\",samp,"_",var_name,".png"))
    
    #open plot, plot the plot, then write the slop text overtop
    plot.new()
    plot(mean_table$START_YEAR.x, mean_table$mean, pch = 16, cex = 1, col = "black", main = paste0(samp," : ",var_name), xlab = "YEAR", ylab = var_name,xlim=c(1999,2018))
    abline(lm(lm))
    mtext(paste0("slope= ",slope), side=3)
    #text(.85,.92,paste0("slope= ",slope),cex=1.25)
    
    dev.off() #erase that plot from memort
    
    #make a row for teh dataframe to write out and connect to shapefile
    new_row<-c(samp,var_name,slope)
    
    #append it
    samp_var_slopes[count,]<-new_row
    
    # 
    # #loop through all incidents, 25 or 75%
    # df<-table_list[[i]]
    # 
    # #specify variable
    # var_name<-var_names[x]
    # 
    # #CHANGE $START_YEAR
    # df_uni<-unique(df[,c("INCIDENT_ID","YEAR",var_name)])
    # 
    # df_uni<-df_uni[complete.cases(df_uni),]#do complete cases
    # 
    # #use the specifc timetk:: preprocessing function
    # #incidents_sub_comp_preproc<-na_preprocessing(incidents_sub_comp)
    # 
    # #take average of variables by year
    # mean_table<- df_uni %>% group_by(YEAR) %>% summarize(mean=mean(.data[[var_name]]))
    # 
    # #make the title for each plot
    # title_to_plot<-paste0(pop_samples[i],"_",var_name)
    # 
    # #plot the regression using the mean of the variable and the year
    # #as the temporal unit
    # to_print <-mean_table %>%
    #   # group_by(INCIDENT_ID) %>%
    #   plot_time_series_regression(
    #     .date_var    = YEAR,
    #     .formula     = mean ~ as.numeric(YEAR),# + month(mnth_year_rt),
    #     #.facet_ncol  = 2,
    #     .interactive = FALSE,
    #     .show_summary = TRUE,
    #     #.title = paste0(pop_samples[i]," : ",var_name),
    #   )
    # 
    # #get slope from summary table
    # slope<-round(to_print$data[4,4]-to_print$data[2,4],4)
    # 
    # #create the file for the plot
    # 
    # png(filename=paste0("figures\\",pop_samples[i],"\\",pop_samples[i],"_",var_name,".png"))
    # 
    # #open, plot, and add slope text
    # plot.new()
    # plot(to_print)
    # text(.85,.92,paste0("slope= ",slope),cex=1.25)
    # 
    # dev.off() #close
    
    
  }
  
}

write.csv(samp_var_slopes,"data\\analysis\\figures\\by_dataquartiles_firesize\\samp_vars_slope.csv")

