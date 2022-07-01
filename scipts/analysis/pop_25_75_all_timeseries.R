library(anytime)
library(lubridate)
library(timetk)
library(lamisc)
# library(scales)
library(TTR)

setwd("C:\\Users\\thebrain\\Dropbox\\FireChasers\\JCA_analysis")
      
#read in up to date incident table
incidents<-read.csv("data\\inc_level_table_1002.csv")

#get rid of one anomalous incident that was caused by erroneous 209
#data set to year 1902 instead of 2002
incidents<-incidents[incidents$Days_Type1_2<1000,]

#creating variables that aren't already in the incident table
incidents$JUR_COMP_SCORE<-incidents$TOTAL_JUR_AFF * incidents$JUR_LEVEL_AFF
incidents$VALS_AT_RISK<-incidents$DAMAG_TOTAL+incidents$MAX_THREAT+incidents$DEST_TOTAL
incidents$NPL_DAYS_4_5<-incidents$Days_PL4+incidents$Days_PL5

#want to get the dates into a consistent format for time series
#not used unless want to try summarizing at unit finer than the year
incidents$dd<-anydate(incidents$DISCOVERY_DATE, "%Y-%m")
incidents$rt<-anydate(incidents$start_report_date, "%Y-%m")
incidents$mnth_year_dd<-format(as.Date(incidents$dd, "%Y-%m"))
incidents$dd_split_yr<-sapply(str_split(incidents$mnth_year_dd,"-"),`[`, 1)
incidents$dd_split_mm<-sapply(str_split(incidents$mnth_year_dd,"-"),`[`, 2)
incidents$year_mon<-paste0(incidents$dd_split_yr,"-",incidents$dd_split_mm)

#specify variables we want to plot
var_names<-c("FINAL_ACRES","CNT_FIRE_PERIMS","Days_Type1_2","JUR_COMP_SCORE","VALS_AT_RISK","NPL_DAYS_4_5")

#get the columns of interest
incidents_subset<-incidents[,c("INCIDENT_ID","YEAR","year_mon",var_names)]
incidents_sub_comp<-incidents_subset[complete.cases(incidents_subset),]#do complete cases

#use the specifc timetk:: preprocessing function
incidents_sub_comp_preproc<-na_preprocessing(incidents_sub_comp)

#create the upper and lower quartile of the population
incidents75<-incidents_sub_comp_preproc[incidents_sub_comp_preproc$FINAL_ACRES>quantile(incidents_sub_comp_preproc$FINAL_ACRES)[[4]],]
incidents25<-incidents_sub_comp_preproc[incidents_sub_comp_preproc$FINAL_ACRES<quantile(incidents_sub_comp_preproc$FINAL_ACRES)[[2]],]


#these 2 lists need to be in same order
pop_samples<-c("all_inc","perc25","perc75")
table_list<-list(incidents_sub_comp_preproc,incidents25,incidents75)




for (i in 1:length(table_list)){

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
  
  #loop through all incidents, 25 or 75%
  df<-table_list[[i]]
  
  #specify variable
  var_name<-var_names[x]
  
  df_uni<-unique(df[,c("INCIDENT_ID","YEAR",var_name)])

  #take average of variables by year
  mean_table<- df_uni %>% group_by(YEAR) %>% summarize(mean=mean(.data[[var_name]]))

  #make the title for each plot
  title_to_plot<-paste0(pop_samples[i],"_",var_name)
  
  #plot the regression using the mean of the variable and the year
  #as the temporal unit
 to_print <-mean_table %>%
    # group_by(INCIDENT_ID) %>%
    plot_time_series_regression(
      .date_var    = YEAR,
      .formula     = mean ~ as.numeric(YEAR),# + month(mnth_year_rt),
      #.facet_ncol  = 2,
      .interactive = FALSE,
      .show_summary = TRUE,
      #.title = paste0(pop_samples[i]," : ",var_name),
    )
 
    #get slope from summary table
    slope<-round(to_print$data[4,4]-to_print$data[2,4],4)
    
    #create the file for the plot
    png(filename=paste0("figures\\figs_1025\\",pop_samples[i],"\\",pop_samples[i],"_",var_name,".png"))
    
    #open, plot, and add slope text
    plot.new()
    plot(to_print)
    text(.85,.92,paste0("slope= ",slope),cex=1.25)
    
    dev.off() #close
  

}

}



