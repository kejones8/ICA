#this script is to get the fire footprint data into a mappable format 
#need to connect each incident_id to the mtbs_ids and be able to map attributes for each incident_id
library(sf)
library(dplyr)

#read in mtbs footprints in sample - don't think I need to represent threatened extent spatially
#because all attributes are technically tied to the single incident, which is represented as the burned boudnary
mtbs_insamp<-read_sf(select_mtbs_out)

mtbs_incid_proj<-st_transform(mtbs_insamp,5070)

#read in table that joins mtbs & incids
link_mtbs_incids<-read.csv(jca_samp_in)

#join incids to mtbs
mtbs_incid<-merge(link_mtbs_incids,mtbs_incid_proj,by.x="mtbs_ids",by.y="Event_ID",all.x=TRUE)
dim(mtbs_incid)

write_sf(mtbs_incid,connect_mtbs_incids)

#make geometries representative of whole incident id/all mtbs footprints
incid_polys<-mtbs_incid %>%
  group_by(incident_id) %>% 
  summarize(geometry = st_union(geometry))


#dissolve all mtbs footprints to incident id to join to incident level data
write_sf(incid_polys,incid_multipolys)

#read in the jurisdictional count data 
jur_counts<-read.csv(final_out)

#read in the jurisdictional area estimates 
jur_area<-read.csv(burn_threat_perc_area_tab_out)

#join tabular data, make sure i preserve NAs where needed 
tab_merged<-merge(jur_counts,jur_area,by="incident_id",all=TRUE)

#merge it all together
what_i_need<-merge(incid_polys,tab_merged,by="incident_id")

#explore some stats for mapping 

####not quite sure what to make of possible data transformations. did learn that county adds the most
####but not quite as many as i thought...makes sense cause counties out west are quite large, but
####still need to consider that county and cenpl consistently contribute more jurisdictions


#starting with jur_burned
hist(what_i_need$jur_burned)
hist(log(what_i_need$jur_burned))
hist(sqrt(what_i_need$jur_burned))
hist(what_i_need$jur_burned^(1/3))

#federal jurisdictions burned
hist(what_i_need$fed_burn_cnt)
#state jurisdictions burned
hist(what_i_need$st_burn_count)
#tribal jurisdictions burned
hist(what_i_need$trib_burn_cnt)
#county jurisdictions burned
hist(what_i_need$cnty_burn_count)
#county jurisdictions burned
hist(what_i_need$cenpl_burn_count)


hist(what_i_need$jur_threatened)

#create the column of jurisdictional levels burned 
#if federal burned > 0 count
#if state burned > 0 count
#if trib burned >0 count
#if cenpl/county > 0 count

### was trying to identify across the federal tribal state burned/threat columns to count where the row for that columns was not zero
#don't want to sum the values, just want to tally that it's nonzero
#want to do the same thing for county/cenpl, but across the two columns either or needs to be non-zero
#colSums(what_i_need[c(28,30,32)] != 0)
what_i_need1<-what_i_need %>% mutate(burn_jur_level_fst=rowSums(.[c(28,30,32)]!=0))
what_i_need2<-what_i_need1 %>% mutate(burn_jur_level_cntcen=rowSums(.[c(34,36)]!=0))
what_i_need2$burn_actual_cntcen<-NA
what_i_need2$burn_actual_cntcen[what_i_need2$burn_jur_level_cntcen>0]<-1
what_i_need2$burn_actual_cntcen[is.na(what_i_need2$burn_actual_cntcen)]<-0


what_i_need2$burn_jur_level_cnt<-what_i_need2$burn_jur_level_fst+what_i_need2$burn_actual_cntcen


# do the same thing as burned but also for threatened

what_i_need3<-what_i_need2 %>% mutate(threat_jur_level_fst=rowSums(.[c(29,31,33)]!=0))
what_i_need4<-what_i_need3 %>% mutate(threat_jur_level_cntcen=rowSums(.[c(35,37)]!=0))
what_i_need4$threat_actual_cntcen<-NA
what_i_need4$threat_actual_cntcen[what_i_need4$threat_jur_level_cntcen>0]<-1
what_i_need4$threat_actual_cntcen[is.na(what_i_need4$threat_actual_cntcen)]<-0


what_i_need4$threat_jur_level_cnt<-what_i_need4$threat_jur_level_fst+what_i_need4$threat_actual_cntcen

#add start year back in
getstartyr<-link_mtbs_incids[,c("incident_id","START_YEAR")]
incid_info<-merge(what_i_need4,getstartyr,all=TRUE)

look_at_4lvl_burnthrt<-incid_info[incid_info$threat_jur_level_cnt==4 |incid_info$burn_jur_level_cnt==4,c("incident_id","START_YEAR","burn_jur_level_cnt","threat_jur_level_cnt")]



st_write(incid_info,incid_count_area_mtbs_out)#,append=FALSE)
