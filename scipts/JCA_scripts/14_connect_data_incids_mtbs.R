#this script is to get the fire footprint data into a mappable format 
#need to connect each incident_id to the mtbs_ids and be able to map attributes for each incident_id
library(sf)
library(dplyr)

#read in mtbs footprints in sample - don't think I need to represent threatened extent spatially
#because all attributes are technically tied to the single incident, which is represented as the burned boudnary
mtbs_insamp<-st_read(select_mtbs_out)

#read in table that joins mtbs & incids
link_mtbs_incids<-read.csv(jca_samp_in)

#join incids to mtbs
mtbs_incid<-merge(link_mtbs_incids,mtbs_insamp,by.x="mtbs_ids",by.y="Event_ID",all.x=TRUE)

#read in the jurisdictional count data 
jur_counts<-read.csv(final_out)

#read in the jurisdictional area estimates 
jur_area<-read.csv(burn_threat_perc_area_tab_out)

#join tabular data, make sure i preserve NAs where needed 
tab_merged<-merge(jur_counts,jur_area,by="incident_id",all=TRUE)

#merge it all together
what_i_need<-merge(mtbs_incid,tab_merged,by="incident_id")

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
# don't want to sum the values, jst want to tally that it's nonzero
#want to do the same thing for county/cenpl, but across the two columns either or needs to be non-zero
colSums(what_i_need[c(28,30,32)] != 0)
what_i_need %>% mutate(jur_level_fst=rowSums(.[c(28,30,32)]!=0))
what_i_need %>% mutate(jur_level_cntcen=rowSums(.[c(34,36)]!=0))
what_i_need$actual_cntcen<-NA
what_i_need$actual_cntcen[what_i_need$jur_level_cntcen>0]<-1


what_i_need$jur_level_burn_cnt<-what_i_need$jur_level_fst+what_i_need$actual_cntcen


# do the same thing as burned but also for threatened


#create the column of jurisdictional levels threatened 
#if federal threat > 0 count
#if state threat > 0 count
#if trib threat >0 count
#if cenpl/county threat > 0 count





write_sf(what_i_need,incid_count_area_mtbs_out)
