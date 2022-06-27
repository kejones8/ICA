library(sf)

incid_counts<-read_sf(incid_count_area_mtbs_out2)

nodups_nas<-incid_counts[!is.na(incid_counts$fed_burn_cnt),]

fed<-range(nodups_nas$fed_burn_cnt)
state<-range(nodups_nas$st_burn_count)
trib<-range(nodups_nas$trib_burn_cnt)
cnty<-range(nodups_nas$cnty_burn_count)
cenpl<-range(nodups_nas$cenpl_burn_count)


which_high_fedfires<-nodups_nas[nodups_nas$fed_burn_cnt==range(nodups_nas$fed_burn_cnt)[2],]
which_high_stfires<-nodups_nas[nodups_nas$st_burn_count==range(nodups_nas$st_burn_count)[2],]
which_high_tribfires<-nodups_nas[nodups_nas$trib_burn_cnt==range(nodups_nas$trib_burn_cnt)[2],]
which_high_cntyfires<-nodups_nas[nodups_nas$cnty_burn_count==range(nodups_nas$cnty_burn_count)[2],]
which_high_cenplfires<-nodups_nas[nodups_nas$cenpl_burn_count==range(nodups_nas$cenpl_burn_count)[2],]

toinvestigate<-rbind(which_high_fedfires,which_high_stfires,which_high_tribfires,which_high_cntyfires,which_high_cenplfires)
toinvestigate$geom<-NULL
df<-as.data.frame(toinvestigate)
write.csv(df,"data\\JCA\\incidents_toJCA_QAQC.csv")


 

