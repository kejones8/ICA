#outlines paths and params for running JCA scripts for k8s sample 

###02_JCA_getmtbsids_jcasamp.R
jca_samp_in<-"K8_Chpt\\data\\k8_incids_mtbsids_notmergedwithmtbsfootprintdownload.csv"

select_mtbs_out<-"K8_Chpt\\data\\k8_mtbs_match_jcasamp.shp"
select_mtbs_in<-"K8_Chpt\\data\\k8_mtbs_match_jcasamp.shp"

threat_work_out<-"K8_Chpt\\data\\k8_mtbs_match_jcasamp_threat.shp"

buffer_nodonuts_fortinter_out<-"K8_Chpt\\data\\k8_mtbsbuf_nodonuts.shp"

###03_JCA_prep_intersect_cenplace
#jca_samp_in<- "K8_Chpt\\data\\k8_incids_mtbsids_notmergedwithmtbsfootprintdownload.csv"
count_cenpl_burn_out<-"K8_Chpt\\data\\k8_count_cenpl_burn.csv"
count_cenpl_threat_out<-"K8_Chpt\\data\\k8_count_cenpl_threat.csv"

##04_JCA_prep_surfman
##05_surfman_sumtercorrections.R 

####NOTHING HERE, these only need to run once####



###06_JCA_inter_surfman
#jca_samp_in
#select_mtbs_out
#threat_work_out
#buffer_nodouts_forinter_out
burn_surfman_inter_out<-"K8_Chpt\\data\\k8_burnsurfmaninter2.shp"
burn_juris_byincid_out<-"K8_Chpt\\data\\k8_burned_juris_byincid.csv"
threat_surfman_inter_out<-"K8_Chpt\\data\\k8_threat_surfman_inter2.shp"
threat_juris_byincid_out<-"K8_Chpt\\data\\k8_threat_juris_byincid.csv"
burn_incid_count_stcnty_out<-"K8_Chpt\\data\\k8_burn_incid_nonfed.csv"
threat_incid_count_stcnty_out<-"K8_Chpt\\data\\k8_threat_incid_nonfed.csv"


###07_JCA_surfman_postproc
#burn_juris_byincid_out
#threat_juris_byincid_out
dod_burn_count_out<-"K8_Chpt\\data\\k8_dod_burn_count.csv"
dod_threat_count_out<-"K8_Chpt\\data\\k8_dod_threat_count.csv"
doe_burn_count_out<-"K8_Chpt\\data\\k8_doe_burn_count.csv"
doe_threat_count_out<-"K8_Chpt\\data\\k8_doe_threat_count.csv"
bor_burn_count_out<-"K8_Chpt\\data\\k8_bor_burn_count.csv"
bor_threat_count_out<-"K8_Chpt\\data\\k8_bor_threat_count.csv"
tva_burn_count_out<-"K8_Chpt\\data\\k8_tva_burn_count.csv"
tva_threat_count_out<-"K8_Chpt\\data\\k8_tva_threat_count.csv"
usfws_burn_count_out<-"K8_Chpt\\data\\k8_usfws_burn_count.csv"
usfws_threat_count_out<-"K8_Chpt\\data\\k8_usfws_threat_count.csv"
nps_burn_count_out<-"K8_Chpt\\data\\k8_nps_burn_count.csv"
nps_threat_count_out<-"K8_Chpt\\data\\k8_nps_threat_count.csv"
usfs_burn_count_out<-"K8_Chpt\\data\\k8_usfs_burn_count.csv"
usfs_threat_count_out<-"K8_Chpt\\data\\k8_usfs_threat_count.csv"
othtrib_burn_count_out<-"K8_Chpt\\data\\k8_othtrib_burn_count.csv"
othtrib_threat_count_out<-"K8_Chpt\\data\\k8_othtrib_threat_count.csv"
ancsa_burn_count_out<-"K8_Chpt\\data\\k8_ancsa_burn_count.csv"
ancsa_threat_count_out<-"K8_Chpt\\data\\k8_ancsa_threat_count.csv"

###08_JCA_prep_inter_state_county
#burn_surfman_inter_out
#threat_surfman_inter_out
nonfed_burn_diss_out<-"K8_Chpt\\data\\k8_nonfed_burn_diss.shp"
nonfed_threat_diss_out<-"K8_Chpt\\data\\k8_nonfed_threat_diss.shp"
#jca_samp_in
#select_mtbs_out
#threat_work_out
threat_county_count_out<-"K8_Chpt\\data\\k8_threat_cnty_count.csv"
threat_state_count_out<-"K8_Chpt\\data\\k8_threat_state_count.csv"
burn_county_count_out<-"K8_Chpt\\data\\k8_burn_cnty_count.csv"
burn_state_count_out<-"K8_Chpt\\data\\k8_burn_state_count.csv"

###09_JCA_prep_inter_biablm
#burn_surfman_inter_out
#threat_surfman_inter_out
#select_mtbs_out
#threat_work_out
#jca_samp_in
threat_blm_count_out<-"K8_Chpt\\data\\k8_threat_blm_count.csv"
threat_bia_count_out<-"K8_Chpt\\data\\k8_threat_bia_count.csv"
burn_blm_count_out<-"K8_Chpt\\data\\k8_burn_blm_count.csv"
burn_bia_count_out<-"K8_Chpt\\data\\k8_burn_bia_count.csv"

###10_JCA_prep_inter_gacc
#select_mtbs_in
#threat_work_out
#jca_samp_in
threat_gacc_count_out<-"K8_Chpt\\data\\k8_threat_gacc_count.csv"
burn_gacc_count_out<-"K8_Chpt\\data\\k8_burn_gacc_count.csv"

###11_JCA_merge_clean_finalize_jurs
#jca_samp_in
#usfs_burn_count_out
#usfs_threat_count_out
#nps_burn_count_out
#nps_threat_count_out
#usfws_burn_count_out
#usfws_threat_count_out
#tva_burn_count_out
#tva_threat_count_out
#bor_burn_count_out
#bor_threat_count_out
#doe_burn_count_out
#doe_threat_count_out
#dod_burn_count_out
#dod_threat_count_out
#burn_blm_count_out
#threat_blm_count_out
#othtrib_burn_count_out
#othtrib_threat_count_out
#burn_bia_count_out
#threat_bia_count_out
#burn_state_count_out
#threat_state_count_out
#burn_county_count_out
#threat_county_count_out
#count_cenpl_burn_out
#count_cenpl_threat_out
#burn_gacc_count_out
#threat_gacc_count_out
final_out<-"K8_Chpt\\data\\k8_incid_withjuris_counts.csv"


##12_area_calc_func.R
###Nothing here, just feeder function to 13_

##13_area_calc_prep_fin.R
burn_threat_perc_area_tab_out<-"K8_Chpt\\data\\k8_areas_burn_threat_byjurislevel.csv"

#14_connect_data_incids_mtbs
#select_mtbs_out
connect_mtbs_incids<-"K8_Chpt\\data\\k8_mtbs_incids.shp"
incid_multipolys<-"K8_Chpt\\data\\k8_incids_multipoly.shp"
incid_count_area_mtbs_out<-"K8_Chpt\\data\\k8_incids_mtbs_counts_areas2.gpkg"
incid_count_area_mtbs_out2<-"K8_Chpt\\data\\k8_incids_mtbs_counts_areas_5lev.gpkg"

