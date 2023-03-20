#####ALL OF THESE PATHS SHOULD BE CHANGED TO BE JUST ICA

source("scipts\\JCA_scripts\\00_JCA_paths.R")
#source("scipts\\JCA_scripts\\00_JCA_paths.R")
print("finished_00")


##scripts run separately prior to running scripts below 
# source("scipts\\JCA_scripts\\02_JCA_sample.R")
# source("scipts\\JCA_scripts\\01_K8_sample.R")

#will require changing everything below to paste0() path creation.....
source("scipts\\JCA_scripts\\01_JCA_sample.R")
print("finished_01")
source("scipts\\JCA_scripts\\02_JCA_getmtbsids_jcasamp.R")
print("finished_02")
source("scipts\\JCA_scripts\\03_JCA_prep_intersect_cenplace.R") 
print("finished_03")
#source("scripts\\JCA_scripts\\04_prep_surfman.R")
#source("scipts\\JCA_scripts\\05_surfman_sumtercorrections.R")
source("scipts\\JCA_scripts\\06_JCA_inter_surfman.R")
print("finished_06")

#run here 12/14 
source("scipts\\JCA_scripts\\07_JCA_surfman_postproc.R")
print("finished_07")
source("scipts\\JCA_scripts\\08_JCA_prep_inter_state_county.R")
print("finished_08")
source("scipts\\JCA_scripts\\09_JCA_prep_inter_biablm.R")
print("finished_09")
source("scipts\\JCA_scripts\\10_JCA_prep_inter_gacc.R")
print("finished_10")
source("scipts\\JCA_scripts\\11_JCA_merge_clean_finalize_jurs.R")
print("finished_11")
source("scipts\\JCA_scripts\\12_area_calc_func.R")
print("finished_12")
source("scipts\\JCA_scripts\\13_area_calc_prep_fin.R")
print("finished_13")
source("scipts\\JCA_scripts\\14_connect_data_incids_mtbs.R")
print("finished_14")
source("scipts\\JCA_scripts\\15_QAQC_juriscounts.R")
print("finished_15")
source("scipts\\16_prepping_PLdata.R")
print("finished_16_PL")
source("scipts\\JCA_scripts\\16_attachtoICA.R")
print("finished_16_ICAattach")







