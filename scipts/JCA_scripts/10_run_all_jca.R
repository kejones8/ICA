#####ALL OF THESE PATHS SHOULD BE CHANGED TO BE JUST JCA

source("scipts\\JCA_scripts\\00_JCA_paths.R")
#source("scipts\\JCA_scripts\\00_JCA_paths.R")


##scripts run separately prior to running scripts below 
# source("scipts\\JCA_scripts\\02_JCA_sample.R")
# source("scipts\\JCA_scripts\\01_K8_sample.R")

#will require changing everything below to paste0() path creation.....
source("scipts\\JCA_scripts\\01_JCA_sample.R")
source("scipts\\JCA_scripts\\02_JCA_getmtbsids_jcasamp.R")
source("scipts\\JCA_scripts\\03_JCA_prep_intersect_cenplace.R") #ran this at 08:08pm on 06/04
#source("scripts\\JCA_scripts\\04_prep_surfman.R")
#source("scipts\\JCA_scripts\\05_surfman_sumtercorrections.R")
source("scipts\\JCA_scripts\\06_JCA_inter_surfman.R")
source("scipts\\JCA_scripts\\07_JCA_surfman_postproc.R")
source("scipts\\JCA_scripts\\08_JCA_prep_inter_state_county.R")
source("scipts\\JCA_scripts\\09_JCA_prep_inter_biablm.R")
source("scipts\\JCA_scripts\\10_JCA_prep_inter_gacc.R")
source("scipts\\JCA_scripts\\11_JCA_merge_clean_finalize_jurs.R")
source("scipts\\JCA_scripts\\12_area_calc_func.R")
source("scipts\\JCA_scripts\\13_area_calc_prep_fin.R")
source("scipts\\JCA_scripts\\14_connect_data_incids_mtbs.R")
source("scipts\\JCA_scripts\\15_QAQC_juriscounts.R")
source("scipts\\JCA_scripts\\16_prepping_PLdata.R")
source("scipts\\JCA_scripts\\17_attachtoICA.R")







