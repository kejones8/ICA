#for use with kate's data
library(dplyr)

source("K8_Chpt\\scripts\\00_K8s_paths.R")
#source("scipts\\JCA_scripts\\00_JCA_paths.R")


##scripts run separately prior to running scripts below 
# source("scipts\\JCA_scripts\\02_JCA_sample.R")
# source("scipts\\JCA_scripts\\01_K8_sample.R")

#will require changing everything below to paste0() path creation.....
source("scipts\\JCA_scripts\\02_JCA_sample.R")
source("scipts\\JCA_scripts\\03_JCA_getmtbsids_jcasamp.R")
source("scipts\\JCA_scripts\\04_JCA_prep_intersect_cenplace.R") #ran this at 08:08pm on 06/04


source("scipts\\JCA_scripts\\05b_JCA_inter_surfman.R")
source("scipts\\JCA_scripts\\05c_JCA_surfman_postproc.R")
source("scipts\\JCA_scripts\\06_JCA_prep_inter_state_county.R")
source("scipts\\JCA_scripts\\07_JCA_prep_inter_biablm.R")
source("scipts\\JCA_scripts\\08_JCA_prep_inter_gacc.R")


source("scipts\\JCA_scripts\\09_JCA_merge_clean_finalize_jurs.R")
