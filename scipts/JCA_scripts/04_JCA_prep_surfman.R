#this script intersects the mtbs footprints with the surface management dataset
#this is where a good deal of the jurisdictional information comes from

library(sf)
library(foreach) #for parallelizing intersection
library(doParallel)
library(dplyr)

#this line should pull from the repo, but don't want to copy the dataset again onto my drive
st_layers("C:\\Users\\thebrain\\Dropbox\\FireChasers\\new\\raw_data\\JurisdictionalAgencies\\WFDSS_Jurisdictional_Agency.gdb")
#would normally need to specify layer reading directly from gdb, but only one layer in here
surf_man<-read_sf("C:\\Users\\thebrain\\Dropbox\\FireChasers\\new\\raw_data\\JurisdictionalAgencies\\WFDSS_Jurisdictional_Agency.gdb")

#project, cause always
surf_man_alb<-st_transform(surf_man, 5070)

#because i was having problems with polygon operations, getting to the bottom of it
as.data.frame(table(st_geometry_type(surf_man_alb)))
#want everything to be multipolygon, not multisurface, so cast
surf_man_multi <- st_cast(surf_man_alb, "MULTIPOLYGON")

#zero buffer takes awhile, so writing it out afterwards
surfman_buf<-st_buffer(surf_man_multi,0)
write_sf(surfman_buf,"data\\JCA\\surfman_zerobuf.shp")


#checked this output in qgis everything was valid, no errors
#used the output of this script to create a new surfman layer that has francis marion & sumter NF assigned correctly


