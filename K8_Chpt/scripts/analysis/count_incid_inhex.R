#this script make a hexgrid of several different resolutions and counts the number of fire footpirnts 
library(sf)
library(dplyr)
library(hkdatasets)
library(mapdata)
library(tmap)
library(tidyr)
source("K8_Chpt\\scripts\\analysis\\hatch_func.R")



#for now, read in just mtbs footprint data
incid_polys<-read_sf(incid_multipolys)
incid_polys_proj<-st_transform(incid_polys,5070)
#colnames(incid_polys_proj)[1]<-"incident_id"
#add start year
#read in table that joins mtbs & incids
link_mtbs_incids<-read.csv(jca_samp_in)
incid_start_yr<-link_mtbs_incids[,c("incident_id","START_YEAR")]

#read in count data
#counts<-as.data.frame(st_read(incid_count_area_mtbs_out))

#counts$geom<-NULL

#join incident data to count data
#incid_data<-merge(incid_polys_proj,counts,by="incident_id")


#create hexgrid of several different resolutions that covers the extent of the incident polygons 
####NEED TO RENAME THIS!!

extent = st_sfc(st_polygon(list(rbind(c(-2937836.32,367158.13), c(2412507.85,367158.13), c(2412507.85,3278304.94), c(-2937836.32,3278304.94),c(-2937836.32,367158.13)))))
st_crs(extent)<-st_crs(incid_polys_proj)

#create extent/res of hexes
#test_hex_100k <-st_make_grid(extent, c(100000,100000), what = "polygons", square = FALSE, flat_topped=TRUE)
test_hex_50k <- st_make_grid(extent, c(50000,50000), what = "polygons", square = FALSE, flat_topped=TRUE)
#test_hex_50k_proj<-st_transform(test_hex_50k,5070)
#st_crs(test_hex_50k)<-st_crs(incid_polys_proj)
st_crs(test_hex_50k)<-st_crs(incid_polys_proj)

# To sf and add grid ID
#honeycomb_grid_sf = st_sf(test_hex_50k) %>%
honeycomb_grid_sf = st_sf(test_hex_50k) %>%
  
  # add grid ID
  mutate(grid_id = 1:length(lengths(test_hex_50k)))

# count number of burned jurs in each cell
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
#this was previously just counting geometries that intersected each hex, now we want to extract
# an attribute (some type of count of jurisdictions) related to each geom
#so, for each geom that intersects the grid cell, go get the associated count data and sum/max/whatever operation for that hex

# honeycomb_grid_sf$indicator<-st_intersects(honeycomb_grid_sf, incid_data) %>% lengths>0
# honeycomb_dointersect<-honeycomb_grid_sf[honeycomb_grid_sf$indicator==TRUE,]
# 
# 
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# hex_tointer<-unique(honeycomb_dointersect$grid_id)
# 
# #for every burned area mtbs footprint, intersect with surface management 
# #write out combined sf object with all intersections
# hex_intersected<-foreach(i=hex_tointer, .combine = rbind, .packages=c('sf')) %dopar%  {
#   
#   sing_hex<-honeycomb_dointersect[honeycomb_dointersect$grid_id==i,]
#   hex_withincidat<-st_intersection(sing_hex,incid_data)#5 miles = 8047 meters
#   
# }
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm
# 
# #writes out a shapefile with all mtbs footprints and the surf management polygons they intersect
# write_sf(hex_intersected,"K8_Chpt\\data_figures\\hex_withincidata.shp",overwrite=TRUE)
# 

#from geoms_int_hex, get which incid ids intersect each hex - think was is currently reported in geoms_int_hex 
#is actually row number
#go back and look at code from surface managment intersect....?
#what format did i convert the st_intersects object to?


honeycomb_grid_sf$counts = lengths(st_intersects(honeycomb_grid_sf, incid_polys_proj))

#now have which incidens intersect each hex, figure out what & how we want to count for juris mapping

#honeycomb_grid_sf$counts <- 
#need to get where hex_intersected only shows once incident id for every unique incident per grid cell
# uni_hex_inter<-hex_intersected[!duplicated(hex_intersected[ , c("incident_id", "grid_id")]), ]
# uni_hex_inter<-uni_hex_inter %>% group_by(grid_id) %>% mutate(burn_threat_count=jur_burned+jur_threatened)
# hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jur_burn_threat=sum(burn_threat_count))
# 

# remove grid without value of 0 (i.e. no points in side that grid)
honeycomb_count = filter(honeycomb_grid_sf, counts > 0)

write_sf(honeycomb_count,"K8_Chpt\\data_figures\\honeycomb_count_test_50k.shp")



tmap_mode("view")

map_honeycomb = tm_shape(honeycomb_count) +
  tm_fill(
    col = "counts",
    palette = "Reds",
    style = "cont",
    title = "Number of Incidents",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Count of : " = "counts"
    ),
    popup.format = list(
      counts = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.3)

map_honeycomb

###by hexgrid, count intersecting incidents grouped by year 
#for each hex, I will have 18 counts
#then fit a simple linear regression
#check results
#then time-series regression
#check results

honeycomb_grid_sf$counts<-NULL

polys_year<-merge(incid_polys_proj,incid_start_yr,by.x="incdnt_",by.y="incident_id")

yrs<-c(1999:2018)

put_counts_here<-data.frame()

for (i in yrs){
  
  poly<-polys_year[polys_year$START_YEAR==i,]
  honeycomb_grid_sf$inc_count = lengths(st_intersects(honeycomb_grid_sf, poly))
  honeycomb_grid_sf$yr<-rep(i,1,nrow(honeycomb_grid_sf))
  appendthis<-honeycomb_grid_sf
  put_counts_here<-rbind(put_counts_here,appendthis)
  
}

library(reshape2)

put_counts_here$geometry<-NULL

#put_counts_here$test_hex_50k<-NULL
put_counts_here$test_hex_50k<-NULL
#melt this down to where I have incid & years as columns % counts for each incident by year in the rows
df_wide <- reshape(put_counts_here, idvar="grid_id", timevar="yr", v.names="inc_count", direction="wide", sep="_")
#reshape(put_counts_here, idvar = "grid_id", timevar = "yr", direction = "wide")

df_wide$slope <- apply(df_wide[2:21], 1, function(x) coef(lm(x ~ seq(x)))[2])
df_wide$score<- apply(df_wide[2:21], 1, function(x) coef(summary(lm(x ~ seq(x))))[1,4])


honeycomb_grid_sf$inc_count<-NULL

honeycomb_grid_sf$yr<-NULL

honeycomb_reg<-merge(honeycomb_grid_sf,df_wide,by="grid_id")
write_sf(honeycomb_reg,"K8_Chpt\\data_figures\\hex_reg_50k.shp")



#pattern<-"left2right"

#trying to find hexes to hatch
honeycomb_nonsig<-honeycomb_reg[!is.na(honeycomb_reg$score) & honeycomb_reg$score>0.05,]
# honeycom_nonsig<-honeycomb_reg %>% 
#   filter(!is.na(honeycomb_reg$score) & honeycomb_reg$score<0.05) %>% 
#   hatchedLayer(mode = "sfc", pattern = "left2right", density = 1)

#trying to group/union the hexes to hatch, so not doing them individually
# union<-honeycomb_nonsig %>% 
#   group_by(grid_id) %>%
#   summarise(geometry = sf::st_union(geometry)) %>%
#   ungroup()

#get them into the correct format based on package instructions to then 
#plot with tmap 
xy.sf <- sf::st_as_sf(honeycomb_nonsig) 
#as(honeycomb_nonsig,"Spatial")
#test<-xy.sf[xy.sf$grid_id==52,]
#test<-honeycomb_nonsig[honeycomb_nonsig$grid_id==52,]
#xy_sf_ply<-st_cast(test,"POLYGON",group_or_split=TRUE)
#as(test, "Spatial")
#xy.sf.hatch <- HatchedPolygons::hatched.SpatialPolygons(test, density = c(40), angle = c(45))


colnames(xy.sf)

nonsig_grids<-xy.sf[,"grid_id"]
length(unique(nonsig_grids$grid_id))
##read in hatch function from:https://gist.github.com/johnbaums/c6a1cb61b8b6616143538950e6ec34aa
put_stuff_here<-data.frame()
count<-0
for (i in nonsig_grids$grid_id){
  #print(i)
  count<-count+1
  print(count)
  mini<-nonsig_grids[nonsig_grids$grid_id==i,]
  maybeez<-hatch(mini,4) 
  put_stuff_here<-rbind(put_stuff_here,maybeez)
}

write_sf(honeycomb_reg,"K8_Chpt\\data_figures\\hatch_50k.shp")
#maybeez<-hatch(nonsig_grids,10)

# #now intersect jca_mtbs footprints with census places of func stat a
# registerDoParallel(makeCluster(12))
# ptm <- proc.time()
# print(Sys.time())
# 
# hex_id<-unique(nonsig_grids$grid_id)
# 
# hatched_polys<-foreach(i=hex_id, .combine = rbind, .packages=c('sf','sp','raster'),.verbose=TRUE) %dopar%  {
#   hatch <- function(x, density) {
#     # x: polygon object (SpatialPolygons* or sf)
#     # density: approx number of lines to plot
#     require(sp)
#     require(raster)
#     e <- extent(x)
#     w <- diff(e[1:2])
#     x1 <- seq(xmin(e), xmax(e)+w, length.out=floor(density*2))
#     x0 <- seq(xmin(e)-w, xmax(e), length.out=floor(density*2))
#     y0 <- rep(ymin(e), floor(density*2))
#     y1 <- rep(ymax(e), floor(density*2))
#     ll <- spLines(mapply(function(x0, y0, x1, y1) {
#       rbind(c(x0, y0), c(x1, y1))
#     }, x0, y0, x1, y1, 
#     SIMPLIFY=FALSE))  
#     if(is(x, 'sf')) {
#       require(sf)
#       ll <- st_as_sf(ll)
#       st_crs(ll) <- st_crs(x)
#       st_intersection(ll, x)
#     } else {
#       proj4string(ll) <- proj4string(x)
#       raster::intersect(ll, x)
#     }
#   }
#   print(hex_id)#myvars = c("LONG_X","LAT_Y")
#   #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
#   poly <- nonsig_grids[nonsig_grids$grid_id==hex_id,]
#   hatch_poly <- hatch(poly,10)
#   # veg_buf <- st_buffer(veg_type,0)
#   # veg_val<-st_make_valid(veg_buf)
#   #inter<-sf::st_intersection(fp,cenpl_buf)
#   
#   #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
#   #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#   
#   #clip1 = point.in.poly(spdf, all_polygons)
#   #getValues(clip1)
# }
# 
# print(Sys.time())
# stopImplicitCluster()
# proc.time() - ptm


#xy.sf.hatch <- hatched.SpatialPolygons(xy.sf, density = c(60), angle = c(45))

tmap_mode("view")

map_honeycomb_slopes = 
  tm_shape(honeycomb_reg) +
  tm_fill(
    col = "slope",
    palette = "-PiYG",
    style = "jenks",
    title = "Slope of Incident Count through time",
    id = "grid_id",
    n=9,
    midpoint = 0,
    showNA = FALSE,
    colorNA = NULL,
    alpha = 0.6,
    popup.vars = c(
      "Slope: " = "slope"
    ),
    popup.format = list(
      slope = list(format = "f", digits = 0)
    )
  ) +   
  tm_borders(col = "grey40", lwd = 0.1)+
  tm_shape(put_stuff_here) +
  tm_lines(lwd=0.15)



map_honeycomb_slopes



