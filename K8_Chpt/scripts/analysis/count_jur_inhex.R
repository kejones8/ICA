#this script make a hexgrid of several different resolutions and counts the number of fire footpirnts 
library(sf)
library(dplyr)
library(hkdatasets)
library(mapdata)
library(tmap)
library(tidyr)
library(foreach)
library(doParallel)
#source("K8_Chpt\\scripts\\analysis\\hatch_func.R")



#for now, read in just mtbs footprint data
incid_polys<-read_sf(incid_multipolys)
incid_polys_proj<-st_transform(incid_polys,5070)
colnames(incid_polys_proj)[1]<-"incident_id"
#add start year
#read in table that joins mtbs & incids
link_mtbs_incids<-read.csv(jca_samp_in)
incid_start_yr<-link_mtbs_incids[,c("incident_id","START_YEAR")]

#read in count data
counts<-as.data.frame(st_read(incid_count_area_mtbs_out))

counts$geom<-NULL

#join incident data to count data
incid_data<-merge(incid_polys_proj,counts,by="incident_id")


#create hexgrid of several different resolutions that covers the extent of the incident polygons 
####NEED TO RENAME THIS!!

extent = st_sfc(st_polygon(list(rbind(c(-2937836.32,367158.13), c(2412507.85,367158.13), c(2412507.85,3278304.94), c(-2937836.32,3278304.94),c(-2937836.32,367158.13)))))
st_crs(extent)<-st_crs(incid_data)

#create extent/res of hexes
#test_hex_100k <-st_make_grid(extent, c(100000,100000), what = "polygons", square = FALSE, flat_topped=TRUE)
test_hex_100k <- st_make_grid(extent, c(100000,100000), what = "polygons", square = FALSE, flat_topped=TRUE)
#test_hex_50k_proj<-st_transform(test_hex_50k,5070)
#st_crs(test_hex_50k)<-st_crs(incid_polys_proj)
st_crs(test_hex_100k)<-st_crs(incid_data)

# To sf and add grid ID
#honeycomb_grid_sf = st_sf(test_hex_50k) %>%
  honeycomb_grid_sf = st_sf(test_hex_100k) %>%
  
  # add grid ID
  mutate(grid_id = 1:length(lengths(test_hex_100k)))

# count number of burned jurs in each cell
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
#this was previously just counting geometries that intersected each hex, now we want to extract
# an attribute (some type of count of jurisdictions) related to each geom
#so, for each geom that intersects the grid cell, go get the associated count data and sum/max/whatever operation for that hex

  honeycomb_grid_sf$indicator<-st_intersects(honeycomb_grid_sf, incid_data) %>% lengths>0
  honeycomb_dointersect<-honeycomb_grid_sf[honeycomb_grid_sf$indicator==TRUE,]
  
 
  registerDoParallel(makeCluster(12))
  ptm <- proc.time()
  print(Sys.time())
  
  hex_tointer<-unique(honeycomb_dointersect$grid_id)
  
  #for every burned area mtbs footprint, intersect with surface management 
  #write out combined sf object with all intersections
  hex_intersected<-foreach(i=hex_tointer, .combine = rbind, .packages=c('sf')) %dopar%  {
    
    sing_hex<-honeycomb_dointersect[honeycomb_dointersect$grid_id==i,]
    hex_withincidat<-st_intersection(sing_hex,incid_data)#5 miles = 8047 meters
    
  }
  print(Sys.time())
  stopImplicitCluster()
  proc.time() - ptm
  
  #writes out a shapefile with all mtbs footprints and the surf management polygons they intersect
  write_sf(hex_intersected,"K8_Chpt\\data_figures\\hex_withincidata_100k.gpkg",overwrite=TRUE)
  
  
  #from geoms_int_hex, get which incid ids intersect each hex - think was is currently reported in geoms_int_hex 
  #is actually row number
  #go back and look at code from surface managment intersect....?
  #what format did i convert the st_intersects object to?
  
  
  #honeycomb_grid_sf$counts = lengths(st_intersects(honeycomb_grid_sf, incid_data))

  #now have which incidens intersect each hex, figure out what & how we want to count for juris mapping
  
  #honeycomb_grid_sf$counts <- 
  #need to get where hex_intersected only shows once incident id for every unique incident per grid cell
  uni_hex_inter<-hex_intersected[!duplicated(hex_intersected[ , c("incident_id", "grid_id")]), ]
  #uni_hex_inter<-uni_hex_inter %>% group_by(grid_id) %>% mutate(burn_threat_count=(jur_burned+jur_threatened),num_inc=n_distinct(incident_id))
  uni_hex_inter<-uni_hex_inter %>% group_by(grid_id) %>% mutate(burn_threat_count=jur_burned+jur_threatened)
  hex_cum_jurlev_count<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jurlev_burn=sum(burn_jur_level_cnt),num_inc=n_distinct(incident_id))
  #hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jurlev_burn=sum(burn_jur_level_cnt),num_inc=n_distinct(incident_id))
  
  #hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jur_burn=sum(jur_burned),num_inc=n_distinct(incident_id))
  #hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jur_threat=sum(jur_threatened),num_inc=n_distinct(incident_id))
  #hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id) %>% summarize(cum_jur_cenpl=sum(cenpl_burn_count),num_inc=n_distinct(incident_id))
  
  #hex_cum_jurcount<-uni_hex_inter %>% group_by(grid_id,num_inc) %>% summarize(cum_jur_burn_threat=sum(burn_threat_count))
  hex_cum_jurlev_count$test_hex_100k<-NULL
  hex_cum_jurlev_count$avg_jurlev_count<-hex_cum_jurlev_count$cum_jurlev_burn/hex_cum_jurlev_count$num_inc
  
  # hex_cum_jurcount$test_hex_100k<-NULL
  # hex_cum_jurcount$avg_jurlev_count<-hex_cum_jurcount$cum_jurlev_burn/hex_cum_jurcount$num_inc
  
  #spathex_withcounts<-merge(honeycomb_dointersect,hex_cum_jurcount,by="grid_id")
  spathex_withcounts<-merge(honeycomb_dointersect,hex_cum_jurlev_count,by="grid_id")
  

  
  # remove grid without value of 0 (i.e. no points in side that grid)
#honeycomb_count = filter(hex_cum_jurcount, counts > 0)

write_sf(spathex_withcounts,"K8_Chpt\\data_figures\\hex_avgjurlev_count_100k.shp",overwrite=TRUE)



tmap_mode("view")

map_honeycomb = tm_shape(spathex_withcounts) +
  tm_fill(
    col = "avg_jur",
    palette = "Reds",
    #style = "cont",
    style = "jenks",
    title = "avg_jur (burn + threat)",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "avg_jur : " = "avg_jur"
    ),
    popup.format = list(
      avg_jur = list(format = "f", digits = 0)
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

#honeycomb_grid_sf$counts<-NULL
# 
# #polys_year<-merge(incid_polys_proj,incid_start_yr,by.x="incdnt_",by.y="incident_id")
# hex_intersected$test_hex_50k<-NULL
# hexes_throughtime<-merge(honeycomb_dointersect,hex_intersected,by="grid_id")
# 
# 
# yrs<-c(1999:2018)
# 
# put_avgjur_peryr<-data.frame()
# 
# for (i in yrs){
#   
#   hex_yr<-hexes_throughtime[hexes_throughtime$START_YEAR==i,]
#   uni_hex_yr<-hex_yr[!duplicated(hex_yr[ , c("incident_id", "grid_id")]), ]
#   
#   inc_per_yr<-uni_hex_yr %>% group_by(grid_id)%>% summarize(inc_per_yr_hex = n_distinct(incident_id))
#   jur_tot<-uni_hex_yr %>% group_by(grid_id) %>% mutate(burn_threat_count=jur_burned+jur_threatened)
#   hex_cum_jur<-jur_tot %>% group_by(grid_id) %>% summarize(cum_jur_yr=sum(burn_threat_count))#,num_inc=n_distinct(incident_id))
#   hex_cum_jur$geometry<-NULL
#   inc_per_yr$geometry<-NULL
#   merged<-merge(hex_cum_jur,inc_per_yr,by="grid_id")
#   merged$avg_jur_per_yr<-merged$cum_jur_yr/merged$inc_per_yr_hex
# 
#   merged$yr<-rep(i,1,nrow(merged))
#   appendthis<-merged
#   put_avgjur_peryr<-rbind(put_avgjur_peryr,appendthis)
#   
# }
# 
# library(reshape2)
# 
# #put_avgjur_peryr$geometry<-NULL
# 
# #put_counts_here$test_hex_50k<-NULL
# put_avgjur_peryr$avg_jur_per_yr<-round(put_avgjur_peryr$avg_jur_per_yr,2)
# 
# ####SEE HOW RESHAPE WORKS
# 
# 
# #melt this down to where I have incid & years as columns % counts for each incident by year in the rows
# df_wide <- reshape(put_avgjur_peryr, idvar="grid_id", timevar="yr", v.names="avg_jur_per_yr", direction="wide", sep="_")
# #reshape(put_counts_here, idvar = "grid_id", timevar = "yr", direction = "wide")
# 
# df_wide$slope <- apply(df_wide[2:21], 1, function(x) coef(lm(x ~ seq(x)))[2])
# df_wide$score<- apply(df_wide[2:21], 1, function(x) coef(summary(lm(x ~ seq(x))))[1,4])
# 
# 
# #honeycomb_grid_sf$inc_count<-NULL
# 
# #honeycomb_grid_sf$yr<-NULL
# 
# hex_jurcounts_reg<-merge(honeycomb_dointersect,df_wide,by="grid_id")
# write_sf(hex_jurcounts_reg,"K8_Chpt\\data_figures\\hex_jurcounts_reg_50k.shp")
# 
# 
# 
# #pattern<-"left2right"
# 
# #trying to find hexes to hatch
# honeycomb_nonsig<-hex_jurcounts_reg[!is.na(hex_jurcounts_reg$score) & hex_jurcounts_reg$score>0.05,]
# # honeycom_nonsig<-honeycomb_reg %>% 
# #   filter(!is.na(honeycomb_reg$score) & honeycomb_reg$score<0.05) %>% 
# #   hatchedLayer(mode = "sfc", pattern = "left2right", density = 1)
# 
# #trying to group/union the hexes to hatch, so not doing them individually
# # union<-honeycomb_nonsig %>% 
# #   group_by(grid_id) %>%
# #   summarise(geometry = sf::st_union(geometry)) %>%
# #   ungroup()
# 
# #get them into the correct format based on package instructions to then 
# #plot with tmap 
# xy.sf <- sf::st_as_sf(honeycomb_nonsig) 
# #as(honeycomb_nonsig,"Spatial")
# #test<-xy.sf[xy.sf$grid_id==52,]
# #test<-honeycomb_nonsig[honeycomb_nonsig$grid_id==52,]
# #xy_sf_ply<-st_cast(test,"POLYGON",group_or_split=TRUE)
# #as(test, "Spatial")
# #xy.sf.hatch <- HatchedPolygons::hatched.SpatialPolygons(test, density = c(40), angle = c(45))
# 
# 
# colnames(xy.sf)
# 
# nonsig_grids<-xy.sf[,"grid_id"]
# length(unique(nonsig_grids$grid_id))
# ##read in hatch function from:https://gist.github.com/johnbaums/c6a1cb61b8b6616143538950e6ec34aa
# put_stuff_here<-data.frame()
# count<-0
# for (i in nonsig_grids$grid_id){
#   #print(i)
#   count<-count+1
#   print(count)
#   mini<-nonsig_grids[nonsig_grids$grid_id==i,]
#   maybeez<-hatch(mini,4) 
#   put_stuff_here<-rbind(put_stuff_here,maybeez)
# }
# 
# write_sf(put_stuff_here,"K8_Chpt\\data_figures\\hatch_jurcountreg_50k.shp")
# #maybeez<-hatch(nonsig_grids,10)
# 
# # #now intersect jca_mtbs footprints with census places of func stat a
# # registerDoParallel(makeCluster(12))
# # ptm <- proc.time()
# # print(Sys.time())
# # 
# # hex_id<-unique(nonsig_grids$grid_id)
# # 
# # hatched_polys<-foreach(i=hex_id, .combine = rbind, .packages=c('sf','sp','raster'),.verbose=TRUE) %dopar%  {
# #   hatch <- function(x, density) {
# #     # x: polygon object (SpatialPolygons* or sf)
# #     # density: approx number of lines to plot
# #     require(sp)
# #     require(raster)
# #     e <- extent(x)
# #     w <- diff(e[1:2])
# #     x1 <- seq(xmin(e), xmax(e)+w, length.out=floor(density*2))
# #     x0 <- seq(xmin(e)-w, xmax(e), length.out=floor(density*2))
# #     y0 <- rep(ymin(e), floor(density*2))
# #     y1 <- rep(ymax(e), floor(density*2))
# #     ll <- spLines(mapply(function(x0, y0, x1, y1) {
# #       rbind(c(x0, y0), c(x1, y1))
# #     }, x0, y0, x1, y1, 
# #     SIMPLIFY=FALSE))  
# #     if(is(x, 'sf')) {
# #       require(sf)
# #       ll <- st_as_sf(ll)
# #       st_crs(ll) <- st_crs(x)
# #       st_intersection(ll, x)
# #     } else {
# #       proj4string(ll) <- proj4string(x)
# #       raster::intersect(ll, x)
# #     }
# #   }
# #   print(hex_id)#myvars = c("LONG_X","LAT_Y")
# #   #veg_type <- read_sf(paste0("data\\veg_simp_",i,".shp"))
# #   poly <- nonsig_grids[nonsig_grids$grid_id==hex_id,]
# #   hatch_poly <- hatch(poly,10)
# #   # veg_buf <- st_buffer(veg_type,0)
# #   # veg_val<-st_make_valid(veg_buf)
# #   #inter<-sf::st_intersection(fp,cenpl_buf)
# #   
# #   #spdf <- SpatialPointsDataFrame(coords = producers_lat_long, data = split_df,
# #   #proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# #   
# #   #clip1 = point.in.poly(spdf, all_polygons)
# #   #getValues(clip1)
# # }
# # 
# # print(Sys.time())
# # stopImplicitCluster()
# # proc.time() - ptm
# 
# 
# #xy.sf.hatch <- hatched.SpatialPolygons(xy.sf, density = c(60), angle = c(45))
# 
# tmap_mode("view")
# 
# map_honeycomb_slopes = 
#   tm_shape(hex_jurcounts_reg) +
#     tm_fill(
#     col = "slope",
#     palette = "-PiYG",
#     style = "jenks",
#     title = "Slope of Avg Jur Count Per Year 1999-2018",
#     id = "grid_id",
#     n=9,
#     midpoint = 0,
#     showNA = FALSE,
#     colorNA = NULL,
#     alpha = 0.6,
#     popup.vars = c(
#       "Slope: " = "slope"
#     ),
#     popup.format = list(
#       slope = list(format = "f", digits = 0)
#     )
#   ) +   
#     tm_borders(col = "grey40", lwd = 0.1)+
#   tm_shape(put_stuff_here) +
#   tm_lines(lwd=0.15)
# 
# 
# 
# map_honeycomb_slopes
# 


