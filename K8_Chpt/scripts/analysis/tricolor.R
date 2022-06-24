library(tricolore)
library(sf)
library(bruceR)

#data(euro_example)
#read in the three different variables 

jurlev_count<-read_sf("K8_Chpt\\data_figures\\hex_avgjurlev_count_100k.shp")
#jurlev<-jurlev_count[,c("grid_id","avg_jr_")]
jurlev<-jurlev_count[,c("grid_id","cm_jrl_","cm_jr_b","nm_nc_x","geometry")]
#colnames(jurlev)<-c("grid_id","avg_jur_lev")
colnames(jurlev)<-c("grid_id","cum_jur_lev","cum_jur_burn","num_inc","geometry")
#jurlev$geometry<-NULL
jurlev_df<-as.data.frame(jurlev)
jurlev_df$avg_jur_lev<-jurlev_df$cum_jur_lev/jurlev_df$num_inc
jurlev_df$avg_jur_burn<-jurlev_df$cum_jur_burn/jurlev_df$num_inc
# inc_count<-read_sf("K8_Chpt\\data_figures\\honeycomb_count_test_100k.shp")
# inc<-inc_count[,c("grid_id","n_colli")]
# inc$geometry<-NULL
# inc_df<-as.data.frame(inc)
#colnames(inc_df)<-c("grid_id","count_inc")
# jur_burnthreat_count<-read_sf("K8_Chpt\\data_figures\\hex_avgjur_burn_100k.shp")
# #burn_threat<-jur_burnthreat_count[,c("grid_id","avg_jr_")]
# jurburn<-jur_burnthreat_count[,c("grid_id","cm_jr_b","avg_jr_")]
# colnames(jurburn)<-c("grid_id","cum_jur_burn","avg_jur_burn")

#lev_inc<-merge(jurlev_df,inc_df,by="grid_id")
#all<-merge(jurlev_df,jurburn,by="grid_id")

all<-jurlev_df

#all<-merge(lev_inc,burn_threat,by="grid_id")
# all$NA.x<-NULL
# all$geometry<-all$NA.y
# all$NA.y<-NULL
# head(all)
#all$avg_jur_lev_scale<-scaler(all$avg_jur_lev,min=0,max=1)
all$cum_jur_lev_scale<-scaler(all$cum_jur_lev,min=0,max=1)
#all$count_inc_scale<-scaler(all$count_inc,min=0,max=1)
all$cum_jur_burn_scale<-scaler(all$cum_jur_burn,min=0,max=1)
all$num_inc_scale<-scaler(all$num_inc,min=0,max=1)

tric <- Tricolore(all, p1 = 'cum_jur_lev', p2 = 'cum_jur_burn', p3 = 'num_inc',spread=1,center=NA,crop=TRUE,breaks=10)
tri_sec <- TricoloreSextant(all, p1 = 'cum_jur_lev_scale', p2 = 'cum_jur_burn_scale', p3 = 'num_inc_scale',center=NA,crop=TRUE)
tri_sec<-TricoloreSextant(all, 'cum_jur_lev', 'avg_jur_burn', 'num_inc',center=NA,crop=TRUE)
all$rgb <- tric$rgb
all$rgb <- tri_sec$rgb

library(ggplot2)

plot_educ <-
  # using sf dataframe `euro_example`...
  ggplot(all) +
  # ...draw a polygon for each region...
  geom_sf(aes(fill = rgb, geometry = geometry), size = 0.1) +
  # ...and color each region according to the color code in the variable `rgb`
  scale_fill_identity()+
  ggtitle("Burned: Cum Jur Lev - Cum Count Burn - Count Inc ")

plot_educ 

library(ggtern)
#> --
#> Remember to cite, run citation(package = 'ggtern') for further info.
#> --
#> 
#> Attaching package: 'ggtern'
#> The following objects are masked from 'package:ggplot2':
#> 
#>     aes, annotate, ggplot, ggplotGrob, ggplot_build, ggplot_gtable,
#>     ggsave, layer_data, theme_bw, theme_classic, theme_dark,
#>     theme_gray, theme_light, theme_linedraw, theme_minimal, theme_void
plot_educ +
  annotation_custom(
    ggplotGrob(tric$key),
    #ggplotGrob(tri_sec$key),
    xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )

plot_educ <-
  plot_educ +
  annotation_custom(
    ggplotGrob(tric$key +
    #ggplotGrob(tri_sec$key +
                 theme(plot.background = element_rect(fill = "white", color = NA)) +
                 labs(L = 'Cum Jur Levels', T = 'Cum Count Burn', R = 'Count Inc')),
   # xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )
plot_educ




library(biscale)
library(classInt)
library(ggplot2)
library(cowplot)

custom_pal3 <- c(
  "1-1" = "#d3d3d3", # low x, low y
  "2-1" = "#ba8890",
  "3-1" = "#9e3547", # high x, low y
  "1-2" = "#8aa6c2",
  "2-2" = "#7a6b84", # medium x, medium y
  "3-2" = "#682a41",
  "1-3" = "#4279b0", # low x, high y
  "2-3" = "#3a4e78",
  "3-3" = "#311e3b" # high x, high y
)

bi_pal(pal = custom_pal3, dim = 3)

remotes::install_github("chris-prener/biscale")

###START CHANGING STUFFS

breaks<-classInt::classIntervals(all$avg_jur_lev,n=3,style="quantile")$brks
all$avgjurlev_bin <- cut(all$avg_jur_lev, breaks = breaks, include.lowest = TRUE)
breaks2<-classInt::classIntervals(all$avg_jur_burn,n=3,style="quantile")$brks
all$avgjurburn_bin <- cut(all$avg_jur_burn, breaks = breaks2, include.lowest = TRUE)

bleh<- bi_class(all, x = avgjurlev_bin, y = avgjurburn_bin, style = "quantile", dim = 3)
as.factor(bleh$bi_class)
bleh_spat<-st_as_sf(bleh)
# create map
poop <- ggplot() +
  geom_sf(data = bleh_spat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
 bi_scale_fill(pal = "PinkGrn", dim = 3) +
  labs(
    title = "Avg Jur Levels vs. Avg Jur Counts - no correction inc. #",
    #subtitle = "PurpleOr Palette"
 ) +
  bi_theme()
 
 legend <- bi_legend(pal = "PinkGrn",
                     dim = 3,
                     xlab = "More Levels",
                     ylab = "More Jurisdictions",
                     size = 8)

finalPlot <- ggdraw() +
  cowplot::draw_plot(poop, 0, 0, 1, 1)+
cowplot::draw_plot(legend, 0.5, 0.5, 0.5, 0.5)
finalPlot
# bloop <- bi_class(all, x = count_inc, y = avg_jur_burnthreat, style = "quantile", dim = 3)
# 
# 
# map <- ggplot() +
#   geom_sf(data = bloop, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
#   bi_scale_fill(pal = "GrPink2", dim = 3) +
#   labs(
#     title = "Jursidictional Tally & Incident Counts",
#     subtitle = "Gray Pink (GrPink) Palette"
#   ) +
#   bi_theme()
# 
# legend <- bi_legend(pal = "GrPink2",
#                     dim = 4,
#                     xlab = "More Incidents",
#                     ylab = "More Jurisdictions",
#                     size = 8)
# 
# finalPlot <- cowplot::ggdraw() +
#   cowplot::draw_plot(map)#, 0, 0, 1, 1) +
#   #cowplot::draw_plot(legend, 0.2, .65, 0.2, 0.2)
