library(tricolore)
library(sf)
library(bruceR)

data(euro_example)
#read in the three different variables 

jurlev_count<-read_sf("K8_Chpt\\data_figures\\hex_avgjurlev_count_100k.shp")
jurlev<-jurlev_count[,c("grid_id","avg_jr_")]
colnames(jurlev)<-c("grid_id","avg_jur_lev")
jurlev$geometry<-NULL
jurlev_df<-as.data.frame(jurlev)
inc_count<-read_sf("K8_Chpt\\data_figures\\honeycomb_count_test_100k.shp")
inc<-inc_count[,c("grid_id","n_colli")]
inc$geometry<-NULL
inc_df<-as.data.frame(inc)
colnames(inc_df)<-c("grid_id","count_inc")
jur_burnthreat_count<-read_sf("K8_Chpt\\data_figures\\hex_avgjur_burn_100k.shp")
burn_threat<-jur_burnthreat_count[,c("grid_id","avg_jr_")]
colnames(burn_threat)<-c("grid_id","avg_jur_burnthreat")

lev_inc<-merge(jurlev_df,inc_df,by="grid_id")

all<-merge(lev_inc,burn_threat,by="grid_id")
all$NA.x<-NULL
all$geometry<-all$NA.y
all$NA.y<-NULL
head(all)
all$avg_jur_lev_scale<-scaler(all$avg_jur_lev,min=0,max=1)
all$count_inc_scale<-scaler(all$count_inc,min=0,max=1)
all$avg_jur_burnthreat_scale<-scaler(all$avg_jur_burnthreat,min=0,max=1)

tric <- Tricolore(all, p1 = 'avg_jur_lev_scale', p2 = 'count_inc_scale', p3 = 'avg_jur_burnthreat_scale')
all$rgb <- tric$rgb

library(ggplot2)

plot_educ <-
  # using sf dataframe `euro_example`...
  ggplot(all) +
  # ...draw a polygon for each region...
  geom_sf(aes(fill = rgb, geometry = geometry), size = 0.1) +
  # ...and color each region according to the color code in the variable `rgb`
  scale_fill_identity()

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
    xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )

plot_educ <-
  plot_educ +
  annotation_custom(
    ggplotGrob(tric$key +
                 theme(plot.background = element_rect(fill = NA, color = NA)) +
                 labs(L = 'Avg Jur Level', T = 'Count Inc', R = 'Avg Jur B & T')),
   # xmin = 55e5, xmax = 75e5, ymin = 8e5, ymax = 80e5
  )
plot_educ



library(biscale)
library(ggplot2)
library(cowplot)

remotes::install_github("chris-prener/biscale")

bleh<- bi_class(all, x = count_inc, y = avg_jur_burnthreat, style = "jenks", dim = 4)
as.factor(bleh$bi_class)
bleh_spat<-st_as_sf(bleh)
# create map
poop <- ggplot() +
  geom_sf(data = bleh_spat, mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
 bi_scale_fill(pal = "PurpleOr", dim = 4) +
  labs(
    title = "Race and Income in St. Louis, MO",
    subtitle = "PurpleOr Palette"
 ) +
  bi_theme()
 
 legend <- bi_legend(pal = "PurpleOr",
                     dim = 4,
                     xlab = "More Incidents",
                     ylab = "More Jurisdictions",
                     size = 8)

finalPlot <- ggdraw() +
  cowplot::draw_plot(poop, 0, 0, 1, 1)+
  cowplot::draw_plot(legend, 0.2, .65, 0.2, 0.2)
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
