# To render custom fonts,'ragg' package was used. In addition to install and import the library,
# Rstudio setting need to be adjusted: Tools > Global Options > General > Graphics, and set the Backend to AGG.
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
library(wbstats)
library(wbgcharts)
library(jsonlite)
library(scales)
library(ragg)
library(countrycode)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Card 1: Share of food lost in production stage (%)
#  Data from figure ES-1, https://files.wri.org/d8/s3fs-public/reducing-food-loss-waste-global-action-agenda_1.pdf
fwaste.dt <- data.frame(region=c("North America and Oceania","Europe","Industrialized Asia",
                                 "North Africa, West and Central Asia",
                                 "Latin America","South and Southeast Asia",
                                 "Sub-Saharan Africa"),
                        total.loss=c(35,34,31,36,34,26,36),
                        loss.pro=c(0.21,0.33,0.29,0.29,0.33,0.32,0.36)) %>%
  mutate(per.loss.pro=round(total.loss*loss.pro,1))

sdg12_food_waste <- figure(
    data = fwaste.dt,
    plot = function(df, style = style_atlas()) {
      ggplot(fwaste.dt, aes(
        x = reorder(factor(region), per.loss.pro, first),
        y = per.loss.pro)) +
        geom_col(fill=style$colors$spot.primary) +
        coord_flip() +
        geom_text(aes(label = paste0(per.loss.pro,"%")), hjust = 1.5,
                  color="white", family = style$family, 
                  size = style$gg_text_size, fontface = "bold") +
        scale_y_continuous(breaks=seq(0,14,2), limits=c(0,14), expand = c(0,.3)) +
        scale_x_discrete(labels = function(x) str_wrap(fwaste.dt$region, width = 20)) +
        style$theme() +
        theme(panel.grid.major.x = theme_minimal()$panel.grid.major.x,
              panel.grid.major.y = element_blank())
        }
)

agg_png(paste0(charts.dir,"sdg12_food_waste.png"), scaling=3, width =1350, height = 700, units="px")
sdg12_food_waste
dev.off()

# Card 2: Plastic waste sites identified  
# Data from Global Plastic Watch. The following codes only contains data extract and preparation
# as the map was produced with Datawrapper. 
json <- paste0("https://raw.githubusercontent.com/earthrise-media/plastics/main/data/site_metadata/compiled_sites.geojson")
site.coord<- fromJSON(json) %>% purrr::pluck(2) %>% as.data.frame()

# Adding layer with data/no data 
# datawrapper key
ctr.wdt <- data.frame(site.coord$properties$country) %>% unique() 
names(ctr.wdt) <- "country"
ctr.wdt$iso3c <- with(ctr.wdt, countrycode(country, origin = 'country.name', destination = 'iso3c'))

# write.csv(ctr.wdt, paste0(charts.dir,"sdg12_card2_map_data.csv"), row.names = F)