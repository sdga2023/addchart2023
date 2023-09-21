rm(list=ls())
# https://github.com/worldbank/wbgviz
# devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(reshape2)
library(stringr)
library(forcats)
library(wbstats)
library(wbgcharts)
library(wbggeo)
library(scales)
library(treemapify)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

# source(paste0(sdg.dir,"make.R"))
source(paste0(sdg.dir,"styles.R"))
style <- style_atlas()

# Get region info/unique code from wbstat
rg.ctr <- wb_countries()
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique()

# Card 1: Proportion of rural and urban people using safely managed drinking water services, (%), 2020
water.dt <- wb_data(
  indicator=c("SH.H2O.SMDW.RU.ZS","SH.H2O.SMDW.UR.ZS"),
  country = "countries_only",
  start_date=2020,
  end_date=2020,
  return_wide = TRUE)

water <- water.dt %>%
  rename(Rural=SH.H2O.SMDW.RU.ZS, Urban=SH.H2O.SMDW.UR.ZS) %>%
  mutate(diff=Urban-Rural) %>%
  filter(rank(desc(diff)) <= 30) %>%
  select(iso3c, country, Rural, Urban) %>%
  gather(key=var, value=Data, Rural:Urban)
  
sdg6_safe_water <- figure(
  data = water.dt,
  plot = function(df, style = style_atlas()) {
    ctr.order <- filter(water, var=="Rural") %>%
      arrange(desc(Data))
    ctr.order <- ctr.order$country
                        
    ggplot(water, aes(Data, country ,color=var)) +
      geom_other_dotplot(
        aes(Data, factor(country, levels = ctr.order), group=var),
        size = style$point_size/1.1,
        linesize=.25
      ) +
      style$theme() +
      style$theme_barchart() + 
      scale_x_continuous(expand = c(0,0), limits=c(0,103), breaks=seq(0,100,25)) +
      scale_color_manual(values = c("#719950","#616EA4")) +
      style$theme() +
      style$theme_barchart() +
      style$theme_legend("top") 
  }
  )

agg_png(paste0(charts.dir,"sdg6_safe_water.png"), width =1350, height = 1200, scaling=3, units="px")
sdg6_safe_water
dev.off()

# Card 2: Number of people practicing open defecation, 2000 and 2020
open.def.dt2 <- wb_data(
  indicator=c("SH.STA.ODFC.ZS","SP.POP.TOTL"),
  country = "countries_only",
  start_date=2000,
  end_date=2020,
  return_wide = FALSE) %>% filter(!is.na(value)&date%in%c(2000,2020))

sdg6_open_def <- figure(
  data=open.def.dt,
  plot=function(df, style = style_atlas()) {
    
    df <- open.def.dt2 %>%
      left_join(select(rg.ctr, iso3c, region_iso3c), by="iso3c") %>%
      select(indicator_id, iso3c, country, date, value,region_iso3c) %>%
      spread(key=indicator_id, value=value) %>%
      filter(!(is.na(SH.STA.ODFC.ZS)|is.na(SP.POP.TOTL))) %>%
      mutate(def.pop=SH.STA.ODFC.ZS/100*SP.POP.TOTL)
    
    ggplot(df, aes(area = def.pop, fill = region_iso3c, label = country,
                   subgroup=date , subgroup1=region_iso3c)) +
      geom_treemap() +
      geom_treemap_subgroup_border(color="white", size=4) +
      geom_treemap_text(colour = "white", place = "center", family=style$family, reflow = T,
                        padding.x = grid::unit(2, "mm"),
                        padding.y = grid::unit(2, "mm"), size=12, 
                        grow=F) +
      geom_treemap_subgroup_text(grow = F, place = "bottom",alpha = 0.5, colour ="white", size=13,
                                 fontface = "bold", family =style$family, min.size = 0,
                                 padding.y = grid::unit(2.5, "mm"),padding.x = grid::unit(2.5, "mm")
      ) +
      scale_fill_manual(values=style$colors$regions[c(1:4,6:7)],#remove NAC legend that are not in data
                        labels=style$labels$region.lb[c(1:4,6:7)]) + 
      style$theme() +
      theme(axis.text.y = element_blank(),
            axis.text.x= element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            panel.spacing = unit(1, "lines")) 
  }
)

agg_png(paste0(charts.dir,"sdg6_open_def_rev.png"), width =1350, height = 1000, scaling=3, units="px")
sdg6_open_def
dev.off()