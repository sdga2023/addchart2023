# To render custom fonts,'ragg' package was used. In addition to install and import the library,
# Rstudio setting need to be adjusted: Tools > Global Options > General > Graphics, and set the Backend to AGG.
rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(stringr)
library(forcats)
library(wbstats)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(scales)
library(ragg)
library(gtable)
library(gridExtra)
library(grid)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat pkg
rg.ctr <- wb_countries()
rgs <- select(rg.ctr, region_iso3c, region) %>% 
  filter(!is.na(region_iso3c)) %>%
  unique()

# Card 1
# Get data from wbstats
wb_countries <- wb_countries()
pop_df <- wb_data(
  indicator="SP.POP.TOTL",
  start_date=1961,
  end_date=2020
)

# ers data
ag_prod_meta <- read_excel(path=paste0(input.dir, "sdg2_AgTFPInternational2019_long.xlsx"))
ag_prod_df <- read_excel(path=paste0(input.dir, "sdg2_AgTFPInternational2019_long.xlsx"), sheet="Data") %>%
  filter(Level=="Country") %>%
  rename(iso3c=ISO3,
         date=Year) %>%
  filter(Region %in% c("SSA", "LAC", "ASIA", "EUROPE","CWANA", "OCEANIA", "NORTH AMERICA")) %>%
  left_join(wb_countries) %>%
  left_join(pop_df)

pop_wgt <- ag_prod_df$SP.POP.TOTL

chart_df <- ag_prod_df %>%
  group_by(region, region_iso3c, date) %>%
  summarise(Outall_Q=sum(Outall_Q, na.rm=T),
            Outall_Index=mean(Outall_Index, na.rm=T),
            Fertilizer_Q=sum(Fertilizer_Q, na.rm=T),
            Machinery_Q=sum(Machinery_Q, na.rm=T),
            Labor_Q=sum(Labor_Q, na.rm=T)) %>%
  ungroup() %>%
  filter(!is.na(region_iso3c))


chart_df_long <- select(chart_df, -Outall_Index) %>%
  gather(key=var, value=value, Outall_Q:Labor_Q) %>%
  filter(!is.na(region)) %>%
  mutate(lab=ifelse(var=="Outall_Q","Total Agriculture Yields",
                    ifelse(var=="Fertilizer_Q", "Total Fertilizer Use",
                           ifelse(var=="Machinery_Q", "Total Agricultural Machinery",
                                  "Total Agricultural Labor")))) %>%
  mutate(lab=factor(lab, levels=c("Total Agriculture Yields","Total Fertilizer Use",
                                  "Total Agricultural Machinery","Total Agricultural Labor")))

# Plot
sdg2_agr_yields_area <-  figure(
  data=chart_df_long,
  plot=function(chart_df_long, style = style_atlas()){
  ggplot(chart_df_long, aes(x=date, y=value/1000000, fill=region_iso3c)) +
  geom_area() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(facets = ~lab, ncol=2, scales = "free",
             strip.position = "top") +
  scale_fill_manual(
    values = c(style$colors$regions),
    labels = style$labels$region.wld.lb) +
  scale_x_continuous(breaks=c(1961,1980,2000,2019), limits=c(1961,2019))+
  style$theme() +
  style$theme_legend("top")
    }
)

agg_png(paste0(charts.dir,"sdg2_agr_yields_area.png"), width =1350, height = 1100, scaling=3, units="px")
sdg2_agr_yields_area
dev.off()

# Card 2
amenia_df <- wb_data(
  indicator="SH.ANM.ALLW.ZS",
  country="all",
  start_date=1961,
  end_date=2019
) %>%
  filter(iso3c %in% c(rgs$region_iso3c, "WLD")) %>%
  filter(!is.na(SH.ANM.ALLW.ZS))

sdg_2_amenia <- figure(
  data = amenia_df,
  plot = function(amenia_df, style = style_atlas()) {
    ggplot(amenia_df, aes(x=date, y=SH.ANM.ALLW.ZS)) +
      geom_line(aes(color=iso3c), size=.8) +
      geom_area(aes(fill=iso3c), alpha=.7) +
      scale_x_continuous(breaks=c(2000,2005,2010,2015,2019)) +
      scale_y_continuous(breaks=seq(0,50,10), limits=c(0,53)) +
      scale_fill_manual(
        values =style$colors$regions.wld) +
      scale_color_manual(
        values =style$colors$regions.wld) +
      style$theme() +
      facet_wrap(~ factor(iso3c), ncol = 4, labeller = as_labeller(style$labels$region.wld.lb,label_wrap_gen(width=20)),
                 scale="free_x") +
      theme(
        legend.position="none",
        panel.spacing.x = unit(.9, "lines"),
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), hjust=.6),
        strip.text=element_text(size=rel(.8)))
  })

agg_png(paste0(charts.dir,"sdg2_amenia.png"), scaling=3, width =1350, height = 1000, units="px")
sdg_2_amenia
dev.off() 
