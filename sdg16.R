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
library(ragg)
library(gridExtra)
library(grid)
library(ggrepel)
library(httr)
library(jsonlite)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

# source(paste0(sdg.dir,"make.R"))
source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat
rg.ctr <- wb_countries()
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique() %>% filter(!is.na(region_iso3c))

rgs.income <- unique(rg.ctr$income_level_iso2c) 
rgs.income <- rgs.income[!is.na(rgs.income)&!grepl("XY",rgs.income)]

# Get ISO3 code from UNSD API 
geo.api <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/DataAvailability/CountriesList"
geo.data <- content(httr::GET(geo.api)) 
unsd.geo <- fromJSON(geo.data, flatten = TRUE)

# function to pull from UNSD api
un_pull <- function(series) {
  api.request <-paste('http://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=',series,'&pageSize=50000',sep="")
  data <- content(httr::GET(api.request),"text") 
  json <- fromJSON(data, flatten = TRUE)
  get.df <- as_tibble(json$data) %>%
    mutate(TimePeriod=timePeriodStart)%>%
    left_join(unsd.geo, by=c("geoAreaCode"="M49")) %>%
    left_join(select(rg.ctr, country, ISO3=iso3c, income_level_iso3c, region_iso3c, region), by="ISO3") %>%
    mutate(value=as.numeric(value)) %>%
    rename(iso3c=ISO3) %>%
    filter(TimePeriod==2021)
} 

# Card 1: Existence of independent NHRIs in compliance with the Paris Principles by accreditation status, 2021
a.status.df <- un_pull("SG_NHR_IMPLN")
b.status.df <- un_pull("SG_NHR_INTEXSTN") 
c.status.df <- un_pull("SG_NHR_NOSTUSN") 
d.status.df <- un_pull("SG_NHR_NOAPPLN") 

a.status.old <- read_excel(paste0(input.dir, "sdg16_SG_NHR_IMPLN.xlsx"), sheet="Table format") 
b.status.old <- read_excel(paste0(input.dir, "sdg16_SG_NHR_INTEXSTN.xlsx"), sheet="Table format") 
c.status.old <- read_excel(paste0(input.dir, "sdg16_SG_NHR_NOSTUSN.xlsx"), sheet="Table format") 
d.status.old <- read_excel(paste0(input.dir, "sdg16_SG_NHR_NOAPPLN.xlsx"), sheet="Table format") 

all.status <- bind_rows(a.status.df,b.status.df,c.status.df,d.status.df) %>%
  mutate(seriesDescription=str_trim(gsub("\\(1 = YES\\; 0 \\= NO\\)","",seriesDescription),"right")) %>%
  mutate(seriesDescription=gsub("National Human Rights Institutions","NHRIs",seriesDescription)) 
all.status <- all.status %>%
  mutate(seriesDescription=factor(seriesDescription,levels=c(
    unique(all.status$seriesDescription)[1],
    unique(all.status$seriesDescription)[2],
    unique(all.status$seriesDescription)[3],
    unique(all.status$seriesDescription)[4]
  )))

sdg16_paris_principle <- figure(
  data=all.status,
  plot=function(all.status, style = style_atlas(), quality = "low"){
    df<- select(rg.ctr, iso3c) %>%
      left_join(select(all.status, iso3c, seriesDescription),by="iso3c")
    
    g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "seriesDescription",legend.nrow = 5) 
    g$theme <- style$theme_bubble_map()
    g
  }
)
agg_png(paste0(charts.dir,"sdg16_paris_principle.png"), scaling=3, width =1350, height = 800, units="px")
sdg16_paris_principle
dev.off()

# Card 2: Bribery incidence (% of firms experiencing at least one bribe payment request)
bribery.df <- wb_data(
  indicator="IC.FRM.BRIB.ZS",
  country="countries_only",
  start_date=1961,
  end_date=2021
) %>%
  filter(!is.na(IC.FRM.BRIB.ZS))

sdg16_bribery <- figure(
  data = bribery.df,
  plot = function(df, style = style_atlas()) {
    bribery.ctr <-bribery.df %>%
      filter(date>=2017) %>%
      group_by(iso3c) %>%
      filter(date==max(date)) %>%
      left_join(select(rg.ctr, iso3c, income_level_iso3c, income_level_iso2c), by="iso3c") %>%
      mutate(iso3c=as.factor(iso3c),
             iso3c=fct_reorder(iso3c,-IC.FRM.BRIB.ZS))
    
    order<- bribery.ctr %>%
      mutate(income_level_iso3c=factor(income_level_iso3c, levels=c("HIC","UMC","LMC","LIC"))) %>%
      arrange(income_level_iso3c,IC.FRM.BRIB.ZS)
    
    bribery.rev <- mutate(bribery.ctr, country=factor(country, levels=order$country))
    
    ggplot(bribery.rev, aes(x=country, IC.FRM.BRIB.ZS)) +
      geom_segment(aes(y = 0, yend = IC.FRM.BRIB.ZS,
                       x = country, xend = country),size=.25) +
      geom_point(aes(color=income_level_iso3c)) +
      scale_color_manual(values=style$colors$incomes.alt) +
      scale_y_continuous(expand=c(0,0),limits = c(0, 62)) +
      annotate("text", x = 13, y = 40, label = "High income", colour="grey", 
               size=style$gg_text_size) +
      annotate("text", x = 39, y = 40, label = "Upper middle income", colour="grey", 
               size=style$gg_text_size) +
      annotate("text", x = 59, y = 40, label = "Lower middle income", colour="grey", 
               size=style$gg_text_size) +
      annotate("text", x = 70, y = 40, label = "Low income", colour="grey", 
               size=style$gg_text_size) +
      coord_flip() +
      style$theme() +
      style$theme_barchart()
  }
)

agg_png(paste0(charts.dir,"sdg16_bribery.png"), width =1350, height = 1800, scaling=3, units="px")
sdg16_bribery
dev.off()
