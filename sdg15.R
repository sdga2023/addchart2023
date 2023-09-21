rm(list=ls())
# https://github.com/worldbank/wbgviz
# devtools::install_github("worldbank/wbgviz", subdir = "wbgdata")
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtreemap) #devtools::install_github("econandrew/ggtreemap")
library(readxl)
library(readr)
library(reshape2)
library(stringr)
library(forcats)
library(wbstats)
library(wbgcharts)
library(wbggeo)
library(scales)
library(waffle)
library(ragg)

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
  unique()

# Import area code (M49 and ISO3Code)
ctr.code <- read.csv(paste0(input.dir,"UNSD — Methodology.csv"), sep=";",stringsAsFactors = F) %>%
  select(m49=M49.Code, iso3c=ISO.alpha3.Code) %>% unique()

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
    rename(iso3c=ISO3)
} 

# Card 1: Countries that established national targets in accordance with Aichi Biodiversity Target 2 of the Strategic Plan for Biodiversity 2011-2020 in their National Biodiversity Strategy and Action Plans
abt2.dt <- un_pull("ER_BDY_ABT2NP")

# Load inputs
abt2.dt <- read_excel(paste0(input.dir, "sdg15_ER_BDY_ABT2NP.xlsx"), sheet="Record format")
abt2.codes <- read_excel(paste0(input.dir, "sdg15_ER_BDY_ABT2NP.xlsx"),
                       sheet="CodeDescriptions")
abt2.codes <- abt2.codes[2:7,]

# plot
sdg15_abt2 <- figure(
  data = abt2.dt,
  plot = function(df, style = style_atlas()) {
    ctr.rg.only <- filter(rg.ctr, !is.na(region_iso3c))
    df <- abt2.dt %>%
      left_join(unsd.geo %>%
                  select(M49, iso3c=ISO3) %>% 
                  mutate(M49=as.numeric(M49)), by=c("GeoAreaCode"="M49")) %>%
      filter(!is.na(iso3c)) %>% # remove regional aggregates
      filter(`Level/Status`!="_T") %>%
      full_join(select(ctr.rg.only, iso3c,country, region_iso3c), by="iso3c") %>%
      mutate(status=ifelse(is.na(GeoAreaName), "No data", 
                           ifelse(`Level/Status`=="ABT2EXCEED","ABT2ACHIEVE",
                           ifelse(`Level/Status`=="ABT2DIGRESS","ABT2NOPROG",`Level/Status`)))) %>%
      mutate(Value=ifelse(is.na(GeoAreaName), 1,Value)) %>%
      filter(!is.na(region_iso3c))
      
      # unique(df$Value)
    p.df <- df %>%
      select(region_iso3c, cat=status, Value) %>%
      group_by(cat, region_iso3c) %>%
      mutate(Value=as.numeric(Value)) %>%
      summarise_all(funs(sum)) %>%
      mutate(cat=factor(cat, levels=c("ABT2ACHIEVE","ABT2INSUFNT",
                                     "ABT2NOPROG","ABT2NONTLT","No data")))
    
    ggplot(p.df, aes(fill=cat, values=Value)) +
      geom_waffle(color = "white", size = .25, n_rows = 5, flip = TRUE) +
      facet_wrap(~region_iso3c, ncol = 7, strip.position = "top",
                 labeller = as_labeller(style$labels$region.lb, label_wrap_gen(width=13))) +
      scale_fill_manual(values=c(style$colors$categorical.alt[[4]],
                                 style$colors$categorical.alt[[2]],style$colors$categorical.alt[[3]],
                                 "#9E2F59", style$colors$neutral),
                        labels=c("ABT2ACHIEVE"="National target reflecting ABT2 exists and progress is on track to achieve or exceed it",
                                 "ABT2INSUFNT"="National target reflecting ABT2 exists and progress is there, but at as insufficient rate",
                                 "ABT2NOPROG"="National target reflecting ABT2 exists, but no progress or moving away from it",
                                 "ABT2NONTLT"="No national target reflecting ABT 2",
                                 "No data"="No data")) +
      coord_equal() +
      guides(fill=guide_legend(nrow=5)) +
      style$theme() +
      theme(panel.grid = element_blank(), 
            legend.position = "bottom",
        
            strip.text = element_text(size=rel(.75), vjust=1),
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y=element_blank(),
            legend.margin = margin(.75,0,0,0, "lines")) +
      scale_x_discrete(expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) 
    }
)

agg_png(paste0(charts.dir,"sdg15_abt2_v3.png"), scaling=3, width =1350, height = 800, units="px")
sdg15_abt2
dev.off()

# Card 2: Total official development assistance for biodiversity, by recipient countries
oda.dt <- un_pull("DC_ODA_BDVL")

# plot
sdg15_oda <-  figure(
  data = oda.dt,
  plot = function(df, style = style_atlas()) {
    dt <- oda.dt %>%
      filter(!is.na(iso3c)&TimePeriod<=2020) %>% # remove regional aggregates
      filter(!is.na(region_iso3c)) #Exclude small countries with no regional classification

    p.dt <- dt %>%
      select(region_iso3c, TimePeriod, value) %>%
      mutate(value=as.numeric(value)) %>%
      group_by(region_iso3c, TimePeriod,) %>%
      summarise_all(funs(sum))

    ctr.order <- filter(p.dt, TimePeriod==2020) %>%
      arrange(value)
    ctr.order <- ctr.order$region_iso3c

    ggplot(p.dt, aes(x=TimePeriod, y=value, fill=factor(region_iso3c, levels=ctr.order))) +
      geom_col() +
      scale_x_continuous(breaks=seq(2002,2020,1), expand=c(0,0))+
      scale_y_continuous(expand=c(0,NA), labels=scales::comma,
                         breaks=seq(0,15000,5000), limits=c(0,15500)) +
      scale_fill_manual(
        values = style$colors$regions[c(1:4,6:7)], #Exclude NAC that is not in data
        labels = style$labels$region.lb[c(1:4,6:7)]) +
      style$theme() +
      theme(legend.position="bottom",
            axis.text=element_text(size=rel(.75)),
            legend.margin = margin(.5,0,0,0, "lines"))
  }
)

agg_png(paste0(charts.dir,"sdg15_oda.png"), scaling=3, width =1350, height = 700, units="px")
sdg15_oda
dev.off()
