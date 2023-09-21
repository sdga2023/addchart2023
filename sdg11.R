rm(list=ls())
# https://github.com/worldbank/wbgviz
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
library(ggbeeswarm)
library(jsonlite)
library(httr)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat pkg
rg.ctr <- wb_countries() %>% filter(region!="Aggregates")
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique()

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
    mutate(value=as.numeric(value))
}

# Card 1: Proportion of urban population living in slums, 2000 and 2020 (%)
slum.alt.df <- slum.df <- wb_data(
  indicator="EN.POP.SLUM.UR.ZS",
  country="countries_only",
  mrv=2021) %>%
  filter(!is.na(EN.POP.SLUM.UR.ZS)) 

slum.diff <- filter(slum.alt.df, date %in%c(2000,2020)) %>%
  select(iso3c,country,date,value=EN.POP.SLUM.UR.ZS) %>%
  spread(key=date, value=value) %>%
  mutate(diff=`2000`-`2020`) %>%
  filter(!is.na(diff))

sdg11_slum <- figure(
  data=slum.diff,
  plot=function(slum.diff, style = style_atlas()){
    p.slum <- select(slum.diff, country:`2020`) %>%
      gather(key=date, value=value, `2000`:`2020`)
    
    p.slum.in<- filter(slum.diff, diff<0) %>%
      arrange(`2020`)

    p.slum.dec<-filter(slum.diff, diff>0) %>%
      arrange(`2020`)
    
    order <- rbind(p.slum.in, p.slum.dec)
    
    order <- order$country
    
    ggplot(p.slum, aes(x=value, y=country,
                                color = as.factor(date),
                                fill = as.factor(date), shape = as.factor(date))) +
      geom_other_dotplot(aes(x=value, y=factor(country, levels=order), group = country), arrow = TRUE, size = style$point_size) +
      scale_shape_manual(values = c(style$shapes$point,99)) +
      scale_color_manual(values = c("#E56666","#54B4B4")) +
      scale_fill_manual(values = c("#E56666","#54B4B4")) +
      style$theme() +
      style$theme_barchart() +
      theme(legend.position = "top") +
      theme(legend.key.size = unit(0.75,"line")) +
      scale_x_continuous(breaks=seq(0,100,20), limits=c(0,102), expand=c(0,0)) #+
  }
)

agg_png(paste0(charts.dir,"sdg11_slum.png"), width =1350, height = 2000, scaling=3, units="px")
sdg11_slum
dev.off()


# Card 2: Proportion of population that has convenient access to public transport by city and income (%), 2020
transport.df <- read_excel(paste0(input.dir, "sdg11_SP_TRN_PUBL.xlsx"), 
                          sheet='Record format') %>%
    left_join(ctr.code, by=c("GeoAreaCode"="m49")) %>%
    left_join(select(rg.ctr, country, iso3c, income_level_iso3c, region_iso3c, region), by="iso3c") %>%
    mutate(Value=as.numeric(Value))

sdg11_transit <- figure(
  data = transport.df,
  plot = function(transport.income, style = style_atlas()) {
    transport.income <- transport.df %>%
      filter(!is.na(iso3c)) %>%
      filter(!is.na(income_level_iso3c) & income_level_iso3c!="INX") %>%
      mutate(income_level_iso3c=factor(income_level_iso3c, 
                                       levels=c("HIC","UMC",
                                                "LMC","LIC")))
    ggplot(transport.income, mapping=aes(income_level_iso3c, Value)) +
      geom_hline(yintercept=transport.df$Value[transport.df$GeoAreaName=="World"],
                 linetype="dashed", color = "#706F7D", size=.25) +
      annotate("text", x = 2.5, y = transport.df$Value[transport.df$GeoAreaName=="World"], 
               label = paste0("Global average: ",
                              round(transport.df$Value[transport.df$GeoAreaName=="World"],0)), 
               colour="#706F7D", 
               size=rel(3.5), vjust=0, hjust=-.05) +
      geom_quasirandom(aes(color=income_level_iso3c),
                       alpha=.8, size=2) +
      coord_flip() +      
      scale_y_continuous(limits=c(0,102), expand=c(0,0)) +
      style$theme() +
      scale_x_discrete(labels=style$labels$income.lb) +
      scale_color_manual(values=style$colors$incomes.alt) +
      theme(panel.grid.major.x = element_line(size = 0.35),
            panel.grid.major.y=element_blank())
  }
)
agg_png(paste0(charts.dir,"sdg11_transit.png"), scaling=3, width =1350, height = 1200, units="px")
sdg11_transit
dev.off()  
