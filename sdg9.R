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
    mutate(value=as.numeric(value))
} 

# Card 1
loan.df <- un_pull("FC_ACC_SSID")

loan <- loan.df %>%
  group_by(ISO3) %>%
  filter(TimePeriod==max(TimePeriod)) %>%
  filter(TimePeriod>=2018) %>%
  filter(!is.na(ISO3)) 

order<- loan %>%
  mutate(region_iso3c=factor(region_iso3c, levels=rev(c("EAS","ECS","LCN",
  "MEA","NAC","SAS","SSF")))) %>%
  arrange(region_iso3c,value)

loan.rev <- mutate(loan, country=factor(country, levels=order$country))

style <- style_atlas()

sdg9_loan <- ggplot(loan.rev, aes(x=country, value)) +
  geom_segment(aes(y = 0, yend = value,
                   x = country, xend = country),size=.25) +
  coord_flip() +
  geom_point(aes(color=region_iso3c)) +
  scale_color_manual(values=style$colors$regions) +
  scale_y_continuous(expand=c(0,0),limits = c(-.8, 82), breaks=seq(0,80,10)) +
  style$theme() +
  style$theme_barchart() + 
  annotate("text", x = 4, y = 52, label = style$labels$region.lb["SSF"], 
           colour=style$colors$regions["SSF"], 
           size=style$gg_text_size*.8, family=style$family, hjust=0) +
  annotate("text", x = 9.5, y = 52, label = style$labels$region.lb["SAS"], 
           colour=style$colors$regions["SAS"], 
           size=style$gg_text_size*.8, family=style$family,hjust=0) +
  annotate("text", x = 14, y = 52, label = style$labels$region.lb["MEA"], 
           colour=style$colors$regions["MEA"], hjust=0,
           size=style$gg_text_size*.8, family=style$family) +
  annotate("text", x = 19, y = 52, label = style$labels$region.lb["LCN"], 
           colour=style$colors$regions["LCN"], 
           size=style$gg_text_size*.8, family=style$family,hjust=0,) +
  annotate("text", x = 40.5, y = 52, label = style$labels$region.lb["ECS"], 
           colour=style$colors$regions["ECS"], 
           size=style$gg_text_size*.8, family=style$family,hjust=0) +
  annotate("text", x = 64.5, y = 52, label = style$labels$region.lb["EAS"], 
           colour=style$colors$regions["EAS"], 
           size=style$gg_text_size*.8, family=style$family,hjust=0) +
  geom_hline(yintercept=50, linetype="dashed", color = "grey20", size=.25) 

agg_png(paste0(charts.dir,"sdg9_loan.png"), width =1350, height = 2000, scaling=3, units="px")
sdg9_loan
dev.off()

# Card 2
#Research and development expenditure (% of GDP)
rnd.df <- wb_data("GB.XPD.RSDV.GD.ZS", 
                  start_date=2000,
                  end_date=2021,
                  country="countries_only") %>%
  filter(!is.na(`GB.XPD.RSDV.GD.ZS`))

rnd.pop <- wb_data("SP.POP.SCIE.RD.P6", 
                  start_date=2000,
                  end_date=2021,
                  country="countries_only") %>%
  filter(!is.na(`SP.POP.SCIE.RD.P6`)) %>%
  select(iso3c, country,date,SP.POP.SCIE.RD.P6) %>%
  group_by(iso3c,country) %>%
  filter(date>=2018) %>%
  filter(date==max(date)) %>%
  ungroup() 
  
sdg9.rnd <- figure(
  data=rnd.df,
  plot = function(rnd.df, style = style_atlas()) {
    df <- filter(rnd.df, date>=2018) %>%
      select(iso3c, country,date,rnd=GB.XPD.RSDV.GD.ZS) %>%
      group_by(iso3c,country) %>%
      filter(date==max(date)) %>%
      ungroup() %>%
      arrange(desc(rnd)) 
      
    df <- mutate(df,cat=ifelse(iso3c%in%df$iso3c[1:15], "top 15","country")) %>%
      left_join(select(rnd.pop,iso3c,rnd.pop=SP.POP.SCIE.RD.P6), by=c("iso3c"))
    
    ggplot(data=df, aes(x=rnd, y=rnd.pop)) +
     geom_point(aes(color=cat)) +
      scale_x_continuous(breaks=seq(0,5,1), limits=c(0,5)) +
      geom_text_repel(data = filter(df,cat=="top 15"), 
                       aes(label = country, x=rnd, y=rnd.pop),
                      family="Open Sans", size=3.4, color = "black",
                      box.padding = 0.2, max.overlaps = Inf,
                       segment.size  = 0.2,
                       segment.color = "grey50") +                       # direction     = "x") +
      scale_y_continuous(breaks=seq(0,9000,3000), limits=c(0,9000), label=comma,
                         expand=c(0,2)) +
      scale_color_manual(values=c(style$colors$neutral,style$colors$spot.primary)) +
      style$theme()  
  }
)

agg_png(paste0(charts.dir,"sdg9.rnd.png"), scaling=3, width =1350, height = 1500, units="px")
sdg9.rnd
dev.off() 
