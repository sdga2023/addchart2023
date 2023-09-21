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
library(gtable)
library(gridExtra)
library(grid)
library(jsonlite)
library(httr)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat pkg
rg.ctr <- wb_countries()
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
    left_join(select(rg.ctr, country, ISO3=iso3c, income_level_iso3c, region_iso3c, region), by="ISO3") 
} 

# Card 1: World’s proportion of Marine Key Biodiversity Areas (KBAs) covered by protected areas (%)
# 1.a: map
kba.df <- un_pull("ER_MRN_MPA") %>%
  rename(iso3c=ISO3)
kba <- filter(kba.df, !is.na(value)&TimePeriod==2021) 

sdg14_kba <- figure(
  data=kba,
  plot=function(df, style = style_atlas(), quality = "low"){
    
    df<- select(rg.ctr, iso3c) %>%
      left_join(select(kba, iso3c, value),by="iso3c")

    df$bins <- supercut(df$value, c(
      "0–25" = "[0,25)",
      "25–50" = "[25,50)",
      "50-75" = "[50,75)",
      "75-100" = "[75,inf)"
    ))
    g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins",legend.nrow = 2) 
    g$theme <- style$theme_bubble_map()
    g
  }
)

agg_png(paste0(charts.dir,"sdg14_kba_div.png"),scaling=3, width =1350, height =800, units="px")
sdg14_kba
dev.off()

# 1.b. Stacked area
kba.wld <- filter(kba.df, geoAreaName=="World"&TimePeriod>=2000&TimePeriod<=2021) %>%
  mutate(value=as.numeric(value)) %>%
  select(geoAreaName, TimePeriod,protected=value) %>%
  mutate(not.prt=100-protected) %>%
  gather(key=indicator, value=value, protected:not.prt)

sdg14_kba_world <- figure(
  data=kba.wld,
  plot=function(df, style = style_atlas()){
    ggplot(kba.wld, aes(x = TimePeriod, y = value, fill = indicator)) +
    geom_area() +
    scale_x_continuous(breaks =c(2000,2021), expand=c(0.01,0.4)) +
    scale_y_continuous(expand=c(0,0)) +
    annotate("text", label = "KBAs covered by protected areas", x = 2002, y = 20, hjust = 0, vjust = 0.5,
             family = style$family, size = style$gg_text_size, 
             fontface="bold", color = style$colors$text.inverse) +
    annotate("text", label = "Not covered", x = 2008, y = 70, hjust = 0, vjust = 0.5,
             family = style$family, size = style$gg_text_size, 
             fontface="bold", color = "white") +
    scale_fill_manual(
      values = c(
        protected = style$colors$spot.primary,
        not.prt ="grey80"
      )) +
    style$theme()
  }
)    

agg_png(paste0(charts.dir,"sdg14_kba_world.png"),scaling=3, width =800, height =900, units="px")
sdg14_kba_world
dev.off()

# Card 2: 
# Indicator 14.6.1, Series : 
#Progress by countries in the degree of implementation of international instruments aiming to combat illegal, unreported and unregulated fishing (level of implementation: 1 lowest to 5 highest) ER_REG_UNFCIM
fish.inst.df <- un_pull("ER_REG_UNFCIM")

fish.inst.old <-read_excel(paste0(input.dir, "sdg14_ER_REG_UNFCIM.xlsx"), sheet="Table format") %>%
  left_join(ctr.code, by=c("GeoAreaCode"="m49")) %>%
  left_join(select(rg.ctr, country,iso3c, region_iso3c, income_level_iso3c), by=c("iso3c"))

sdg14_fishing_inst <- figure(
  data=fish.inst.df,
  plot=function(df, style = style_atlas()){
    df <- fish.inst.df %>%
      rename(iso3c=ISO3) %>%
      filter(!is.na(iso3c)&TimePeriod%in%c(2020,2022)) %>%  
      mutate(value=as.numeric(gsub("NaN|N",NA, value))) %>%
      select(TimePeriod, value, country, iso3c) %>%
      spread(key=TimePeriod, value=value) %>%
      filter(!is.na(`2020`)&!is.na(`2022`)) %>%
      mutate(diff=`2022`-`2020`) %>%
      mutate(cat=ifelse(`2022`==5&diff==0, "no.chg.5",
                                     ifelse(`2022`==5&diff>0,"chgto5",
                                            ifelse(diff>0, 'progress', 
                                                   ifelse(diff<0, "decreased",
                                                          ifelse(diff==0, "no.chg",NA)))))) %>%
      mutate(cat=ifelse(is.na(diff), "no.chg",cat)) %>%
      mutate(cat=factor(cat, levels=c("decreased","no.chg","progress","chgto5","no.chg.5"))) %>%
      arrange(cat, desc(country)) %>%
      select(country,iso3c, `2022`,`2020`,diff,cat) 
    
    for (i in unique(df$cat)) {
      tem <- df %>%
        filter(cat==i) %>%
        arrange(`2022`, desc(country))
      assign(paste0("tem.", i), tem)
    }
      
    order.df <- rbind(tem.decreased, tem.no.chg, tem.progress, tem.chgto5, tem.no.chg.5)

   df2 <- df %>% 
     gather(key=year, value=value, `2020`:`2022`) %>%
     mutate(year=as.numeric(year)) %>%
     filter(!is.na(value)) %>%
     filter(!is.na(country)) 

    order <- order.df$country
    
    ggplot(df2, aes(x=value, y=country, shape = as.factor(year))) +
      geom_other_dotplot(aes(x=value, y=factor(country, levels=order), group = country), arrow = TRUE,
                         size = 3) + 
      scale_shape_manual(values = c(1,99), labels=c("2020", "2022")) +
      scale_x_continuous(limits=c(0.8,5.2), seq(1,5,1), expand=c(0,0),position = "top") +
      annotate("rect", xmin = .8, xmax = Inf, ymin = 0, ymax = 7.5,
               alpha = .3, fill="#ED8E3F") +
      annotate("text", x = 1.1, y = 4.5, label = "Decreased",
               colour="black", size=3, hjust = 0, family=style$family) +
      annotate("rect", xmin = .8, xmax = Inf, ymin = 7.5, ymax = 20.5,
               alpha = .2, fill="#FFBF24") +
      annotate("text", x = 1.1, y = 13.5, label = "No change",
               colour="black", size=3, hjust = 0, family=style$family) +
      annotate("rect", xmin = .8, xmax = Inf, ymin = 20.5, ymax = 25.5,
               alpha = .2, fill="#ABD786") +
      annotate("text", x = 1.1, y = 23.5, label = "Increased",
               colour="black", size=3, hjust = 0, family=style$family) +
      annotate("rect", xmin = .8, xmax = Inf, ymin = 25.5, ymax = Inf ,
               alpha = .2, fill="#22789D") +
      annotate("text", x = 1.1, y = 40, label = "Achieved and maintained the highest level",
               colour="black", size=3, hjust = 0, family=style$family) +
      style$theme() +
      style$theme_barchart() +
      theme(legend.position = "top") +
      theme(legend.key.size = unit(0.75,"line"))
    }
  )    
    
agg_png(paste0(charts.dir,"sdg14_fishing_inst_rev.png"),scaling=3, width =1350, height =3000, units="px")
sdg14_fishing_inst
dev.off()