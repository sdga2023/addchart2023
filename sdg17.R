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
library(ggbeeswarm)
library(waffle)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

# source(paste0(sdg.dir,"make.R"))
source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat
rg.ctr <- wb_countries() %>%
  filter(region!="Aggregates")
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique() %>% filter(!is.na(region_iso3c))

# Card 1: Indicator 17.3.2, Series : Volume of remittances (in United States dollars) as a proportion of total GDP (%)
remitt.df <- wb_data("BX.TRF.PWKR.DT.GD.ZS", 
                  mrv=2023,
                  country="all") %>%
  filter(!is.na(BX.TRF.PWKR.DT.GD.ZS))

sdg17_remittance <- figure(
  data=remitt.df,
  plot = function(df, style = style_atlas()) {
    df <- remitt.df %>% 
      left_join(select(rg.ctr, iso3c, region_iso3c, income_level_iso3c), by="iso3c") %>%
      filter(!is.na(region_iso3c)) %>%
      filter(date==2021) %>%
      filter(BX.TRF.PWKR.DT.GD.ZS>=10) 
   
    ggplot(df, aes(x = reorder(factor(country), BX.TRF.PWKR.DT.GD.ZS, first), 
                   y = BX.TRF.PWKR.DT.GD.ZS, 
                   fill=income_level_iso3c)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values=style$colors$incomes.alt,
                        labels=style$labels$income.lb) +
      geom_text(data=df,
                aes(x=country,y=1, label = round(BX.TRF.PWKR.DT.GD.ZS,1)),
                    hjust = 0,
                color="white", family = style$family, 
                size = style$gg_text_size*.8, fontface = "bold") +
      style$theme() +
      style$theme_barchart() +
      scale_y_continuous(breaks=seq(0,50,10), limits=c(0,50),expand = c(0,0.5)) +
      theme(legend.position = c(0.8, 0.1)) 
  })

agg_png(paste0(charts.dir,"sdg17_remittance.png"), scaling=3, width =1350, height = 1600, units="px")
sdg17_remittance
dev.off() 

# Card 2: Debt service as a proportion of exports of goods and services (%)
debt.list <- wb_search("debt") 
debt.df <-  wb_data("DT.TDS.DPPF.XP.ZS",
                 mrv = 2022,
                 country="countries_only") %>%
  filter(!is.na(DT.TDS.DPPF.XP.ZS))


sdg17_debt <- figure(
  data=debt.df,
  plot = function(debt.df, style = style_atlas()) {
    debt <- debt.df %>%
      left_join(select(rg.ctr, iso3c, region_iso3c, income_level_iso3c),by="iso3c") %>%
      filter(date%in%c(2010,2021)) %>%
      select(-c(unit:last_updated)) %>%
      spread(key=date, value=DT.TDS.DPPF.XP.ZS) %>%
      mutate(diff=`2021`-`2010`)
    
    ggplot(data=debt, aes(x=`2010`, y=`2021`)) +
      geom_point(aes(color=region_iso3c), alpha=0.8) +
      scale_x_continuous(breaks=seq(0,60,20), limits=c(-1,62),expand=c(0,0))+
        scale_y_continuous(breaks=seq(0,60,20), limits=c(-1,62), expand=c(0,0))+
      geom_abline(slope = 1, linetype="dashed", color = "black", size=.25) +
      annotate("text",x=20, y=45, label="Increased from 2010 to 2021",
               family = style$family, size = style$gg_text_size, 
               color = style$color$text) +
      annotate("text",x=40, y=20, label="Decreased from 2010 to 2021",
               family = style$family, size = style$gg_text_size, 
                color = style$color$text) +
      scale_color_manual(values=style$colors$regions, labels=style$labels$region.lb) +
      style$theme() +
      theme(legend.position = "bottom")
  }
)
agg_png(paste0(charts.dir,"sdg17_debt.png"), scaling=3, width =1350, height = 1350, units="px")
sdg17_debt
dev.off() 

# Card 3: Individuals using the Internet (% of population)
df.cty <- wb_data("IT.NET.USER.ZS", 
                  start_date=2021,
                  end_date=2021,
                  country="countries_only") %>%
  filter(!is.na(IT.NET.USER.ZS)) %>%
  left_join(select(rg.ctr, iso3c, income_level_iso3c), by="iso3c") %>%
  mutate(income_level_iso3c=factor(income_level_iso3c, 
                                   levels=c("LIC","LMC","UMC","HIC")))

df.rg <- wb_data("IT.NET.USER.ZS", 
                 start_date=2021,
                 end_date=2021,
                 country="income_levels_only") %>%
  filter(!is.na(IT.NET.USER.ZS))

df.wld <- wb_data("IT.NET.USER.ZS", 
                 start_date=2021,
                 end_date=2021,
                 country="all") %>%
  filter(iso3c=="WLD")

rg.hline <- left_join(df.rg, select(rg.ctr, income_level_iso3c, income_level_iso2c) %>%
                     unique(), by=c("iso3c"="income_level_iso2c"))

sdg17_internet <- figure(
  data = df.cty,
  plot = function(df.cty, style = style_atlas()) {
    ggplot(df.cty, mapping=aes(income_level_iso3c, IT.NET.USER.ZS)) +
      geom_hline(yintercept=df.wld$IT.NET.USER.ZS, linetype="dashed", color = "#706F7D", size=.25) +
      annotate("text", x = 3.5, y = df.wld$IT.NET.USER.ZS, label = "Global average:63", colour="#706F7D", 
               size=rel(3.5), vjust=1.5) +
      geom_quasirandom(aes(color=income_level_iso3c)) +
      scale_color_manual(values=style$colors$incomes.alt) +
      geom_segment(aes(x = 0.75, xend =1.25, 
                       y = rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="LIC"],
                       yend=rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="LIC"]), 
                   size=.25, color=style$colors$incomes.alt[["LIC"]]) +
      geom_segment(aes(x = 1.75, xend =2.25,
                       y = rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="LMC"],
                       yend=rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="LMC"]), 
                   size=.25, color=style$colors$incomes.alt[["LMC"]]) +
      geom_segment(aes(x = 2.75, xend =3.25,
                       y = rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="UMC"],
                       yend=rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="UMC"]), 
                   size=.25, color=style$colors$incomes.alt[["UMC"]]) +
      geom_segment(aes(x = 3.75, xend =4.25,
                       y = rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="HIC"],
                       yend=rg.hline$IT.NET.USER.ZS[rg.hline$income_level_iso3c=="HIC"]), 
                   size=.25, color=style$colors$incomes.alt[["HIC"]]) +
      geom_text(data=rg.hline, aes(label=round(IT.NET.USER.ZS,0), color=income_level_iso3c),
                hjust=-2.8, vjust=.75,size=rel(3.5))+
      scale_y_continuous(limits=c(0,100)) +
      style$theme() +
      scale_x_discrete(labels = str_wrap_lines(style$labels$income.lb,2,force=TRUE)) 
  }
)
agg_png(paste0(charts.dir,"sdg17_internet.png"), scaling=3, width =1350, height = 1000, units="px")
sdg17_internet
dev.off() 

# Card 4: 17.19.2
# Get ISO3 code from UNSD API 
geo.api <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/DataAvailability/CountriesList"
geo.data <- content(httr::GET(geo.api)) 
unsd.geo <- fromJSON(geo.data, flatten = TRUE)

# function to pull from UNSD api
un_pull <- function(series) {
  api.request <-paste('http://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=',series,'&pageSize=50000',sep="")
  data <- content(httr::GET(api.request),"text") 
  json <- fromJSON(data, flatten = TRUE)
  get.df <- as_tibble(json$data)  %>%
    mutate(TimePeriod=timePeriodStart)%>%
    left_join(unsd.geo, by=c("geoAreaCode"="M49")) %>%
    left_join(select(rg.ctr, country, ISO3=iso3c, income_level_iso3c, region_iso3c, region), by="ISO3") %>%
    mutate(value=as.numeric(value)) %>%
    rename(iso3c=ISO3)%>%
    filter(TimePeriod==2020)
} 

birth.reg.df <- un_pull("SG_REG_BRTH90N")
dth.reg.df <- un_pull("SG_REG_DETH75N")

df.reform <- function(df0) {
    df <- df0 %>%
      filter(!is.na(iso3c)) 
    
    df.out <- rg.ctr %>%
      select(country, iso3c,region,region_iso3c, income_level_iso3c) %>%
      left_join(select(df, iso3c,value) , by="iso3c") %>%
      mutate(cat=ifelse(is.na(value),"No data",value)) %>%
      mutate(cat=ifelse(cat=="1","Yes",
                        ifelse(cat=="0","No","No data")))

}

birth <- df.reform(birth.reg.df)
dth <- df.reform(dth.reg.df)

all.dt <- birth %>%
  left_join(select(dth, iso3c, dth.cat=cat), by="iso3c") %>%
  mutate(cat.all = ifelse(country %in% c("Burundi","Gambia, The","Mozambique"), "Doesn’t meet either completeness threshold",
                          ifelse(cat=="Yes"&dth.cat=="Yes","Meets both birth and death registration data completeness threshold",
                                 ifelse(cat=="Yes"&dth.cat=="No", "Meets birth completeness threshold but not death",
                                        ifelse(cat=="No"&dth.cat=="Yes","Meets death completeness threshold but not birth",
                                               ifelse(cat=="No data"&dth.cat=="No data","Missing data",
                                                      ifelse(cat=="No"&dth.cat=="No", "Doesn’t meet either completeness threshold", NA))))))) %>%
  mutate(n=1)

p.df <- all.dt %>%
  select(region,cat.all,n) %>%
  group_by(cat.all, region) %>%
  summarise_all(funs(sum)) %>%
  ungroup() %>%
  mutate(cat.all=factor(cat.all, levels=c("Meets both birth and death registration data completeness threshold",
                                  "Meets birth completeness threshold but not death",
                                  "Meets death completeness threshold but not birth",
                                  "Doesn’t meet either completeness threshold",
                                  "Missing data"))) %>%
  arrange(cat.all) %>%
  mutate()
 
p2<- ggplot(p.df, aes(values=n)) +
  geom_waffle(aes(fill=cat.all),color = "white", size = .25, n_rows = 5, flip = TRUE) +
  facet_grid(~region, 
             labeller = labeller( region = label_wrap_gen(13))) +
  coord_equal() +
  scale_fill_manual(values=c("#ABD786","#FFBF24","#ED8E3F",
                             "#9E2F59",style$colors$neutral)) +
  guides(fill=guide_legend(nrow=5)) +
  style$theme() +
  theme(panel.grid = element_blank(),
        strip.text.y = element_blank(),
        strip.text.x=element_text(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y=element_blank(),
        legend.position = "bottom") +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,.3)) 

agg_png(paste0(charts.dir,"sdg17_registration.png"), scaling=3, width =1350, height =750, units="px")
p2 
dev.off()      