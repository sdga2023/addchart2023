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
library(maps)
library(wbgmaps)
library(wbggeo)
library(jsonlite)
library(scales)
library(ragg)
library(gtable)
library(gridExtra)
library(grid)
library(ggbeeswarm)
library(mapproj)
library(ggalt)
library(httr)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat pkg
rg.ctr <- wb_countries() %>%
  filter(region!="Aggregates")
rgs <- select(rg.ctr, region_iso3c, region) %>%
  filter(!is.na(region_iso3c)) %>%
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

# Card 1
# Get data
sdg_441_df <- un_pull("SE_ADT_ACTS")

# Plot
sdg_4_IT_cmfl_skills <- figure(
  data = sdg_441_df,
  plot = function(sdg_441_df, style = style_atlas()) {
    # Latest value (2017~) by type of skills  
    sdg_441 <- sdg_441_df %>%
      filter(dimensions.Sex=="BOTHSEX"&TimePeriod>=2017) %>%
      filter(`dimensions.Type of skill`%in%c("CMFL")) %>%
      filter(dimensions.Age=="ALLAGE") %>%
      filter(dimensions.Location=="ALLAREA") %>%
      group_by(ISO3) %>%
      filter(TimePeriod==max(TimePeriod, na.rm=T)) %>%
      ungroup() %>%
      mutate(income_level_iso3c=factor(income_level_iso3c, levels=c("LIC","LMC","UMC","HIC")))

    median <- sdg_441 %>%
      select(income_level_iso3c, value) %>%
      group_by(income_level_iso3c) %>%
      summarize_each(funs(median(.,na.rm=T))) %>%
      ungroup()
      
    ggplot(sdg_441, mapping=aes(x=income_level_iso3c, y=value)) +
      geom_quasirandom(aes(color=`dimensions.Type of skill`), size=3) +
      scale_x_discrete(labels = str_wrap_lines(style$labels$income.lb,2,force=TRUE)) +
      geom_point(data= median, size=12, alpha=1, color="black", shape=95) +
      scale_fill_manual(values="black", labels="median") +
      scale_color_manual(values=style$colors$spot.primary, labels="country") +
      scale_y_continuous(breaks=seq(0,100,25), limits = c(0,100))+ 
      style$theme() +
      style$theme_legend("top")
  }
)

agg_png(paste0(charts.dir,"sdg4_IT_cmfl_skills_dark.png"), scaling=3, width =1350, height = 1000, units="px")
sdg_4_IT_cmfl_skills
dev.off()

# Card 2
literacy_df <- wb_data(
  indicator=c('SE.ADT.LITR.ZS',
              'SE.ADT.LITR.MA.ZS',
              'SE.ADT.LITR.FE.ZS'),
  start_date = 1976,
  end_date=2020,
  country='all'
) %>%
  filter(iso3c%in%c("WLD",rgs$region_iso3c)) %>% # income group aggregate not available
  pivot_longer(
    cols=c('SE.ADT.LITR.ZS',
           'SE.ADT.LITR.MA.ZS',
           'SE.ADT.LITR.FE.ZS'),
    names_to='indicator',
    values_to='value'
  ) %>%
  mutate(Group=case_when(
    indicator=="SE.ADT.LITR.ZS" ~ "Total",
    indicator=="SE.ADT.LITR.MA.ZS" ~ "Male",
    indicator=="SE.ADT.LITR.FE.ZS" ~ "Female"
  )) %>%
  filter(!is.na(value))

# Plot
sdg_4_literacy <- figure(
  data = literacy_df,
  plot = function(literacy_df, style = style_atlas()) {
    
  rg.order <- filter(literacy_df,date==2020&Group=="Total"&iso3c!="WLD") %>%
    arrange(value)
  rg.order <- c(rg.order$iso3c,"NAC","WLD")
  
  ggplot(literacy_df, aes(x=date, y=value, color=Group, group=Group)) +
    geom_line() +
    scale_color_manual(
      values = c(
        "Total"  = "#54B4B4",
        "Male" = style$colors$gender.alt[["male"]],
        "Female" =style$colors$gender.alt[["female"]]
      )) +
    style$theme() +
    style$theme_legend("top") +
    facet_wrap(~ factor(iso3c, levels=rg.order), ncol = 4, 
               labeller = as_labeller(style$labels$region.wld.lb,label_wrap_gen(width=22)),
               scale="free_x") +
        theme(
              panel.spacing.x = unit(.9, "lines"),
              axis.text.y = element_text(size=rel(.8)),
              axis.text.x = element_text(size=rel(.8), hjust=.6),
              strip.text=element_text(size=rel(.8)))
    })

agg_png(paste0(charts.dir,"sdg4_literacy_region.png"), scaling=3, width =1350, height = 1000, units="px")
sdg_4_literacy
dev.off()  

# Card 3
comp_df <- wb_data(
  indicator=c('SE.PRM.CMPT.ZS',
              'SE.PRM.CMPT.MA.ZS',
              'SE.PRM.CMPT.FE.ZS'),
  start_date = 1976,
  end_date=2020,
  country='all'
) %>%
  filter(iso3c%in%c("WLD",rgs$region_iso3c)) %>% # income group aggregate not available
  pivot_longer(
    cols=c('SE.PRM.CMPT.ZS',
           'SE.PRM.CMPT.MA.ZS',
           'SE.PRM.CMPT.FE.ZS'),
    names_to='indicator',
    values_to='value'
  ) %>%
  mutate(Group=case_when(
    indicator=="SE.PRM.CMPT.ZS" ~ "Total",
    indicator=="SE.PRM.CMPT.MA.ZS" ~ "Male",
    indicator=="SE.PRM.CMPT.FE.ZS" ~ "Female"
  )) %>%
  filter(!is.na(value)) 

# Plot
sdg_4_completion <- figure(
  data = comp_df,
  plot = function(comp_df, style = style_atlas()) {
    comp_df <- mutate(comp_df, Group=factor(Group, levels=c("Male","Total","Female"))) %>%
      filter(date>=1999)
    
    rg.order <- filter(comp_df,date==2020&Group=="Total"&iso3c!="WLD") %>%
      arrange(value)
    rg.order <- c(rg.order$iso3c,"WLD")
    
    ggplot(comp_df, aes(x=date, y=value, color=Group, group=Group)) +
      geom_line() +
      scale_color_manual(
        values = c(
          "Total"  = "#54B4B4",
          "Male" = style$colors$gender.alt[["male"]],
          "Female" =style$colors$gender.alt[["female"]])) +
      style$theme() +
      style$theme_legend("top") +
      facet_wrap(~ factor(iso3c, levels=rg.order), ncol = 4, labeller = as_labeller(style$labels$region.wld.lb,label_wrap_gen(width=22)),
                 scale="free_x") +
      theme(
        panel.spacing.x = unit(.9, "lines"),
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8), hjust=.6),
        strip.text=element_text(size=rel(.8)))
    })

agg_png(paste0(charts.dir,"sdg4_completion.png"), scaling=3, width =1350, height = 1000, units="px")
sdg_4_completion
dev.off() 
