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
library(waffle)
library(ggbeeswarm)

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

# Card 1: 13.1.3 
prop.gov.df <- read_excel(paste0(input.dir, "sdg13_13.1.3_SG_DSR_SILS.xlsx"), sheet="comprehensive table")

names(prop.gov.df)[grepl("average",names(prop.gov.df))] <- "value"
names(prop.gov.df)[grepl("Income",names(prop.gov.df))] <- "income"

prop.gov.df <- filter(prop.gov.df, !is.na(value)) %>%
  left_join(select(rg.ctr, iso3c, country,income_level_iso3c, income_level), 
            by=c("ISO code"="iso3c")) %>%
  mutate(income_level_iso3c=factor(income_level_iso3c, 
                                   levels=c("HIC","UMC",
                                        "LMC","LIC")))

sdg13_drr.dot <- figure(
  data = prop.gov.df,
  plot = function(df, style = style_atlas()) {
    prop.gov <-prop.gov.df %>%
      mutate(`ISO code`=as.factor(`ISO code`),
             `ISO code`=fct_reorder(`ISO code`,-value))
    
    order<- prop.gov %>%
      mutate(income_level_iso3c=factor(income_level_iso3c, levels=c("HIC","UMC","LMC","LIC"))) %>%
      arrange(income_level_iso3c,value)
    
    prop.gov.rev <- mutate(prop.gov, country=factor(country, levels=order$country))
    
    ggplot(prop.gov.rev, aes(x=country, value)) +
      geom_segment(aes(y = 0, yend = value,
                       x = country, xend = country),size=.25) +
      geom_point(aes(color=income_level_iso3c)) +
      scale_color_manual(values=style$colors$incomes.alt) +
      scale_y_continuous(expand=c(0,0),limits = c(0, 102)) +
      annotate("text", x = 4, y = 83, label = "High income", colour="grey",
               size=style$gg_text_size*.8) +
      annotate("text", x = 38, y = 83, label = "Upper middle income", colour="grey",
               size=style$gg_text_size*.8) +
      annotate("text", x = 67, y = 83, label = "Lower middle income", colour="grey",
               size=style$gg_text_size*.8) +
      annotate("text", x = 86, y = 83, label = "Low income", colour="grey",
               size=style$gg_text_size*.8) +
      coord_flip() +
      style$theme() +
      style$theme_barchart()
  }
)

agg_png(paste0(charts.dir,"sdg13_drr.dot.png"), width =1350, height = 2200, scaling=3, units="px")
sdg13_drr.dot
dev.off()


# Card 2: 13.a.1 Climate finance
cpi.df <- read_excel(paste0(input.dir,"sdg13_CPI_Climate finance.xlsx"), 
                     sheet="Sheet2", range="A4:M15") %>%
  mutate(cat= c(rep("adap",3),rep("miti",4),rep("multi",4))) 

cpi1<- filter(cpi.df, !`Row Labels` %in% c("Adaptation","Mitigation",
                                           "Multiple Objectives")) %>%
  select(c(1,13,14)) %>%
  rename(var=`Row Labels`, value=`Grand Total`) %>%
  mutate(cat=factor(cat, levels=c("adap","miti","multi")),
         var=factor(var, levels=c("Public","Private","Unknown"))) %>%
  filter(var!="Unknown")

# plot
cpi3 <- filter(cpi.df, !`Row Labels` %in% c("Adaptation","Mitigation",
                                                   "Multiple Objectives")) %>%
  rename(var=`Row Labels`, value=`Grand Total`) %>%
  select(-cat) %>%
  filter(var!="Unknown") %>%
  group_by(var) %>%
  summarize_each(funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  select(-c(12:13)) %>%
  gather(key=sec, value=value, 
         `Agriculture, Forestry, Other land uses and Fisheries`:`Water & Wastewater`) 

p.cpi3 <- figure(
  data = cpi3,
  function(cpi3, style = style_atlas()) {
    sec.order<- filter(cpi3, var=="Public") %>%
      arrange(value)
    sec.order <- sec.order$sec
    
    ggplot(cpi3, aes(fill=factor(sec, levels=sec.order), y=value/1000, x=var)) + 
      geom_bar(stat="identity", width = 0.75) +
      coord_flip() +
      scale_fill_manual(values=c("#719950", "#22789D", "#2B98C6","#54B4B4",
                                 style$colors$categorical.alt),
                        ) +
      guides(fill=guide_legend(nrow=5,byrow=TRUE,reverse = TRUE))+
      
      scale_y_continuous(breaks=seq(0,350,50), limits=c(0,352))+
      style$theme() +
      style$theme_barchart() +
      theme(legend.position = "bottom",
            legend.spacing.y = unit(.025, 'cm'))
  })

agg_png(paste0(charts.dir,"sdg13_climate_finance_column.png"), width =1350, height = 600, scaling=3, units="px")
p.cpi3
dev.off()