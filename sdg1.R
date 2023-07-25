# To render custom fonts,'ragg' package was used. In addition to install and import the library,
# Rstudio setting need to be adjusted: Tools > Global Options > General > Graphics, and set the Backend to AGG.
rm(list=ls())
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

##############
### CARD 1 ###
# Prepare data
dt1 <- read.csv(paste0(input.dir, "sdg1_Card1.csv")) 

card1 = subset(dt1, select = -c(year))
card1_long <- melt(card1, id.vars = c("countrycode", "countryname","n"))

ctr.order <- filter(card1_long, variable=="valuemul") %>%arrange(value)
card1_long <- mutate(card1_long, countryname=factor(countryname, levels=ctr.order$countryname)) %>% arrange(countryname)

# Plot
sdg1_poverty_rate <- figure(
  data = card1_long,
  plot = function(df, style = style_atlas()) {
    ggplot(card1_long, aes(value, countryname ,color=variable)) +
      geom_other_dotplot(
        aes(value,countryname, group=variable),
        size = style$point_size/1.1,
        linesize=.25) +
      style$theme() +
      style$theme_barchart() + 
      scale_y_discrete(expand=expansion(add = c(1,2.35))) +
      scale_x_continuous(expand = c(0,0), limits=c(0,71), breaks=seq(0,70,10)) +
      scale_color_manual(values = c(style$colors$categorical.alt),
                         labels=c("Monetary","Multidimensional")) +  
      style$theme() +
      style$theme_barchart() +
      style$theme_legend("top") 
  }
 )

agg_png(paste0(charts.dir,"sdg1_poverty_rate.png"), width =1350, height = 2000, scaling=3, units="px")
sdg1_poverty_rate
dev.off()

      
##############
### CARD 2 ###
##############
card2 <- read.csv(paste0(input.dir,"sdg1_Card2.csv"))
card2$lab <- with(card2, ifelse(incomelevel=="HIC","High income",
                                ifelse(incomelevel=="LIC","Low income",
                                       ifelse(incomelevel=="LMC","Lower middle income",
                                              ifelse(incomelevel=="UMC", "Upper middle income",NA)))))
## Only the last point 
card2.2010 <- card2 %>%
  filter(decade==2010) %>%
  mutate(rest=100-value) %>%
  gather(key=var, value=pct, c(value,rest))

# Plot
sdg1_social_protection_by_income_2010 <-figure(
  data = card2.2010,
  plot = function(df, style = style_atlas()) {
    ggplot(card2.2010, aes(fill=var, y=pct, x=fct_reorder2(lab, var == "rest", pct))) + 
      geom_bar(stat="identity", width = 0.75) +
      coord_flip() +
      scale_fill_manual(values=c(style$colors$neutral,style$colors$spot.primary)) +
      scale_y_continuous(expand=c(0,3))+
      geom_text(data=filter(card2.2010, var=="value"), aes(x=lab, y=pct,label=round(pct,0)), 
                color="white", family = style$family, 
                size = style$gg_text_size, hjust=1.5, fontface = "bold") +
      style$theme() +
      style$theme_barchart() 
  },
)
agg_png(paste0(charts.dir,"sdg1_social_protection_by_income_2010.png"), scaling=3, width =1350, height = 500, units="px")
sdg1_social_protection_by_income_2010
dev.off()
