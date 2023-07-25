# To render custom fonts,'ragg' package was used. In addition to install and import the library,
# Rstudio setting need to be adjusted: Tools > Global Options > General > Graphics, and set the Backend to AGG.
rm(list=ls())
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
library(ggbeeswarm)
library(ggalluvial)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))
stye<- style_atlas()

# Get region info/unique code from wbstat
rg.ctr <- wb_countries()
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique() %>% filter(!is.na(region_iso3c))

# Card 1: Refugee by origin and destination, 2000-2020
#downloaded from: https://www.unhcr.org/refugee-statistics/download/?url=gs75ZW
ref.orig.dest <- read.csv(paste0(input.dir,"sdg10_refugee_orig_dest_population.csv"), skip=14)
ref.orig.dest <- ref.orig.dest[,1:6]
names(ref.orig.dest) <- c("year","orig","orig.iso","dest","dest.iso","ref.pop")
ref.orig.dest <- ref.orig.dest %>%
  mutate(orig.iso=ifelse(grepl("Unknown",orig), NA,orig.iso),
         dest.iso=ifelse(grepl("Unknown",dest), NA,dest.iso)) # Assign NA to unknown areas to exclude in calculation

ref.df <-ref.orig.dest %>% 
  select(orig.iso, dest.iso, ref.pop) %>%
  group_by(orig.iso, dest.iso) %>%
  summarize_each(funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  left_join(select(rg.ctr, iso3c, income_level_iso3c), by = c("orig.iso"="iso3c")) %>%
  left_join(select(rg.ctr, iso3c, income_level_iso3c), by = c("dest.iso"="iso3c")) %>%
  rename(orig.income = "income_level_iso3c.x", dest.income = "income_level_iso3c.y") %>%
  mutate(ref.pop=ifelse(orig.income=="INX"|dest.income=="INX",NA,ref.pop)) # Assign NA to unclassified country (VEN) to exclude in calculation

# Aggregate by income 
income.order<- c("HIC","UMC","LMC","LIC")

ref.income <- ref.df %>% 
  select(orig.income, dest.income, ref.pop) %>%
  group_by(orig.income, dest.income) %>%
  summarize_each(funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  filter(orig.income!="INX"&dest.income!="INX") %>% # remove flow with unclassified country (VEN)
  mutate(ref.rnd = round(ref.pop/1000000,1)) %>%
  mutate(orig.income=factor(orig.income, levels=income.order),
         dest.income=factor(dest.income, levels=income.order))
# write.csv(ref.income, paste0(sdg.dir, "misc/sdg10_ref.csv"), row.names=F)

# Number label - manually added using Adobe after chart was produced 
# To add data/location label
orig.label <- select(ref.income, orig.income, ref.pop) %>%
  group_by(orig.income) %>%
  summarize_each(funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  mutate(orig.income=paste0("orig.", orig.income)) %>%
  mutate(perc=round(ref.pop/sum(.$ref.pop)*100,0))

dest.label <- select(ref.income, dest.income, ref.pop) %>%
  group_by(dest.income) %>%
  summarize_each(funs(sum(.,na.rm=T))) %>%
  ungroup() %>%
  mutate(dest.income=paste0("dest.", dest.income)) %>%
  mutate(perc=round(ref.pop/sum(.$ref.pop)*100),0)

all.label <- select(orig.label, label=orig.income, ref.pop, perc) %>%
  bind_rows(dest.label %>% select(label="dest.income", ref.pop, perc)) %>%
  mutate(num=round(ref.pop/1000000,0))

# Plot
stratum_fill <- c(
  rev(c("HIC","UMC","LMC","LIC")),
  rev(c("HIC","UMC","LMC","LIC"))
)

# Plot
sdg10_refugee_by_origin_dest<-ggplot(data = ref.income, aes(axis1 = orig.income, axis2 = dest.income, y = ref.pop)) +
  scale_x_discrete(limits = c("orig.income", "dest.income"), labels=c("Origin", "Destination"),
                   expand = c(0.17, 0.17), position = "top") +
  geom_alluvium(aes(fill = orig.income), width=1/10) +
  scale_y_continuous(expand=c(0,1))+
  scale_fill_manual(values=style$colors$incomes.alt) +
  geom_stratum(width=1/10, fill = style$colors$incomes.alt[stratum_fill], color=NA) +
  geom_text(
    aes(label=str_wrap_lines(style$labels$income.lb[orig.income], 2)),
    stat="stratum",
    hjust = 1,
    vjust=0.8,
    nudge_x = -0.07,
    family = style$theme()$text$family,
    size = style$gg_text_size*0.8,
    lineheight = 0.75) +
  geom_text(
    aes(label=str_wrap_lines(style$labels$income.lb[dest.income], 2)),
    stat="stratum",
    hjust = 0,
    nudge_x = 0.07,
    family = style$theme()$text$family,
    size = style$gg_text_size*0.8,
    lineheight = 0.75) +
  style$theme() +
  theme(axis.text.y=element_blank(),
        panel.grid = element_blank())
  
agg_png(paste0(charts.dir,"sdg10_refugee_by_origin_dest.png"), scaling=3, width =1350, height = 1100, units="px")
sdg10_refugee_by_origin_dest
dev.off()  
  
# Card 2: The costs of sending remittances remain high, particularly in Sub-Saharan Africa 
card2 = read.csv(paste0(input.dir,"sdg10_Card2.csv")) %>%
  mutate(regionname=gsub("and","&",regionname)) %>%
  left_join(unique(select(rg.ctr, region, region_iso3c)), by=c("regionname"="region"))

rg.hline <- select(card2,regionmean_ib,regionname) %>% unique()

sdg10_remittance <- figure(
  data = card2,
  plot = function(card2, style = style_atlas()) {
    ggplot(card2, mapping=aes(regionname, ib)) +
      geom_hline(yintercept=3, linetype="dashed", color = "grey", size=.25) +
      annotate("text", x = 1, y = 2.3, label = "SDG target: 3%", colour="grey", 
                size=rel(3)) +
      geom_quasirandom(aes(color=region_iso3c)) +
      scale_color_manual(values=style$colors$regions) +
      geom_segment(aes(x = 0.75, xend =1.25, y = rg.hline$regionmean_ib[1],
                   yend=rg.hline$regionmean_ib[1]), 
                   size=.25, color=style$colors$regions[["EAS"]]) +
      geom_segment(aes(x = 1.75, xend =2.25, y = rg.hline$regionmean_ib[2],
                   yend=rg.hline$regionmean_ib[2]),
                   size=.25, color=style$colors$regions[["ECS"]]) +
      geom_segment(aes(x = 2.75, xend =3.25, y = rg.hline$regionmean_ib[3],
                   yend=rg.hline$regionmean_ib[3]),
                   size=.25, color=style$colors$regions[["LCN"]]) +
      geom_segment(aes(x = 3.75, xend =4.25, y = rg.hline$regionmean_ib[4],
                   yend=rg.hline$regionmean_ib[4]),
                   size=.25, color=style$colors$regions[["MEA"]]) +
      geom_segment(aes(x = 4.75, xend =5.25, y = rg.hline$regionmean_ib[5],
                   yend=rg.hline$regionmean_ib[5]),
                   size=.25, color=style$colors$regions[["SAS"]]) +
      geom_segment(aes(x = 5.75, xend =6.25, y = rg.hline$regionmean_ib[6],
                   yend=rg.hline$regionmean_ib[6]),
                   size=.25, color=style$colors$regions[["SSF"]]) +
      scale_y_continuous(limits=c(0,25)) +
      style$theme() +
      scale_x_discrete(labels = str_wrap_lines(rg.hline$regionname,2,force=TRUE)) +
      labs(y="Average transaction cost of sending remittances (%)") +
      theme(axis.title.y=element_text(color=style$colors$text,
                                 angle=90,vjust=.5, 
                                 margin =margin(.5,.5,.5,.5, unit="mm")))
  }
)

agg_png(paste0(charts.dir,"sdg10_remittance.png"), scaling=3, width =1350, height = 800, units="px")
sdg10_remittance
dev.off()  
