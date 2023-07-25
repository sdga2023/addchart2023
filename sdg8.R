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
library(wbgmaps)
library(wbggeo)
library(scales)
library(ragg)
library(treemapify)

# Set directories
dir <- getwd()
sdg.dir <- paste0(dir,"/SDG_Atlas/addChart2022/")
input.dir <- paste0(sdg.dir, "inputs/" )
charts.dir <- paste0(sdg.dir, "charts/" )

source(paste0(sdg.dir,"styles.R"))

# Get region info/unique code from wbstat pkg
rg.ctr <- wb_countries()
rgs <- select(rg.ctr, region_iso3c, region) %>%
  unique() %>% filter(!is.na(region_iso3c))

# Card 1
# Account ownership, 2021 (% of population ages 15 and older)
indicators <- tribble(
  ~indicator_id,        ~group,      ~category,
  "FX.OWN.TOTL.MA.ZS", "Gender",    "Male",
  "FX.OWN.TOTL.FE.ZS", "Gender",    "Female",
  "FX.OWN.TOTL.SO.ZS", "Education", "Secondary or more",
  "FX.OWN.TOTL.PL.ZS", "Education", "Primary or less")
  
df <- wbstats::wb_data(
  indicator=indicators$indicator_id,
  country=c("WLD", "EAS","ECS","LCN","MEA","NAC","SAS","SSF"),
  start_date= 2021,
  end_date = 2021,
  return_wide = FALSE
  )
  
# Plot  
sdg8_account_pop15plus <- figure(
  data = df,
  plot = function(df, style = style_atlas()) {
    df <- df %>% left_join(indicators)
    
    iso.order <- filter(df, category=="Male" & iso3c!="WLD") %>% arrange(desc(value))
    iso.order <- c("WLD", iso.order$iso3c) # world at the bottom
    
    df <- df %>%
      mutate(iso3c = factor(iso3c, levels =iso.order )) %>%
      mutate(category = factor(category, levels = indicators$category)) %>%
      mutate(group=factor(group, levels=c("Gender","Education")))
    
    ggplot(df, aes(value, iso3c ,color=category)) +
      annotate("rect", xmin = 0, xmax = 100, ymin = 0.5, ymax = 1.5,
               fill="grey",alpha = .2) +
      geom_other_dotplot(aes(value,iso3c, group=paste0(group,category)),
                         size = style$point_size, 
                         linesize=.25
                         ) +
      scale_color_manual(values = c(
        "Male"               = style$colors$spot.primary.dark,
        "Female"             = style$colors$spot.primary.light,
        "Secondary or more"  = style$colors$spot.primary.dark,
        "Primary or less"    = style$colors$spot.primary.light        
                )) +
      guides(color=guide_legend(nrow=2)) +
      scale_x_continuous(limits = c(0, 100), expand=c(0,10)) +
      scale_y_discrete(labels = 
                         c('WLD'="World",
                           'EAS'="East Asia & Pacific",
                           'ECS'="Europe & Central Asia", 
                           'LCN'="Latin America & Caribbean",
                           'MEA'="Middle East & North Africa",  
                           'NAC'="North America",  
                           'SSF'="Sub-Saharan Africa", 
                           'SAS'="South Asia"
                         )) +
      facet_wrap(~group,nrow=1) +
      style$theme() +
      style$theme_legend("top") +
      style$theme_barchart() + 
      theme(
        strip.text.x=element_text(size=rel(.8),hjust=0.5),
        panel.spacing.x = unit(0.05, "npc"),
        strip.text.y = element_blank(),
        legend.spacing.x = unit(0.5, "lines"),
        legend.spacing.y = unit(.5, "lines")) 
    }
    )

agg_png(paste0(charts.dir,"sdg8_account_pop15plus_rev.png"), scaling=3, width =1350, height = 700, units="px")
sdg8_account_pop15plus
dev.off()


# Card 2
#Proportion of youth (aged 15-24 years) not in education, employment or training. Latest value after 2018.
indicators <- tribble(
  ~indicator_id,        ~group,      ~category,
  "SL.UEM.NEET.MA.ZS", "Share of youth not in education, employment or training",       "Male",
  "SL.UEM.NEET.FE.ZS", "Share of youth not in education, employment or training",      "Female"
  )
    
df <- wbstats::wb_data(
  indicator=indicators$indicator_id,
  country="countries_only",
  start_date= 2018,
  end_date = 2021,
  return_wide = FALSE) %>%
  group_by(country) %>%
  mutate(value_fill=value) %>%
  fill(value_fill, .direction = 'up') %>%
  filter(date==2021)

sdg8_pct_pop15t24_no_edu_employed <- figure(
  data = cntry_df,
  plot = function(df, style = style_atlas()) {
    df <- df %>% left_join(indicators)
    
    cntry_df <- df %>%
      filter(!is.na(value_fill)) %>%
      left_join(wbstats::wb_countries()) %>%
      filter(income_level!="High income") %>%
      mutate(value=value_fill) %>%
      ungroup() 
    
    ctr.order <- filter(cntry_df, indicator_id=="SL.UEM.NEET.FE.ZS") %>%arrange(value)
    cntry_df <- mutate(cntry_df, country=factor(country, levels=ctr.order$country)) %>% arrange(country) %>%
      mutate(category=tolower(category))
    
    ggplot(cntry_df, aes(x=value, y=country)) +
      geom_other_dotplot(
        aes(value,country, group=category,color=category),
        size = 2, linesize = .25) +
       scale_x_continuous(expand = c(0,0.5), limits=c(0,102), breaks=seq(0,100,20)) +
      scale_color_manual(values = c(style$colors$gender.alt)) +  
      geom_text(data=filter(cntry_df, iso3c=="PAK"&category=="female"), 
                aes(x=value, y=country, label=category), hjust=-.2, family=style$family, size=3.5)+
      geom_text(data=filter(cntry_df, iso3c=="PAK"&category=="male"), 
                aes(x=value, y=country, label=category), hjust=1.2, family=style$family, size=3.5)+
      style$theme() +
      style$theme_barchart() +
      theme(
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.spacing.y = unit(0, "lines"),
        legend.margin=margin(c(0,0,0,0)))
    }
  )

agg_png(paste0(charts.dir,"sdg8_pct_pop15t24_no_edu_employed.png"),scaling=3, width =1350, height =2200, units="px")
sdg8_pct_pop15t24_no_edu_employed
dev.off()

# Card 3
#  Findex dataset from https://www.worldbank.org/en/publication/globalfindex/Data
findex.dt <- read_excel(paste0(input.dir, "sdg8_Databank-wide.xlsx"))

# Pull list of indicator names and description
findex.lab <- findex.dt[1,] %>%
  select(account_t_d:fin5_2017_d_12) %>%
  gather(key=ind, value=desc, account_t_d:fin5_2017_d_12)

unique(findex.dt$countrynewwb[is.na(findex.dt$regionwb21_hi)])

findex.re <- findex.dt[c(2:nrow(findex.dt)),] %>%
  mutate(account_t_d=as.numeric(account_t_d),
         adult_account=pop_adult_l1*account_t_d,
         adult_no_account=pop_adult_l1-adult_account) %>%
  left_join(select(rg.ctr,iso3c,region_iso3c, admin_region_iso3c),by=c("codewb"="iso3c")) %>%
  select(countrynewwb:year,adult_account, adult_no_account, region_iso3c,admin_region_iso3c) %>%
  bind_rows(tibble(countrynewwb="Middle East & North Africa",
                  codewb="MEA", # MEA aggregate for 2014 is somehow missing. Manually sum countries 
                  year=2014,
                  adult_account=sum(.$adult_account[.$region_iso3c=="MEA"&.$year==2014],na.rm=T),
                  adult_no_account=sum(.$adult_no_account[.$region_iso3c=="MEA"&.$year==2014],na.rm=T))) %>%
  filter(codewb%in%c("EAS","ECS","LCN","MEA","NAC","SAS","SSF")) %>%
  select(-c(region_iso3c,admin_region_iso3c)) %>%
  gather(key=var, value=value, adult_account:adult_no_account) %>%
  mutate(all.lab=paste0(countrynewwb, "\n",round(value/1000000,0)))
  
# Plot 
sdg8_unbanked <- figure(
  data = findex.re,
  plot = function(findex.re, style = style_atlas()) {
    findex.re <- findex.re %>%
      filter(var=="adult_no_account") %>%
      mutate(lab=ifelse(var=="adult_no_account", "Unbanked",NA))
    ggplot(filter(findex.re,year%in%c(2011,2021)), aes(area = value, fill = codewb, label = all.lab,
                                             subgroup = year)) +
      geom_treemap() +
      geom_treemap_subgroup_border(color="white", size=9) +
      geom_treemap_text(colour = "white", place = "center", family=style$family, reflow = T,
                        padding.x = grid::unit(2, "mm"),
                        padding.y = grid::unit(2, "mm"), size=11, grow=F) +
      scale_fill_manual(values=style$colors$regions) +
      style$theme() +
      theme(axis.text.y = element_blank(),
            axis.text.x= element_blank(),
            panel.grid = element_blank(),
            legend.position = "none"
      ) 
  }
 )
agg_png(paste0(charts.dir,"sdg8_unbanked_2011_2021.png"), width =1350, height = 900, units="px", scaling=3)
sdg8_unbanked
dev.off()
