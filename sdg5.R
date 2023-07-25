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
library(gridExtra)
library(grid)
library(ggrepel)

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

# Get ISO3 code from UNSD API 
geo.api <- "https://unstats.un.org/sdgs/UNSDGAPIV5/v1/sdg/DataAvailability/CountriesList"
geo.data <- content(httr::GET(geo.api)) 
unsd.geo <- fromJSON(geo.data, flatten = TRUE)

# Card1: WBL
wbl.df <- wb_data("SG.LAW.INDX", 
                  mrv=2022,
                  country="all") %>%
  filter(!is.na(SG.LAW.INDX))

sdg5_wbl <- figure(
  data=wbl.df,
  plot = function(df, style = style_atlas()) {

  df <- wbl.df %>% 
    left_join(select(rg.ctr, iso3c, region_iso3c), by="iso3c") %>%
    filter(date==2022)

  rg.only <- c("SSF", "ECS", "MEA", "EAS", "LCN" ,"NAC","SAS")
  
  df.ctr <- filter(df, region_iso3c%in%c(rg.only)) 
  
  df.rg <- filter(df, iso3c%in%rg.only) %>%
    select(-region_iso3c) %>%
    rename(region_iso3c=iso3c) 

  rg.order <- arrange(df.rg, desc(SG.LAW.INDX))
  rg.order <- rg.order$region_iso3c
  
  ggplot(data= df, aes(x=factor(region_iso3c, levels=rg.order), y=SG.LAW.INDX, color=region_iso3c)) +
    geom_point(data= df.ctr, size=3, alpha=0.7, shape=19) +
    geom_point(data= df.rg, size=4, alpha=1, colour = "black", shape=1) +
    scale_color_manual(values=style$colors$regions)+ 
    annotate(geom = "text", x = 8, y = 35,color=style$colors$text,
             size=style$gg_text_size*0.7, label = "More equality",  hjust=1)+
    annotate("segment", x = 8, xend =8, y = 36, yend = 40,
             arrow = arrow(length = unit(.2,"cm"))) +
    annotate(geom = "text", x = "MEA", y = 89,color=style$colors$text,
             size=style$gg_text_size*0.7, label = "Country", vjust = -1.5, hjust=0.5) +
    annotate(geom = "text", x = "MEA", y = 53,color=style$colors$text,
             size=style$gg_text_size*0.7, label = "Region", vjust = -1.5, hjust=0.5) +
    annotate(geom = "text", x = "MEA", y = 53,color=style$colors$text,
             size=style$gg_text_size*0.7, label = "Region", vjust = -1.5, hjust=0.5) +
    scale_y_continuous(breaks=seq(20,100, 20), limits=c(20,101.5))+
    scale_x_discrete(label=style$labels$region.lb, expand=c(0,1.2)) +
    coord_flip()+
    style$theme() +
    theme(panel.grid.major.x=element_line(size = 0.35),
          panel.grid.major.y=element_blank(),
          )
}
)

agg_png(paste0(charts.dir,"sdg5_wbl.png"), scaling=3, width =1350, height = 700, units="px")
sdg5_wbl
dev.off() 

# Card 2: Female genital mutilation prevalence (%)
fgm.df <- wb_data("SH.STA.FGMS.ZS", 
                  mrv=2021,
                  country="countries_only") %>%
  filter(!is.na(SH.STA.FGMS.ZS))

sdg5_fgm <- figure(
  data=fgm.df,
  plot = function(df, style = style_atlas()) {
    latest <- 2012 
    df <- fgm.df %>%
      left_join(select(rg.ctr,iso3c, region_iso3c ), by="iso3c") %>%
      filter(date>=latest)%>%
      group_by(iso3c) %>%
      filter(date==max(date))
  
    ggplot(df, aes(
      x = reorder(factor(country), SH.STA.FGMS.ZS, first),
      y = SH.STA.FGMS.ZS
    )) +
      geom_col(fill=style$colors$spot.primary) +
      coord_flip() +
      geom_text(data=filter(df, !country%in%c("Togo", "Ghana","Niger","Uganda")),
                aes(x=country,y=1, label = SH.STA.FGMS.ZS), hjust = 0,
                color="white", family = style$family, 
                size = style$gg_text_size*.8, fontface = "bold") +
      geom_text(data=filter(df, country%in%c("Togo", "Ghana","Niger","Uganda")),
                aes(x=country,y=SH.STA.FGMS.ZS, label = SH.STA.FGMS.ZS), hjust = -.25,
                color="black", family = style$family, 
                size = style$gg_text_size*.8, fontface = "bold") +
      scale_y_continuous(breaks=seq(0,100,25), limits=c(0,100),expand = c(0,2)) +
      style$theme() +
      theme(panel.grid.major.x = theme_minimal()$panel.grid.major.x,
            panel.grid.major.y = element_blank()
      )
    })

agg_png(paste0(charts.dir,"sdg5_fgm.png"), scaling=3, width =1350, height = 1500, units="px")
sdg5_fgm
dev.off() 

# Card 3: Proportion of seats held by women in national parliaments (%)
fe.par <- wb_data("SG.GEN.PARL.ZS", 
                  mrv=2021,
                  country="all") %>%
  filter(iso3c %in% c(rgs$region_iso3c,"WLD")) %>%
  filter(!is.na(SG.GEN.PARL.ZS))

# Version 1: area chart
sdg5_w_parliament <- figure(
  data=fe.par,
  plot = function(fe.par, style = style_atlas()) {
    
    rg.order <- filter(fe.par, date==2021&iso3c!="WLD") %>%
      arrange(desc(SG.GEN.PARL.ZS)) 
    rg.order <- c(rg.order$country, "World")

    ggplot(fe.par, aes(x=date, y=SG.GEN.PARL.ZS, group=iso3c, color=iso3c)) +
      geom_line(aes(color=iso3c), size=.8) +
      geom_area(aes(fill=iso3c), alpha=.7) +
      scale_x_continuous(breaks=c(1997,2010,2021), expand=c(0,3)) +
      scale_y_continuous(breaks=seq(0,40,10), limits=c(0,43)) +
      scale_fill_manual(
        values =style$colors$regions.wld) +
      scale_color_manual(
        values =style$colors$regions.wld) +
      style$theme() +
      facet_wrap(.~factor(country, 
                          levels=rg.order), ncol = 4, 
                  labeller=label_wrap_gen(width=18),
                 scale="free_x") +
      theme(
        legend.position="none",
        panel.spacing.x = unit(.9, "lines"),
        axis.text.y = element_text(size=rel(.8)),
        axis.text.x = element_text(size=rel(.8)),
        strip.text=element_text(size=rel(.8)))
  }
)

agg_png(paste0(charts.dir,"sdg5_w_parliament.png"), scaling=3, width =1350, height = 1000, units="px")
sdg5_w_parliament
dev.off() 

# Card 4: women decision making
w.decision.df <- read_excel(paste0(input.dir,"sdg5_women-decisionmaking-addchart.xlsx"), sheet="Sheet1")
w.decision.df$GeoAreaName[w.decision.df$GeoAreaName=="Tanzania"] <- "United Republic of Tanzania"
names(w.decision.df)[3:4] <-c("do","dont")

# Plot
sdg5_women_decision <-  figure(
  data=w.decision.df,
  plot = function(w.decision.df, style = style_atlas()) {
    w.decision <- w.decision.df %>% 
      left_join(unsd.geo, by=c("GeoAreaName"="Name")) %>%
      left_join(select(rg.ctr, country, ISO3=iso3c, region_iso3c, region), by="ISO3") %>%
      select(ISO3:country, do:dont, region_iso3c:region) %>%
      gather(key=ind, value=value, do:dont) %>%
      mutate(value=as.numeric(value),
             ind=factor(ind, levels=c("dont","do")),
             region=factor(region, levels=c("East Asia & Pacific","Europe & Central Asia",
                                            "Latin America & Caribbean","South Asia",
                                            "Sub-Saharan Africa"))) 
    
    ctr.order <- filter(w.decision, ind=="do") %>%
      arrange(desc(region), value)
    ctr.order <- ctr.order$country
    
    ggplot(w.decision, aes(x = value, y =factor(country, levels=ctr.order), fill = ind)) +
      geom_col(position = position_stack(reverse = FALSE), width=.8) +
      guides(fill = guide_legend(reverse = TRUE,nrow=2,byrow=TRUE)) +
      scale_fill_manual(values = c(style$colors$neutral,
                               style$colors$spot.primary),
                        labels = c('dont'="Women who don't",
      'do'="Women who make their own decisions")) +
      annotate("text",x=-72, y = 11, label=str_wrap("Sub-Saharan Africa",width=18),
               hjust = 0, family=style$family,
               size = style$gg_text_size*.8, color=style$colors$text) +
      annotate("text",x=-72, y = 12, label=str_wrap("South Asia",width=18),
               hjust = 0, family=style$family,
               size = style$gg_text_size*.8, color=style$colors$text) +
      annotate("text",x=-72, y = 17, label=str_wrap("Latin America & Caribbean",width=18),
               hjust = 0, family=style$family,
               size = style$gg_text_size*.8, color=style$colors$text) +
      annotate("text",x=-72, y = 24, label=str_wrap("Europe & Central Asia",width=15),
               hjust = 0, family=style$family,
               size = style$gg_text_size*.8, color=style$colors$text) +
      annotate("text",x=-72, y = 27, label=str_wrap("East Asia & Pacific",width=20),
               hjust = 0, family=style$family,
               size = style$gg_text_size*.8, color=style$colors$text) +
      coord_cartesian(xlim = c(0, 100), # This focuses the x-axis on the range of interest
                      clip = 'off') +
      style$theme() +
      style$theme_barchart() +
      theme(strip.text.y = element_blank(),
            legend.position = "top",
            plot.margin=unit(c(.25,.25,.25,3.25), 'cm'),
            legend.text = element_text(size = rel(.8), lineheight = 0.8)) 
  })
  
agg_png(paste0(charts.dir,"sdg5_women_decision.png"), scaling=3, width =1350, height = 1800, units="px")
sdg5_women_decision
dev.off()   