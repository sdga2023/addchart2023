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
rg.only <- filter(rgs, region!="Aggregates") 

# Card 1: A significant share of world population still uses polluting inefficient cooking systems
## In Sub-Saharan Africa, less than 20% of population uses efficient cooking systems in 2020
#SDG 7.1.2 Access to clean fuels and technologies for cooking (% of population)
cooking.dt <-  wb_data(indicator = "EG.CFT.ACCS.ZS", 
                       start_date=2000,
                       end_date=2020,
                       country="all") %>%
  filter(iso3c %in% c(rg.only$region_iso3c, "WLD")) %>%
  filter(date==2020)

sdg7_cooking_rg <- figure(
  data= cooking.dt,
  plot=function(df, style = style_atlas()){
    dt <- cooking.dt %>% 
      filter(!is.na(EG.CFT.ACCS.ZS)) %>%
      mutate(rest=100-EG.CFT.ACCS.ZS) %>%
      select(iso3c,country, EG.CFT.ACCS.ZS, rest) %>%
      gather(key=val, value=value,EG.CFT.ACCS.ZS:rest)
    
    rg.order <- filter(dt, val=="EG.CFT.ACCS.ZS"& iso3c!="WLD") %>%
      arrange(desc(value))
     
    rg.order <- c(rg.order$iso3c,"WLD")
    dt$iso3c <- factor(dt$iso3c, levels=rg.order)
    dt$val <- factor(dt$val, levels=c("EG.CFT.ACCS.ZS","rest"))
    
    ggplot(dt, aes(x = iso3c, y = value, fill = val)) +
      annotate("rect", ymin = 0, ymax = Inf, xmin = 7.5, xmax = 8.5,
               fill="grey",alpha = .3) +
      geom_col(position = position_stack(reverse = TRUE), width=.6) +
      scale_fill_manual(values = c(style$colors$spot.primary,
                                   style$colors$neutral)) +
      scale_y_continuous(expand=c(0,0), limits=c(0,102)) +
      # scale_x_discrete(labels = str_wrap_lines(style$labels$region.wld.lb,2,force=TRUE)) +
      scale_x_discrete(labels = style$labels$region.wld.lb) +
      coord_flip()+
      style$theme() +
      geom_text(data=filter(dt, val=="EG.CFT.ACCS.ZS"), 
                            aes(label = paste0(round(value,0),"%"), x=iso3c, y=value),
                hjust=1.5, family="Open Sans",size = 3.5,
                            color="white", fontface="bold") +
      style$theme_barchart() +
          theme(strip.text.y = element_blank(),
            legend.spacing.y = unit(.25, "lines"),
            legend.text = element_text(size = rel(.8), lineheight = 0.8))
  }
)

agg_png(paste0(charts.dir,"sdg7_cooking_rg_bar.png"), scaling=3, width =1350, height = 800, units="px")
sdg7_cooking_rg
dev.off()  

# Card2: The share of renewable energy in total final energy consumption increased in 60% of the top 20 energy consuming countries between 2000 and 2019
#renewable energy share in top 20 energy consuming countries in 2000, 2019
# List of top 20 energy-consuming countries from Energy Progress Report
# https://trackingsdg7.esmap.org/data/files/download-documents/sdg7-report2022-ch3-renewable_energy.pdf
top20 <- data.frame(country=c("Brazil","Canada","Spain","Italy","Germany","India","France",
                              "Turkey","United Kingdom","China","United States","Mexico",
                              "Indonesia","Japan","Pakistan","Nigeria",
                              "Korea","Russia","Iran","Saudi Arabia")) 

top20$country[top20$country=="Turkey"] <- "Turkiye"
top20$country[top20$country=="Korea"] <- "Korea, Rep."
top20$country[top20$country=="Russia"] <- "Russian Federation"
top20$country[top20$country=="Iran"] <- "Iran, Islamic Rep."

top20 <- left_join(top20, select(rg.ctr,country,iso3c, region_iso3c, region), by=c("country")) 

re.energy.dt <- wb_data(indicator = "EG.FEC.RNEW.ZS", 
                        start_date=2000,
                        end_date=2020, 
                        country="all") %>%
  left_join(select(rg.ctr, iso3c, region_iso3c), by="iso3c")

top20.re.eng <- filter(re.energy.dt, iso3c %in% top20$iso3c&date%in%c(2000,2019)) 

# difference
top20.re.wide <- filter(re.energy.dt, iso3c %in% top20$iso3c&date%in%c(2000,2019)) %>%
  spread(key=date, value=EG.FEC.RNEW.ZS) %>%
  mutate(dif=`2019`-`2000`) %>%
  mutate(cat=ifelse(`2019`-`2000`>0, "Increased", "Decreased")) %>%
  gather(key=date, value=value, `2000`:`2019`)

style <- style_atlas()

df <-top20.re.wide %>%
  mutate(date=factor(date, levels=c(2019,2000))) %>%
  mutate(cat=factor(cat, levels=c("Increased","Decreased"))) %>%
  bind_rows(data.frame(country=c("a","b","c","d"), # Add empty rows to make the bar size same. They are removed in Illustrator
                           cat=rep("Decreased",4),
                           value=rep(0,4),
                           date=as.factor(c(rep(2000,2),rep(2019,2)))))

increased <- ggplot(filter(df, cat=="Increased"), 
                    aes(x = fct_reorder(country,value), y = value, fill = date)) +
  geom_col(position = "bullet", width=.8) +
  scale_fill_manual(
    values=c(
          `2019` = style$colors$spot.primary,`2000` = style$colors$neutral),
    guide = guide_legend(reverse = TRUE, nrow=1, byrow=TRUE)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,102))+
  coord_flip()+
  labs(title="Increased")+
  style$theme() +
  style$theme_barchart() +
  style$theme_legend("top") +
  theme(strip.text = element_text(size=rel(.8), face="bold"),
        plot.title = element_text(family = "Open Sans", size=rel(.8)))
    
ctr.order.de <- filter(df, date==2019&cat=="Decreased") %>%arrange(value)
ctr.order.de <-c(c("a","b","c","d"), ctr.order.de$country[3:10])
    
decreased <- ggplot(filter(df, cat=="Decreased"), 
                    aes(x = factor(country,levels=ctr.order.de), y = value, fill = date)) +
  geom_col(position = "bullet", width=.8) +
  scale_fill_manual(
    values=c(`2019` = style$colors$spot.primary,
             `2000` = style$colors$neutral)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,102))+
  coord_flip()+
  labs(title="Decreased")+
  style$theme() +
  style$theme_barchart() +
  style$theme_legend("top") +
  theme(strip.text = element_text(size=rel(.8), face="bold"),
        plot.title = element_text(family = "Open Sans", size=rel(.8)),
        legend.position = "none")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
    
mylegend<-g_legend(increased)
    
agg_png(paste0(charts.dir,"sdg7_top20_renew_energy.png"), scaling=3, width =1350, height = 1800, units="px")
grid.arrange(mylegend, arrangeGrob(increased + theme(legend.position="none"),
                                   decreased + theme(legend.position="none"),
                                   nrow=2), nrow=2, heights=c(1, 10)) 
dev.off()  
