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

# Card 1: UHC 
#Trends in SDI and catastrophic health spending at the 10% threshold by World Bank region, 2000-2017
wdi.dta <- function(ind0){
  dta <- wb_data(ind0, 
                 start_date=2000,
                 end_date=2017,
                 country="all") %>% 
    filter(!is.na(.data[[ind0]]))  

  dta <- dta[,2:5]
  return(dta)
}

# By income group
income <-unique(rg.ctr$income_level_iso2c)
income <-income[!is.na(income)&!grepl("XY",income)]

sci <- wdi.dta("SH.UHC.SRVS.CV.XD") %>%
  filter(iso3c%in%c(income,"WLD"))
hs10 <- wdi.dta("SH.UHC.OOPC.10.ZS") %>%
  filter(iso3c%in%c(income,"WLD"))

sci.re <- sci %>% 
  left_join(select(hs10, date,iso3c,SH.UHC.OOPC.10.ZS), by=c("iso3c","date")) %>%
  left_join(unique(select(rg.ctr,income_level_iso2c, income_level_iso3c)), by=c("iso3c"="income_level_iso2c")) %>%
  mutate(date=factor(date, levels=c("2000","2005","2010","2015","2017"))) %>%
  mutate(iso3c=ifelse(iso3c!="WLD",income_level_iso3c,iso3c))

# plot
sdg3.uhc <- figure(
  data=sci.re,
  plot = function(df, style = style_atlas()) {
    yr.label <- filter(sci.re, iso3c=="UMC")
    df <- sci.re 
    
      ggplot(data=df, aes(x=SH.UHC.SRVS.CV.XD, y=SH.UHC.OOPC.10.ZS, group=iso3c)) +
      geom_line(aes(color=iso3c),arrow=arrow(length=unit(0.2,"cm"), ends = "last"),show.legend = FALSE) +
      geom_point(aes(color=iso3c),shape=16, size=2) +
      annotate("text",x=yr.label$SH.UHC.SRVS.CV.XD[1],y=yr.label$SH.UHC.OOPC.10.ZS[1], 
               label = "2000",vjust=-.7, size=3,family=style$family ) +
        annotate("text",x=yr.label$SH.UHC.SRVS.CV.XD[2],y=yr.label$SH.UHC.OOPC.10.ZS[2], 
                 label = "2005",vjust=1.7, size=3,family=style$family ) +
        annotate("text",x=yr.label$SH.UHC.SRVS.CV.XD[3],y=yr.label$SH.UHC.OOPC.10.ZS[3], 
                 label = "2010",vjust=1.7, size=3,family=style$family ) +
        annotate("text",x=yr.label$SH.UHC.SRVS.CV.XD[4],y=yr.label$SH.UHC.OOPC.10.ZS[4], 
                 label = "2015",vjust=.4, hjust=1.2, size=3,family=style$family ) +
        annotate("text",x=yr.label$SH.UHC.SRVS.CV.XD[5],y=yr.label$SH.UHC.OOPC.10.ZS[5], 
                 label = "2017",vjust=.6, hjust=-.2, size=3,family=style$family ) +
        
      scale_x_continuous(breaks=seq(0,90,10), limits=c(0,90), expand=c(0,0.6)) +
      scale_y_continuous(breaks=seq(0,20,5), limits=c(0,21), expand=c(0,0)) +
        style$theme() +
        scale_color_manual(values=c(style$colors$incomes.alt, c('WLD'='grey70')),
                           labels=c(style$labels$income.lb, c("WLD"="World"))) +
        theme(legend.position="top")
  }
)

agg_png(paste0(charts.dir,"sdg3.uhc_income.png"), scaling=3, width =1350, height = 800, units="px")
sdg3.uhc
dev.off() 

# Card 2: Under-five mortality
cm.df <- wb_data(indicator = c("SH.DYN.MORT", "SH.DTH.MORT"),
                 start_date=2000,
                 end_date=2021,
                 country="all") %>%
  filter(iso3c%in%c("WLD",rgs$region_iso3c))


style<-style_atlas()

# u5mr plot
rate <- ggplot(cm.df,  
                   aes(x=date, y=SH.DYN.MORT, group=iso3c, color=iso3c)) +
  geom_hline(yintercept=25, linetype="dashed", color = "grey", size=.25) +
  annotate("text", x = 2001, y = 21, label = "SDG target for 2030", colour="grey", 
               size=rel(3), hjust=0) +
  geom_line() +
  scale_color_manual(values=c(style$colors$regions, c("WLD"="grey70")),
                     labels=style$labels$region.wld.lb) +
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2021), limits=c(2000,2021)) +
  style$theme() +
  theme(legend.position="bottom",
        legend.title=element_blank())

p.dth <- filter(cm.df, date==2021&iso3c!="WLD") %>%
  arrange(date) %>%
  mutate(lab=ifelse(iso3c%in%c("SSF","SAS"), 
                    paste0(country,"\n",scales::comma(round(SH.DTH.MORT/1000,0))),"")) 

p.dth$iso3c <- factor(p.dth$iso3c, levels=p.dth$iso3c) 

# deaths plot    
dth <- ggplot(data=p.dth,aes(x=1, y=SH.DTH.MORT, group=iso3c, fill=iso3c)) +
  geom_col()+
  coord_polar("y", start=0) +
  geom_text(aes(label = lab),
            position = position_stack(vjust = 0.25), family="Open Sans", size=3,
                color="white") +
  theme_void() +
  scale_fill_manual(values=style$colors$regions, labels=style$labels$region.lb) +
  theme(legend.position = "none",
        legend.title = element_blank())
    
# Create common legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(rate)

agg_png(paste0(charts.dir,"sdg3.u5mr_rev.png"), scaling=3, width =1350, height = 700, units="px")
grid.arrange(arrangeGrob(rate + theme(legend.position="none"),
                                           dth + theme(legend.position="none"),
                                           nrow=1, widths = c(6,4)), mylegend, nrow=2, heights=c(9, 1))
dev.off()     

# Card 3: Life expectancy 
le.df <- wb_data(indicator = c("SP.DYN.LE00.IN"),
                 start_date=2015,
                 end_date=2021,
                 country="countries_only") %>%
  filter(!is.na(SP.DYN.LE00.IN))

# plot
sdg3_life_expectancy <- figure(
  data=le.df,
  plot = function(df, style = style_atlas()) {
    cap <- -2  
    df <- le.df %>%
      select(iso3c:date, value=SP.DYN.LE00.IN) %>%
      spread(key=date, value=value) %>%
      left_join(select(rg.ctr,iso3c,region_iso3c), by="iso3c") %>%
      mutate(cat=ifelse(`2021`-`2019`<cap,"Dropped by\nmore than 2 years",
                        ifelse(is.na(`2021`),"Country","Country"))) %>%
      gather(key=date, value=value, `2015`:`2021`) %>%
      filter(!is.na(region_iso3c)) %>%
      filter(!is.na(value)) %>%
      mutate(date=as.numeric(date))
    df$cat[is.na(df$cat)] <-"Country" 
    
    ggplot(df, aes(x=date, y=value, group=iso3c)) +
      annotate("rect", xmin = 2019, xmax = 2021, ymin = 40, ymax = 90,
               alpha = .2,fill=style$colors$spot.primary.light) +
      geom_line(aes(color=cat, size=cat)) +
      scale_y_continuous(breaks=seq(40,90,10), limits=c(40,90),expand=c(0,0)) +
      scale_x_continuous(breaks=c(2015,2019,2021)) +
      scale_size_manual(values=c(0.1,.3),guide = guide_legend(reverse = TRUE))+
      facet_wrap(~ region_iso3c, ncol = 4, 
                 labeller = as_labeller(style$labels$region.wld.lb,label_wrap_gen(width=22)),
                 scale="free_x") +
      scale_color_manual(values=c("grey60",
                                  style$colors$spot.primary),
                         guide = guide_legend(reverse = TRUE)) +
      style$theme() +
      theme(legend.position = c(0.87, 0.25),
            panel.spacing.x = unit(.9, "lines"),
            axis.text.y = element_text(size=rel(.7)),
            axis.text.x = element_text(size=rel(.7), hjust=.6),
            strip.text=element_text(size=rel(.7)))
  }
)

agg_png(paste0(charts.dir,"sdg3_life_expectancy.png"), scaling=3, width =1350, height = 900, units="px")
sdg3_life_expectancy
dev.off()  

# Card4: MMR
mmr.df <- wb_data(indicator = c("SH.STA.MMRT", "SH.MMR.DTHS"),
                 start_date=2000,
                 end_date=2020,
                 country="all") %>%
  filter(iso3c%in%c("WLD",rgs$region_iso3c)) %>%
  filter(date%in%seq(2000,2020,5)) %>%
  mutate(SH.MMR.DTHS=SH.MMR.DTHS/1000)

mmr.order <- mmr.df %>%
  filter(country!="World"&date==2020) %>%
  arrange(desc(SH.STA.MMRT))

mmr.order <- c(mmr.order$iso3c,"WLD")

# Create color palette for rate plot
colfunc <- colorRampPalette(c(style$colors$spot.primary.dark, style$colors$spot.primary, style$colors$spot.primary.light))

p.ratio <- ggplot(mmr.df, aes(
  x = factor(iso3c, levels=mmr.order), y = SH.STA.MMRT, fill = as.factor(date))) +
  annotate("rect", ymin = 0, ymax = Inf, xmin = 7.5, xmax = 8.5,
           fill="grey",alpha = .3) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = str_wrap_lines(style$labels$region.wld.lb,2,force=TRUE)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(values = colfunc(5)) +
  style$theme() +
  theme(axis.text=element_text(size=rel(.75)))+
  style$theme_legend("top") +
  theme(plot.margin=unit(c(.25,.1,1,.1),"cm")) 

# deaths
mmdth.order <- mmr.df %>% 
  filter(date==2020&iso3c!="WLD") %>%
  arrange(SH.MMR.DTHS)
mmdth.order<-mmdth.order$iso3c

# Reorder legends 
col.re <-c()
for (i in mmdth.order) {
  c <-style$colors$regions[[i]] 
  col.re <- c(col.re, c)
}

p.dth <- ggplot(filter(mmr.df, date==2020&iso3c!="WLD"), 
                aes(fill=factor(iso3c, levels=mmdth.order), 
                    y=SH.MMR.DTHS, x=as.factor(date))) + 
  geom_bar(stat="identity", width = 1) +
  coord_flip() +
  scale_fill_manual(values=col.re,
                    labels=style$labels$region.lb,
                    guide=guide_legend(reverse = TRUE)) +
  scale_y_continuous(expand=c(0, 5), limits=c(0,300))+
  style$theme() +
  style$theme_barchart() +
  theme(axis.text.y=element_blank()) +
  style$theme_legend("bottom") +
  theme(plot.margin=unit(c(.5,.1,.25,.1),"cm"))


agg_png(paste0(charts.dir,"sdg3.mmr.png"), scaling=3, width =1350, height = 1100, units="px")
grid.arrange(p.ratio, p.dth, nrow=2, heights=c(7,3))
dev.off() 
