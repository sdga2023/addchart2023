# library(showtext)
# font_add("Andes", "Andes-Regular.otf", bold="Andes-Bold.otf")
# font_add("OpenSans", "OpenSans-Regular.ttf", bold = "OpenSans-Bold.ttf")

#' @export
style_base <- function(textsize=12) {
  list(
    gg_text_size = grid::convertX(grid::unit(textsize, "points"), "mm", valueOnly = TRUE),
    gg_max_point_size = grid::convertX(grid::unit(0.1, "npc"), "mm", valueOnly = TRUE),
    theme_map = function(aspect_ratio = 1) {
      t <- theme(
        panel.grid = element_blank(),
        axis.text = element_blank()
      )
      
      if (aspect_ratio > 1) {
        t + theme(
          legend.position = "right",
          legend.direction = "vertical",
          legend.justification = c(1, 1),
          legend.key.width = unit(1, "lines")
        )
      } else {
        t + theme(
          legend.position = "top",
          legend.direction = "horizontal",
          legend.justification = c(0.5, 1),
          legend.key.width = unit(1.5, "lines"),
          legend.text = element_text(margin=margin(0,3,0,0,"lines"))
          #legend.margin = margin(0,0,0,1,"lines")
        )
      }
    },
    theme_bubble_map = function() {
      theme(
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm"),
        axis.text = element_blank(),
        legend.position = c(0,0),
        legend.direction = "vertical",
        legend.justification = c(0, 0)
      )
    },
    theme_barchart = function() {
      theme(
        axis.text.y = element_text(face="plain"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = theme_minimal()$panel.grid.major.x
      )
    },
    theme_x_title = function() {
      theme(
        axis.title.x = element_text(margin = margin(1,0,0,0,"lines"))
      )
    },
    theme_scatter = function() {
      theme(
        axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
        panel.grid.major.x = NULL
      )
    },
    theme_legend = function(position = "top") {
      listy(
        top = theme(
          legend.position = "top",
          legend.margin = margin(0,0,0,0, "lines")
        ),
        topleft = top + theme(legend.justification = c(0, 0.5)),
        right = theme(
          legend.position = "right",
          legend.margin = margin(0,0,0,0.5, "lines")
        ),
        righttop = right + theme(legend.justification = c(0.5, 1)),
        rightbottom = right + theme(legend.justification = c(0.5, 0)),
        bottom = theme(
          legend.position = "bottom",
          legend.margin = margin(0.3,0,0,0, "lines")
        ),
        left = theme(
          legend.position = "left",
          legend.margin = margin(0,0.5,0,0, "lines")
        ),
        lefttop = left + theme(legend.justification = c(0.5, 1))
      )[position]
    }
  )
}

#' @export
style_atlas <- function(textsize=12, family="Open Sans", family.alt="Andes", family.bold = "Open Sans", is.cmyk = FALSE) {
  modifyList(style_base(textsize), listy(
    ## FONTY STUFF #############################################################
    family = family,
    ## COLORS ##################################################################
    colors = listy(
      neutral                  = "grey80",
      text                     = "grey20",
      text.inverse             = "white",
  #  spot.primary             = if (!is.cmyk) "#509A9A"               else cmyk(2.7, 100, 58.6, 12.2, maxColorValue = 100),
      spot.primary             = if (!is.cmyk) "#616EA4"               else cmyk(2.7, 100, 58.6, 12.2, maxColorValue = 100),
      spot.primary.light       = if (!is.cmyk) lighten(spot.primary)   else cmyk(1.3, 50, 29.3, 6.1, maxColorValue = 100),
      spot.primary.dark        = if (!is.cmyk) darken(spot.primary)    else cmyk(0, 97, 68, 75, maxColorValue = 100),
      spot.secondary           = if (!is.cmyk) "gray30"                else cmyk(0, 0, 0, 80, maxColorValue = 100),
      spot.secondary.light     = if (!is.cmyk) lighten(spot.secondary) else cmyk(0, 0, 0, 50, maxColorValue = 100),
      spot.secondary.dark      = darken(spot.secondary),
      binary.pos               = if (!is.cmyk) "#2B98C6",
      binary.neg               = if (!is.cmyk) "#E56666",
      regions = c(
        EAS                    = if (!is.cmyk) "#ED8E3F"               else cmyk(0, 55, 90, 10, maxColorValue = 100),
        ECS                    = if (!is.cmyk) "#E56666"               else cmyk(2.7, 100, 58.6, 12.2, maxColorValue = 100),
        LCN                    = if (!is.cmyk) "#83B35C"               else cmyk(72, 5, 100, 20, maxColorValue = 100),
        MEA                    = if (!is.cmyk) "#9E2F59"               else cmyk(45, 83, 0, 20, maxColorValue = 100),
        NAC                    = if (!is.cmyk) "#22789D"               else cmyk(0, 0, 0, 80, maxColorValue = 100),
        SAS                    = if (!is.cmyk) "#509A9A"               else cmyk(80, 40, 0, 10, maxColorValue = 100),
        SSF                    = if (!is.cmyk) "#FFBF24"               else cmyk(0, 20, 100, 0, maxColorValue = 100)
      ),
      regions.wld = c(regions, c(WLD="#4E5783")),
      regions.light            = rgba2rgb(regions, alpha = 0.7, background = "white"),
      regions.dark             = rgba2rgb(regions, alpha = 0.7, background = "black"),
      incomes = c(
        HIC                    = spot.primary,
        UMC                    = spot.primary.light,
        LMC                    = spot.secondary.light,
        LIC                    = spot.secondary
      ),
      incomes.alt = c(
        HIC                    = "#1B607D",
        UMC                    = "#7B8AD0", 
        LMC                    = "#D36F8E",
        LIC                    = "#AA2626"
      ),
      gender = c(
        female                 = spot.primary,
        male                   = spot.secondary
      ),
      gender.alt = c(
        female                 ="#FFBF24",
        male                   ="#95A3DF"
        
      ),
      urban_rural = c(
        urban                  = "#616EA4",#spot.primary,
        rural                  = "#719950"#spot.secondary
      ),
      categorical = c(
        spot.primary,
        spot.secondary,
        spot.primary.light,
        spot.secondary.light,
        spot.primary.dark,
        spot.secondary.dark
      ),
      categorical.alt = c(
        "#E56666",
        "#FFBF24",
        "#ED8E3F",
        "#ABD786",
        "#509A9A",
        "#4E5783"
      ),
      sdg = c(
        sdg1 = "#e5243b",
        sdg2 = "#dda63a",
        sdg3 = "#4c9f38",
        sdg4 = "#c5192d",
        sdg5 = "#e7412e",
        sdg6 = "#26bde2",
        sdg7 = "#fcc30b",
        sdg8 = "#a21942",
        sdg9 = "#fd6925",
        sdg10 = "#dd1367",
        sdg11 = "#fd9d24",
        sdg12 = "#bf8b2e",
        sdg13 = "#3f7e44",
        sdg14 = "#0a97d9",
        sdg15 = "#56c02b",
        sdg16 = "00689d",
        sdg17 = "#19486a"
      ),
      
      reference                = "grey70",
      baseline                 = "black",
      continuous.primary       = function(n) {
        # g <- c("#ABD786","#FFBF24","#ED8E3F","#9E2F59") # For diverging scale (sdg 16_1)
      # g <- c("#E56666","#FCC8C5","#95CBE2","#2b98c6") # For diverging scale
      g <- scales::gradient_n_pal(c("white", spot.primary.light, spot.primary), values = c(0, 0.55, 1))
        max_pale <- 0.20
      if      (n == 1) g(c(1))
      else if (n == 2) g(c(0.55, 1))
      else             g(max_pale + (0:(n-1))/(n-1)*(1 - max_pale))
      },
      continuous.primary.dark  = function(n) {
        g <- scales::gradient_n_pal(c("white", spot.primary.light, spot.primary, spot.primary.dark))
        max_pale <- 0.20
        g(max_pale + (0:(n-1))/(n-1)*(1 - max_pale))
      },
      continuous.secondary       = function(n) {
        g <- scales::gradient_n_pal(c("white", spot.secondary.light, spot.secondary), values = c(0, 0.55, 1))
        max_pale <- 0.20
        if      (n == 1) g(c(1))
        else if (n == 2) g(c(0.55, 1))
        else             g(max_pale + (0:(n-1))/(n-1)*(1 - max_pale))
      },
      continuous               = continuous.primary
     
    ),
    
    # Region labels
    labels = listy(
      region.lb = 
        c('EAS'="East Asia & Pacific",
          'ECS'="Europe & Central Asia", 
          'LCN'="Latin America & Caribbean",
          'MEA'="Middle East & North Africa",  
          'NAC'="North America",  
          'SSF'="Sub-Saharan Africa", 
          'SAS'="South Asia"
      ),
      region.wld.lb = c(region.lb,
                         c('WLD'="World")),
      income.lb = 
        c('HIC'='High income',
          'UMC'='Upper middle income',
          'LMC'='Lower middle income',
          'LIC'='Low income')
      
      ),
    
    ## SHAPES & LINES ##########################################################
    shapes = list(
      point = 16 # has no stroke by default
      #incomes = c(
      #  HIC                    = 21,
      #  UMC                    = 21,
      #  LMC                    = 1,
      #  LIC                    = 1
      #)
      #categorical = c(
      #  19,
      #  19,
      #  1,
      #  1,
      #  1
      #)
    ),
    point_size = 2,
    point_stroke = 0,
    #point_stroked_stroke = 1,
    #point_stroked_size = 0.75,
    linetypes = list(
      regions = c(
        EAS = "solid",
        ECS = "solid",
        LCN = "solid",
        MEA = "solid",
        NAC = "solid",
        SAS = "solid",
        SSF = "solid"
      ),
      world = c(WLD = "12"),
      reference = "longdash",
      baseline = "solid"
    ),
    linesize = 0.8,
    linesize_reference = 0.4,
    arrow = function(ends = "last") { grid::arrow(length = unit(1.5, "mm"), type = "closed", ends = ends) },
    
    ## SHAPES & LINES ##########################################################
    theme = function() {
      theme_minimal() +
        theme(text                 = element_text(family = family, size=textsize, color=colors$text),
              line                 = element_line(size = 0.35),
              panel.grid.major.x   = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y   = element_blank(),
              plot.caption         = element_text(hjust=0, size=rel(6/7), lineheight = 1, margin=margin(0,0,0,0, unit="line"), color= "grey60"),#if (is.cmyk) "grey80" else "black"),
              plot.title           = element_text(size = rel(.8), color=colors$text),
              # plot.title           = element_text(hjust=0, size=rel(10/7), lineheight = 1, family=family, face="bold", color = "black"),
              plot.subtitle        = element_text(hjust=0, size=rel(8/7), lineheight = 1, color=colors$text),
              strip.text           = element_text(hjust = 0, size=rel(.75), lineheight = 1, color=colors$text), # strip text is NOT kind of like subtitle text
              # axis.text            = element_blank(),
              axis.text            = element_text(size = rel(.75), color=colors$text),
              axis.text.y          = element_text(color=colors$text),
              axis.text.x          = element_text(color=colors$text),
              axis.title           = element_text(size = rel(.8), color=colors$text),
              axis.title.x         = element_blank(),
              axis.title.x.top    = element_blank(),
              axis.title.x.bottom = element_blank(),
              axis.title.y         = element_blank(),
              axis.title.y.left   = element_blank(),
              axis.title.y.right  = element_blank(),
              legend.box.spacing   = unit(0.1, "lines"),
              legend.margin        = margin(0,0,0.1,0, "lines"),
              legend.title         = element_blank(),
              # legend.key.size      = unit(1.5*textsize, "points"),
              legend.key.size      = unit(.7,"line"),
              legend.text          = element_text(size = rel(.7), lineheight = 0.8, color=colors$text),
              legend.background    = element_rect(fill = "white", color = NA),
              legend.position      = "none",
              strip.placement      = "outside",
              plot.margin          = margin(1,1,1,1, unit = "mm") #trbl
        )
    },

    theme.no.axis = function() {
      theme_minimal() +
        theme(text                 = element_text(family = family, size=textsize, color=colors$text),
              line                 = element_line(size = 0.35),
              panel.grid.major.x   = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y   = element_blank(),
              plot.caption         = element_text(hjust=0, size=rel(6/7), lineheight = 1, margin=margin(1.5,0,0,0, unit="line"), color= if (is.cmyk) "grey60" else "black"),
              plot.title           = element_text(hjust=0, size=rel(10/7), lineheight = 1, family=family.alt, face="bold", color = "black"),
              plot.subtitle        = element_text(hjust=0, size=rel(8/7), lineheight = 1, color=colors$text),
              strip.text           = element_text(hjust = 0, size=rel(1.0), lineheight = 1, color=colors$text), # strip text is NOT kind of like subtitle text
              # axis.text            = element_blank(),
              axis.text.y          = element_text(color=colors$text),
              axis.text.x          = element_text(color=colors$text),
              axis.title           = element_text(size = rel(1.0), color=colors$text),
              axis.title.x         = element_blank(),
              #axis.title.x.top    = element_blank(),
              #axis.title.x.bottom = element_blank(),
              axis.title.y         = element_blank(),
              #axis.title.y.left   = element_blank(),
              #axis.title.y.right  = element_blank(),
              legend.box.spacing   = unit(0.2, "lines"),
              legend.margin        = margin(0,0,0.3,0, "lines"),
              legend.title         = element_blank(),
              legend.key.size      = unit(1.5*textsize, "points"),
              legend.text          = element_text(size = rel(1.0), lineheight = 0.8, color=colors$text),
              legend.background    = element_rect(fill = "white", color = NA),
              legend.position      = "none",
              strip.placement      = "outside",
              plot.margin          = margin(1,1,5,1, unit = "mm") #trbl
        )
    }
    
  ))
}

#' @export
style_atlas_cmyk <- purrr::partial(style_atlas, is.cmyk=TRUE)


#' @export
style_atlas_open <- function(textsize=7, ...) {
  style_atlas(textsize=textsize, family="sans", family.bold = "sans", ...)
}


#' @export
style_worldbank.org <- function(textsize=7) {
  modifyList(style_base(textsize), list(
    theme = function() {
      theme_minimal() +
        theme(text = element_text(family = "Open Sans", size = textsize, color="#333333"),
              panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              #legend.position = "none",
              plot.caption=element_text(hjust=0, size=rel(0.9), margin=margin(1.5,0,0,0, unit="line")),
              plot.title=element_text(hjust=0, size=rel(1.15), face="bold"),
              plot.subtitle = element_text(hjust=0,size=rel(1.0)),
              axis.text=element_text(size=rel(1.0)),
              axis.text.y=element_text(face="bold"),
              axis.title=element_blank(),
              plot.margin=unit(c(5,5,5,0),"mm"),
              legend.title = element_blank())
    },
    colors = list(
      spot.primary = "#0071bc",
      spot.secondary = "#009fda",
      spot.secondary.light = "#a5e8ff",
      regions =
        c(EAS = "#0071bc",
          ECS = "#009fda",
          LCN = "#a5e8ff",
          MEA = "#0071bc",
          NAC = "#009fda",
          SAS = "#a5e8ff",
          SSF = "#0071bc"),
      categorical = c(
        "#0071bc",
        "#009fda",
        "#a5e8ff",
        "#9e9f9e",
        "#686868"
      ),
      world = "black",
      continuous = function(n) { scales::seq_gradient_pal(low = "white", high = "#0071bc")((1:n)/n) }
    ),
    shapes = list(
      categorical = c(
        19,
        19,
        1,
        1,
        1
      )
    )
  ))
}
