
# ---- SETUP ----

# Code appearance ([Tools] [Global Options] [Appearance]) : Tomorrow Night Blue

options(repr.plot.width=49,
        repr.plot.height=36,
        scipen=999,
        digits=2,
        warn=-1)
my.color="#273749" # 214263 0B4279 0b2131 1b3142


# ---- PACKAGES ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  devtools,	#[.KEY]		https://cran.r-project.org/web/packages/devtools/index.html
  here,		#[.KEY]		https://cran.r-project.org/web/packages/here/index.html
  readr,		#[.KEY]		https://cran.r-project.org/web/packages/readr/index.html
  scales,		#[.KEY]		https://cran.r-project.org/web/packages/scales/index.html
  # smacof,	#[ANALYTICS]	https://cran.r-project.org/web/packages/smacof/index.html
  # pheatmap,	#[ANALYTICS]	https://cran.r-project.org/web/packages/pheatmap/index.html
  # docxtractr,	#[DATA]		https://cran.r-project.org/web/packages/docxtractr/index.html
  # eurostat,	#[DATA]		https://cran.r-project.org/web/packages/eurostat/index.html
  readxl,	#[DATA]		https://cran.r-project.org/web/packages/readxl/index.html
  # restatapi,	#[DATA]		https://cran.r-project.org/web/packages/restatapi/index.html
  # crosstalk,	#[INTERACTIVE]	https://cran.r-project.org/web/packages/crosstalk/index.html
  glue,		#[OUTILS]	https://cran.r-project.org/web/packages/glue/index.html
  janitor,	#[OUTILS]	https://cran.r-project.org/web/packages/janitor/index.html
  rlang,	#[OUTILS]	https://cran.r-project.org/web/packages/rlang/index.html
  rvest,	#[OUTILS]	https://cran.r-project.org/web/packages/rvest/index.html
  withr,	#[OUTILS]	https://cran.r-project.org/web/packages/withr/index.html
  # DT,		#[TAB]		https://cran.r-project.org/web/packages/DT/index.html
  flextable,	#[TAB]		https://cran.r-project.org/web/packages/flextable/index.html
  gt,	#[TAB]		https://cran.r-project.org/web/packages/gt/index.html
  huxtable,	#[TAB]		https://cran.r-project.org/web/packages/huxtable/index.html
  kableExtra,	#[TAB]		https://cran.r-project.org/web/packages/kableExtra/index.html
  # reactable,	#[TAB]		https://cran.r-project.org/web/packages/reactable/index.html
  # dtwclust,	#[TIME SERIES]	https://cran.r-project.org/web/packages/dtwclust/index.html
  # fable,        	#[TIME SERIES]	https://cran.r-project.org/web/packages/fable/index.html
  # feasts,	        #[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  # slider,	#[TIME SERIES]	https://cran.r-project.org/web/packages/feasts/index.html
  # tsibble,	#[TIME SERIES]	https://cran.r-project.org/web/packages/tsibble/index.html
  # urca,        	#[TIME SERIES]	https://cran.r-project.org/web/packages/urca/index.html
  # zoo,       	#[TIME SERIES]	https://cran.r-project.org/web/packages/zoo/index.html
  ggh4x,	#[VIZ]		https://cran.r-project.org/web/packages/ggh4x/index.html
  ggforce,	#[VIZ]		https://cran.r-project.org/web/packages/ggforce/index.html
  ggimage,	#[VIZ]		https://cran.r-project.org/web/packages/ggimage/index.html
  ggiraph,	#[VIZ]		https://cran.r-project.org/web/packages/ggiraph/index.html
  # ggplotify,	#[VIZ]		https://cran.r-project.org/web/packages/ggplotify/index.html
  ggpubr,	#[VIZ]		https://cran.r-project.org/web/packages/ggpubr/index.html
  # ggrepel,	#[VIZ]		https://cran.r-project.org/web/packages/ggrepel/index.html
  # gplots,	#[VIZ]		https://cran.r-project.org/web/packages/gplots/index.html
  ggtext,		#[VIZ]		https://cran.r-project.org/web/packages/ggtext/index.html
  # highcharter,	#[VIZ]		https://cran.r-project.org/web/packages/highcharter/index.html
  magick,	#[VIZ]		https://cran.r-project.org/web/packages/magick/index.html
  patchwork,	#[VIZ]		https://cran.r-project.org/web/packages/patchwork/index.html
  # plotly,	#[VIZ]		https://cran.r-project.org/web/packages/plotly/index.html
  # plotrix,	#[VIZ]		https://cran.r-project.org/web/packages/plotrix/index.html
  ragg,        	#[VIZ]		https://cran.r-project.org/web/packages/ragg/index.html
  webshot,        	#[VIZ]		https://cran.r-project.org/web/packages/webshot/index.html
  tidyverse	#[.KEY]]	https://cran.r-project.org/web/packages/tidyverse/index.html
)
#webshot::install_phantomjs(force=FALSE)
#remotes::install_github("clauswilke/sinab")
library(sinab)


# ---- FONTS ----
pacman::p_load(extrafont)
#extrafont::font_import(prompt=FALSE)


# ---- THEME ----
my.theme = function() {
  theme_minimal() +
    theme(text=element_text(family="Calibri",
                            color=my.color),
          axis.line.x.bottom=element_line(color="grey",
                                          size=.3),	# set as element_blank to remove : axis.line is ignored
          axis.line.y.left=element_line(color="grey",
                                        size=.3),	# set as element_blank to remove : axis.line is ignored
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_text(face="italic"),
          legend.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_rect(size=0.1,
                                    color="grey",
                                    fill=NA),
          panel.grid=element_blank(),
          panel.spacing=unit(0.1,"lines"),
          plot.title=element_markdown(size=26),
          plot.title.position="plot",
          plot.subtitle=element_markdown(),
          strip.background=element_blank(),
          strip.placement="outside",
          strip.text=element_text(color=my.color,
                                  face="italic"))
}

theme_set(my.theme())


# ---- AUXILIARY FUNS ----

# * FUN : Label Color ----
f.label.color = function(x,
                         sign=FALSE,
                         fontface="bold",
                         color.negative="red",
                         color.neutral="grey",
                         color.pozitive="green4") {
  paste0({if (fontface=="bold") "<b>" else ""},
         "<span style='color:",
         case_when(x<0~color.negative,
                   x>0~color.pozitive,
                   TRUE~color.neutral),
         "'>",
         #{if(sign & x>0) "+" else ""},
         x,"</span>",
         {if (fontface=="bold") "</b>" else ""})}
# scale_y_continuous(labels=function (X) f.label.color(X,"midnightblue","#273749","magenta") / labels=f.label.color)

# ---- FUN : Pretty Rounding ----
f.pretty.round = function (x,step=5) {
  E=ifelse(x==0,0,floor(log10(abs(x))-1))
  F=x/10^E
  step*ceiling(F/step)*10^E
}

# ---- FUN : element_html ----
# https://stackoverflow.com/questions/63912586/is-it-possible-to-change-the-alignment-of-only-1-facet-title
element_html <- function(css = NULL, family = NULL, face = NULL, size = NULL, colour = NULL, fill = NULL,
                         linetype = NULL, linewidth = NULL, hjust = NULL, vjust = NULL, lineheight = NULL,
                         margin = NULL, width = NULL, height = NULL, color = NULL, 
                         debug = FALSE, inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  
  # doesn't work with other values at this time
  hjust <- 0
  vjust <- 1
  
  structure(
    list(
      css = css,
      family = family, face = face, size = size, colour = colour, fill = fill,
      linetype = linetype, linewidth = linewidth, hjust = hjust, vjust = vjust,
      lineheight = lineheight, margin = margin, width = width, height = height,
      debug = debug, inherit.blank = inherit.blank),
    class = c("element_html", "element_text", "element")
  )
}

element_grob.element_html <- function(element, label = "", x = NULL, y = NULL,
                                      family = NULL, face = NULL, colour = NULL, size = NULL,
                                      hjust = NULL, vjust = NULL, lineheight = NULL,
                                      margin = NULL, ...) {
  if (is.null(label))
    return(ggplot2::zeroGrob())
  
  # for now we ignore hjust and vjust, it doesn't work yet
  hj <- 0
  vj <- 1
  
  css <- element$css %||% ""
  
  html_grob(
    label, x = x, y = y, hjust = hj, vjust = vj,
    width = element$width, height = element$height,
    css = css
  )
}

# ---- COLOR PALETTE ----

color.palette.CC = c('COCA-COLA'="darkred",
                     'DORNA'="darkred",
                     'FANTA'="darkred",
                     'FUZETEA'="darkred",
                     'IZVORUL ALB'="darkred",
                     'POIANA NEGRI'="red",
                     'SCHWEPPES'="darkred",
                     'SPRITE'="darkred",
                     'CAPPY'="darkred",
                     'PEPSI'="royalblue4",
                     '7UP'="royalblue4",
                     'LIPTON'="royalblue4",
                     'MIRINDA'="royalblue4",
                     'EVERVESS'="royalblue4",
                     'MOUNTAIN DEW'="steelblue",
                     'PRIGAT'="royalblue4",
                     'BUCOVINA'="khaki4",
                     'NESTEA'="khaki4",
                     'TYMBARK'="khaki4",
                     'AQUA CARPATICA'="darkslategray",
                     'FRUTTI FRESH'="mediumorchid",
                     'BORSEC'="orange")

# noquote(paste0("\'",enframe(color.palette.CC,name="brand",value="color") %>%
# mutate(color=recode(color,royalblue4="#27408b",steelblue="#4682b4",
# forestgreen="#228b22",mediumorchid="#ba55d3"),
# css=paste0(".",str_replace(brand," ","_")," { background-color: ",color,"; color: white; }")) %>%
# pull() %>%unlist() %>%paste(collapse="@"),"\'"))

# https://www.farb-tabelle.de/en/rgb2hex.htm
css.colors.CC = 'p { text-align: center; padding-top: 3px; font-family: Calibri; font-size: 26px; }
.COCA-COLA { background-color: darkred; color: white; }
.DORNA { background-color: darkred; color: white; }
.FANTA { background-color: darkred; color: white; }
.FUZETEA { background-color: darkred; color: white; }
.IZVORUL_ALB { background-color: darkred; color: white; }
.POIANA_NEGRI { background-color: red; color: white; }
.SCHWEPPES { background-color: darkred; color: white; }
.SPRITE { background-color: darkred; color: white; }
.CAPPY { background-color: darkred; color: white; }
.PEPSI { background-color: #27408b; color: white; }
.ZUP { background-color: #27408b; color: white; }
.LIPTON { background-color: #27408b; color: white; }
.MIRINDA { background-color: #27408b; color: white; }
.EVERVESS { background-color: #27408b; color: white; }
.MOUNTAIN_DEW { background-color: #4682b4; color: white; }
.PRIGAT { background-color: #27408b; color: white; }
.BUCOVINA { background-color: #8b864e; color: white; }
.NESTEA { background-color: #8b864e; color: white; }
.TYMBARK { background-color: #8b864e; color: white; }
.AQUA_CARPATICA { background-color: #2f4f4f; color: white; }
.FRUTTI_FRESH { background-color: #ba55d3; color: white; }
.BORSEC { background-color: orange; color: white; }'