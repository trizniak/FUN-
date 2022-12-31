
# ==== SETUP ====
# #### ~~~~~ ####

.connect2internet("OrizaT",
                  method="libcurl")

# ~~~ OPTIONS ~~~ ####
# ... Code appearance ([Tools] [Global Options] [Appearance]) : Tomorrow Night Blue

options(repr.plot.width=49,
        repr.plot.height=36,
        scipen=999,
        digits=1,
        warn=-1,
        dplyr.summarise.inform=FALSE) # suppress additional info


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
  lubridate,      #[OUTILS]       https://cran.r-project.org/web/packages/lubridate/index.html
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

# ... Necessary on business machine ¯\_(ツ)_/¯ ####
library(dplyr)
library(tibble)


# ~~~ FONTS ~~~ ####
pacman::p_load(extrafont)
#extrafont::font_import(prompt=FALSE)
grDevices::windowsFonts(KLB=grDevices::windowsFont("Calibri"))


# ~~~ COLOR PALETTES ~~~ ####

palette.personal = c(main.dark="#273749",
                     main.medium="#446699",
                     main.light="#a1b3c9")


# ~~~ THEME ~~~ ####
my.theme = function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(text=element_text(family="KLB",
                                     color=palette.personal["main.dark"]),
                   axis.line.x.bottom=element_line(color=palette.personal["main.light"],
                                                   size=.3),	# set as element_blank to remove : axis.line is ignored
                   axis.line.y.left=element_line(color=palette.personal["main.light"],
                                                 size=.3),	# set as element_blank to remove : axis.line is ignored
                   axis.text=element_blank(),
                   axis.ticks=element_blank(),
                   axis.title=element_text(face="italic"),
                   legend.title=element_blank(),
                   panel.background=element_blank(),
                   panel.border=element_rect(size=0.1,
                                             color=palette.personal["main.light"],
                                             fill=NA),
                   panel.grid=element_blank(),
                   panel.spacing=unit(0.1,"lines"),
                   plot.title=element_markdown(),
                   plot.title.position="plot",
                   plot.subtitle=element_markdown(),
                   strip.background=element_blank(),
                   strip.placement="outside",
                   strip.text=element_text(color=palette.personal["main.dark"],
                                           face="italic"))
}

ggplot2::theme_set(my.theme())


# ~~~ LABELS ~~~ ####


# ~~~ AUX FUNS ~~~ ####

# * FUN : TIMING + MESSAGE ####
f.timing = function (.start=section.start,
                     .message,
                     .sep.rows=1) {
  cat(sep="\n",
      paste0(rep("\n",.sep.rows) %>%
               paste(collapse=""),
             .message,
             lubridate::int_diff(c(.start,
                                   Sys.time())) %>%
               lubridate::int_length() %>%
               round() %>%
               lubridate::seconds_to_period(),
             rep("\n",.sep.rows) %>%
               paste(collapse="")))
}

# * FUN : Label Color ####
f.label.color = function(x,
                         color.negative="red", # palette.ESTAT["red.3"]
                         color.neutral="lightgrey", # palette.ESTAT["teal.3"]
                         color.pozitive="green4") { # palette.ESTAT["green.3"]
  paste0("<b><span style='color:",
         dplyr::case_when(x<0~color.negative,
                          x>0~color.pozitive,
                          TRUE~color.neutral),
         "'>",x,"</span>")}
# scale_y_continuous(labels=function (X) f.label.color(X,"midnightblue","#273749","magenta") / labels=f.label.color)

# * FUN : Pretty Rounding ####
f.pretty.round = function (x,
                           step=5) {
  E=ifelse(x==0,0,
           floor(log10(abs(x))-1))
  F=x/10^E
  step*ceiling(F/step)*10^E
}

# FUN : Percentage rounding to 0.5, and dropping trailing zeros ####
f.pct.round = function(x,
                       step=5) {
  paste0(prettyNum(step*round(x/step,1),
                   digits=2,
                   format="g"),
         "%")
}

# * FUN : Get Eurostat data ####
# https://stackoverflow.com/questions/59796178/r-curlhas-internet-false-even-though-there-are-internet-connection
f.data.estat = function(.filename,
                        .lag=0,
                        .filter=FALSE) {
  # http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=ilc_di01&lang=en
  eurostat::get_eurostat(.filename,
                         time_format="num",
                         keepFlags=TRUE) %>%
    dplyr::rename(COUNTRY=geo,
                  YEAR=time,
                  level=values) %>%
    {if (.filter)
      dplyr::filter(COUNTRY %in% names(country.list),
                    YEAR>=2005,
                    !is.na(level))
      else .} %>%
    dplyr::mutate(YEAR=YEAR-.lag,
                  d.break=replace_na(str_detect(tolower(flags),"b"),FALSE),
                  d.estimate=replace_na(str_detect(tolower(flags),"e"),FALSE),
                  d.provisional=replace_na(str_detect(tolower(flags),"p"),FALSE),
                  COUNTRY=as.character(COUNTRY))
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
