
# ======= STAR WARS =======

# ---- SETUP ----
source("./OUTILS/CONFIG/SETUP.R")

## AUX FUN ----
f.image.uri=function(link,
                     size=169,
                     temp.file="./T E M P/temp.jpg"){
  options(download.file.method="libcurl",url.method="libcurl")
  download.file(url=link,
                destfile=temp.file,
                mode="wb")
  magick::image_write(magick::image_resize(
    magick::image_read(temp.file),paste0(size,"x",size)),
    path=temp.file)
  xfun::base64_uri(temp.file)
  #unlink(temp.file)
}
Vf.image.uri=Vectorize(f.image.uri)

# ---- DATA ----
# https://www.rottentomatoes.com/franchise/star_wars_saga
# https://www.pocket-lint.com/tv/news/star-wars/147767-star-wars-machete-best-movie-viewing-order

data.StarWars = read_delim("DATA/StarWars.txt","\t",
                           escape_double=FALSE,
                           trim_ws=TRUE)

#StarWars.logo="https://i0.wp.com/www.4ye.co.uk/wp-content/uploads/2014/05/Star-Wars-Logo-Art.jpg"
StarWars.logo="https://download.logo.wine/logo/Star_Wars/Star_Wars-Logo.wine.png"
RottenTomatoes.logo="https://www.rottentomatoes.com/assets/pizza-pie/images/rtlogo.9b892cff3fd.png"

# ---- VIZ ----

girafe(ggobj=(data.StarWars %>%
         ggplot(aes(x=`rotten tomatoes`,
                    y=plot.order)) +
         geom_rect_interactive(aes(xmin=-Inf,xmax=Inf,
                       ymin=min(.data$plot.order[.data$series=="Original Trilogy"])-0.5,
                       ymax=max(.data$plot.order[.data$series=="Original Trilogy"])+0.5),
                   data_id="Original Trilogy",
                   fill="lightgrey",
                   alpha=0.4) +
           geom_rect_interactive(aes(xmin=-Inf,xmax=Inf,
                         ymin=min(.data$plot.order[.data$series=="Prequels Trilogy"])-0.5,
                         ymax=max(.data$plot.order[.data$series=="Prequels Trilogy"])+0.5),
                         data_id="Prequels Trilogy",
                         fill="lightgrey",
                     alpha=0.4) +
           geom_point_interactive(aes(data_id=series,
                                    tooltip=paste0("<img src=\"",
                                                   Vf.image.uri(poster),"\">"))) +
         geom_path(data=with(data.StarWars,
                             spline(y=`rotten tomatoes`,
                                    x=plot.order,
                                    n=99)) %>%
                     as.data.frame(),
                   aes(x=y,
                       y=x)) +
         geom_text_interactive(aes(label=movie,
                                   data_id=series,
                                   tooltip=paste0("<img src=\"",
                                                  Vf.image.uri(poster),"\">")),
                   family="Calibri",
                   fontface="italic",
                   hjust=1,
                   nudge_x=-1) +
           annotate("text",
                    x=50,
                    y=c(mean(.data$plot.order[.data$series=="Original Trilogy"],na.rm=TRUE),
                        mean(.data$plot.order[.data$series=="Prequels Trilogy"],na.rm=TRUE)),
                    label=c("Original Trilogy","Prequels Trilogy"),
                    hjust=1,
                    angle=90) +
           annotation_custom(grid::rasterGrob(
             magick::image_read("./OUTILS/MIX/(Oriza Trizniak).png"),
                                              interpolate=TRUE),
                             xmin=-3,
                             xmax=-1,
                             ymin=-3,
                             ymax=-1) +
           coord_cartesian(clip="off") +
           scale_x_continuous(limits=c(0,100)) +
         scale_y_continuous(position="right",
                            breaks=c(data.StarWars$plot.order,
                                     max(data.StarWars$plot.order)+1),
                            labels=c(data.StarWars$`release year`,
                                     "Release Year"),
                            limits=c(NA,
                                     max(data.StarWars$plot.order)+1),
                            sec.axis=sec_axis(~.,
                                              name=paste0("&#8680;",
                                                          " <span style='color:transparent'>.</span> ",
                                                          "Plot Order  &#8680;"))) +
         labs(title=paste0("<img src='",StarWars.logo,"' width='69' align='top'/>"),
              subtitle="A Winding Road to Greatness (and back)",
              x="Rotten Tomatoes score") +
         theme(plot.title=element_markdown(),
               axis.title.y.left=element_markdown(face="plain"),
               axis.title.y.right=element_blank(),
               axis.text.y.right=element_markdown(),
               axis.line.y.left=element_blank(),
               panel.border=element_blank(),
               plot.margin=unit(c(1,1,3,1),"lines"))),
       options=list(opts_tooltip(css="font-size:13px;font-family:Calibri;background-color:white;color:#273749;;padding:9px;border-radius:4px"),
                    opts_sizing(rescale=TRUE),
                    opts_zoom(min=0.1,max=2),
                    opts_toolbar(pngname="StarWars"),
                    opts_hover_inv(css="opacity:0;"),
                    opts_hover(css="stroke-width:2;"),
                    opts_selection(css="stroke-width:6px;",
                                   type="single",
                                   only_shiny=FALSE,
                                   selected=character(0))),
       fonts=list(sans="Calibri"),
       width_svg=7,
       height_svg=6)


magick::image_annotate(image_read('http://jeroen.github.io/images/tiger.svg'),
                       "CONFIDENTIAL", size = 30, color = "red", boxcolor = "pink",
               degrees = 60, location = "+50+100")
