
# ==== Download Girl Genius comic ====
# #### ~~~~~~~~~~~~~~~~~~~~~~~~~~ ####


# ~~~ RUN : START ~~~ ####
run.start=Sys.time()
cat(paste0("Run : ",
           format(Sys.Date(),
                  "%Y.%m.%d"),
           " @ ",
           format(Sys.time(),
                  "%H:%M")))


# ~~~ PARAMS & INFO ~~~ ####

# PARAM : start date ####
param.start_date = "https://www.girlgeniusonline.com/comic.php?date=20021104"

# PARAM : Destination folder ####
param.folder_dest = "C:/Users/Public/.BOOKZ/COMIX/Girl Genius/"

# PARAM : Number of comics to download ####
param.basket_size = 9999


# ~~~ AUX FUNS ~~~ ####

# FUN : TIMING + MESSAGE ####
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

# FUN : Download current comic and identify next one's date ####
f.gg = function (.url,
                 .blank) {
  
  if (length(.url)>0)
  {.img = read_html(.url) %>%
    html_elements("img[alt='Comic']") %>%
    html_attr("src")
  
  .file = str_split_i(.img,"/",-1)
  
  .img_date = str_split_i(.file,"\\.",1) %>%
    str_remove_all("[:alpha:]") %>%
    lubridate::as_date(format="%Y%m%d") %>%
    unique()
  
  download.file(url=.img,
                destfile=paste0(param.folder_dest,
                                .file),
                silent=TRUE,
                cacheOK=FALSE,
                method="libcurl",
                mode="wb")
  
  {if (.img_date <= today())
    return(read_html(.url) %>%
             html_elements("a[title='The Next Comic']") %>%
             html_attr("href") %>%
             unique())}}
}


# ~~~ INIT ~~~ #####
  # [ SECTION START ] ####
  section.start=Sys.time()
  cat(sep="\n",
      "\nINIT\n")

# ... packages ####
suppressMessages(if (!require("pacman")) utils::install.packages("pacman"))
pacman::p_load(tidyverse,rvest)

  # [ SECTION END ] ####
  f.timing(.message="Init completed in ")


# ~~~ ACTION : Download comics ~~~ ####

  # [ SECTION START ] ####
  section.start=Sys.time()
  cat(sep="\n",
      "\nDownloading images\n")

purrr::accumulate(seq(param.basket_size),
                  f.gg,
                  .init=param.start_date)

  # [ SECTION END ] ####
  f.timing(.message="Download completed in ")


# ~~~ RUN : END ~~~ ####
f.timing(.start=run.start,
         .message="RUNTIME : ",
         .sep.rows=2)

cat(sep="\n\n",
    "\n------------- END -------------")
