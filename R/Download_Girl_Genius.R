
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


# ~~~ INIT ~~~ #####
  # [ SECTION START ] ####
  section.start=Sys.time()
  cat(sep="\n",
      "\nINIT\n")
 
# ... packages ####
suppressMessages(if (!require("pacman")) utils::install.packages("pacman"))
pacman::p_load(tidyverse)

  # [ SECTION END ] ####
  f.timing(.message="Init completed in ")


# ~~~ PARAMS & INFO ~~~ ####

# PARAM : root url ####
param.root_url = "https://www.girlgeniusonline.com/ggmain/strips/"

# PARAM : Destination folder ####
param.folder_dest="C:/Users/Public/.BOOKZ/COMIX/Girl Genius/"


# ~~~ ACTION ~~~ ####

# ACT : Prepare list of images to download ####

  # [ SECTION START ] ####
  section.start=Sys.time()
  cat(sep="\n",
      "\nPreparing list of images to download\n")

list.pix = seq(lubridate::ymd("2002/11/01"),
               lubridate::ymd("2024/05/01"),
               "days") %>%
  stringr::str_remove_all("-") %>%
  paste0(param.root_url,"ggmain",.,".jpg") %>%
  .[which(purrr::map(.,
                     ~!is.null(httr::HEAD(.x)$headers$`content-length`)) %>%
            unlist)] %>%
  str_split_i("/",-1)

  # [ SECTION END ] ####
  f.timing(.message="List of images prepared in ")
 
# ACT : Download images ####

  # [ SECTION START ] ####
  section.start=Sys.time()
  cat(sep="\n",
      "\nDownloading images\n")

purrr::map(list.pix,
           ~try(download.file(url=paste0(param.root_url,.x),
                              destfile=paste0(param.folder_dest,.x),
                              silent=TRUE,
                              cacheOK=FALSE,
                              method="libcurl",
                              mode="wb")))

  # [ SECTION END ] ####
  f.timing(.message="Download completed in ")


# ~~~ RUN : END ~~~ ####
f.timing(.start=run.start,
         .message="RUNTIME : ",
         .sep.rows=2)

cat(sep="\n\n",
    "\n------------- END -------------")
