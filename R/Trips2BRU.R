
# ==== Trips to Brussels (Jan-Jul 2023) ====
# #### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

# ~~~ SETUP ~~~ ####
source("./R/_SETUP.R")



data.trips2bru = tibble(date=seq.Date(from=as.Date("2023/1/16"),
                                      to=as.Date("2023/7/31"),
                                      by="day")) %>%
  mutate(YEAR=lubridate::year(date),
         MONTH=lubridate::month(date),
         WEEK=lubridate::isoweek(date),
         WEEKDAY=lubridate::wday(date,
                                 label=TRUE,
                                 week_start=1),
         unit.meeting=date %in% as.Date(c("23/01/2023",
                                          "06/02/2023",
                                          "20/02/2023",
                                          "06/03/2023",
                                          "20/03/2023",
                                          "03/04/2023",
                                          "17/04/2023",
                                          "15/05/2023",
                                          "12/06/2023",
                                          "26/06/2023",
                                          "10/07/2023",
                                          "24/07/2023"),
                                        format="%d/%m/%Y"),
         holiday=date %in% as.Date(c("06/04/2023",
                                     "07/04/2023",
                                     "10/04/2023",
                                     "01/05/2023",
                                     "09/05/2023",
                                     "18/05/2023",
                                     "19/05/2023",
                                     "29/05/2023",
                                     "21/07/2023")),
         team.meeting=WEEKDAY=="Thu" &
           !holiday) %>%
  left_join(eval(.) %>%
              group_by(YEAR,
                       WEEK) %>%
              summarize(is.unit.meeting=sum(unit.meeting)>0),
            by=c("YEAR","WEEK")) %>%
  mutate(aller=ifelse(is.unit.meeting,
                      unit.meeting | team.meeting,
                      date %in% (eval(.) %>%
                        filter(team.meeting) %>%
                        mutate(date=date %m-%
                                 days(1)) %>%
                        pull(date))),
         retour=unit.meeting | team.meeting) %>%
  group_by(trip=cumsum(aller)) %>%
  mutate(trip=ifelse(trip>0 &
                       cumsum(!retour)==min(cumsum(!retour)),
                     trip,
                     NA)) %>%
  filter(!is.na(trip)) %>%
  group_by(WEEK,
           trip=cumsum(aller)) %>%
  summarize(reason=case_when(sum(unit.meeting)>0 ~ "Unit meeting",
                             sum(team.meeting)>0 ~ "team meeting",
                             TRUE ~ ""),
            aller=min(date) %>%
              format("%d %b"),
            retour=max(date) %>%
              format("%d %b")) %>%
  ungroup() %>%
  mutate(overnight=retour>aller)
