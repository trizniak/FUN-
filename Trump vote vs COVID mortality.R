
# ---- *** Trump vote vs COVID mortality *** ----

# ---- Inspiration ----
# https://twitter.com/_cingraham/status/1432733716632137731
# https://thewhyaxis.substack.com/p/gop-covid-policy-is-killing-gop-voters

# ---- SETUP ----
source("./SETUP.R")

# ---- DATA ----
data.TrumpVoteCOVID =
  tibble(vote=c("0-20%","20-40%","40-60%","60-80%","80-100%"),
         mortality=c(4.89,5.08,7.31,10.51,14.89)) %>%
  mutate(vote.n=str_remove(vote,"%")) %>%
  separate(vote.n,
           into=c("vote.lo",
                  "vote.HI"),
           convert=TRUE) %>%
  mutate(vote.mid=rowMeans(select(.,
                                  starts_with("vote.")),
                           na.rm=TRUE))
