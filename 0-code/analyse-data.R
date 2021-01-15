###########################################################################
###########################################################################
###                                                                     ###
###       Diversity in Economics Seminars: load and analyse data        ###
###                                                                     ###
###########################################################################
###########################################################################

##----------------------------------------------------------------
##              Install and load required packages               -
##----------------------------------------------------------------

if (!require("tidyverse", character.only = TRUE)) {
  install.packages("tidyverse", dependicies = TRUE)
}
library("tidyverse", character.only = TRUE)


##----------------------------------------------------------------
##                          Import data                          -
##----------------------------------------------------------------

ipeds <- read_csv("0-data/data.csv", col_names=TRUE, col_types=cols())
ipeds$repec_semdept <- ordered(ipeds$repec_semdept, levels=c("1-10", "11-25", "26-50", "51-100", "101-289", "Unranked"))
ipeds$repec_owndept <- ordered(ipeds$repec_owndept, levels=c("1-10", "11-25", "26-50", "51-100", "101-289", "Unranked"))

##----------------------------------------------------------------
##                            Table 1                            -
##----------------------------------------------------------------

##...................................
## Demographic breakdown of talks
##...................................
demographics_talks <- ipeds %>%
  group_by(urm_status) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="talk")
demographics_speakers <- ipeds %>%
  distinct(id, urm_status) %>%
  group_by(urm_status) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="speaker")
demographics <- demographics_talks %>% 
  bind_rows(demographics_speakers) %>% 
  mutate(variable="demographics") %>%
  rename(label=urm_status)

##...................................
## Seniority breakdown of talks
##...................................
seniority_talks <- ipeds %>%
  filter(!is.na(years_from_phd)) %>%
  mutate(label = cut(years_from_phd, breaks=c(-Inf,5,11, Inf), labels=c("< 6 years", "6--11 years", "12+ years"))) %>%
  group_by(label) %>%
  tally() %>%
  filter(!is.na(label)) %>%
  mutate(percent = n/sum(n), dimension="talk")
seniority_speakers <- ipeds %>%
  filter(!is.na(years_from_phd)) %>%
  group_by(id) %>%
  summarise(years_from_phd=max(years_from_phd, na.rm=TRUE)) %>%
  mutate(label = cut(years_from_phd, breaks=c(-Inf,5,11, Inf), labels=c("< 6 years", "6--11 years", "12+ years"))) %>%
  group_by(label) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="speaker")
seniority <- seniority_talks %>%
  bind_rows(seniority_speakers) %>%
  mutate(variable="seniority")

##...................................
## Host RePEc breakdown of talks
##...................................
repec_talks <- ipeds %>%
  group_by(repec_owndept) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="talk") %>%
  rename(label=repec_owndept)
repec_speakers <- ipeds %>%
  group_by(id) %>%
  mutate(label = min(repec_owndept, na.rm=TRUE)) %>%
  distinct(id, label) %>%
  group_by(label) %>%
  tally() %>%
  mutate(percent = n/sum(n), dimension="speaker")
repec <- repec_talks %>%
  bind_rows(repec_speakers) %>%
  mutate(variable="repec")

##...................................
## Number of talks
##...................................
n <- data.frame(
  dimension = c("talk", "speaker"),
  variable = c("n", "n"),
  label = c("\\(N\\)", "\\(N\\)"),
  n = c(nrow(ipeds), length(unique(ipeds$id)))
)

##...................................
## Export data for Table 1.
##...................................
write_csv(
  n %>% 
    bind_rows(demographics) %>% 
    bind_rows(seniority) %>% 
    bind_rows(repec) %>% 
    relocate(c(dimension, variable, label, n), .before=percent), 
  "0-data/table-1.csv"
)

##----------------------------------------------------------------
##                            Figure 1                           -
##----------------------------------------------------------------

##...................................
## Create time series
##...................................
demo_category <- ipeds %>%
  group_by(semester, school, urm_status) %>%
  tally() %>%
  mutate(percent = n/sum(n)) %>%
  ungroup() %>%
  complete(school, nesting(semester, urm_status), fill=list(n=0, percent=0)) %>%
  group_by(semester, urm_status) %>%
  summarise(`Department mean` = mean(percent), `Department median` = median(percent)) %>%
  pivot_longer(c(`Department mean`, `Department median`), names_to="type", values_to="percent")

##...................................
## Export data for Figure 1
##...................................
write_csv(demo_category, "0-data/figure-1.csv")

##...................................
## Recreate Figure 1
##...................................
# figure1.data <- read_csv("0-data/figure-1.csv", col_names=TRUE, col_types=cols())
# figure1.plot <- ggplot(data=figure1.data, aes(x=as.Date(semester), y=percent, linetype=type)) +
#   geom_line() +
#   facet_wrap(~ urm_status, ncol=2, scales="free_y")
# ggsave(figure1.plot, file="figure-1.png")

