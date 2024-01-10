## ----echo = FALSE, warning=FALSE, include = FALSE------
#### Loading the required packages
require(sf)
require(ggplot2)
require(cowplot)
require(tmap)
require(mapview)
require(RColorBrewer)


## ------------------------------------------------------
shape <- read_sf(dsn = "C:/Users/mjstowe/OneDrive - UNSW/Desktop/R", layer = "Primary_Health_Networks")


## ------------------------------------------------------
ethos.loc <- read.csv("ethos.locations.csv")


## ----echo=FALSE, warning=FALSE, include = FALSE--------

map <- ggplot() + 
  geom_sf(data = shape) +
   xlim(105, 160) +
  # geom_rect(aes(xmin = 148, xmax = 154, ymin = -36, ymax = -26), color = "red", fill =   NA) +
  labs() +
labs( x="",
      y="") +
  theme_test() +
  theme(panel.background = element_rect(fill = "lightblue")) 
    # geom_point(aes(x = Longitude,
    #              y = Latitude, fill = "red"), shape= 21, colour="black", data = ethos.loc, size=2, show.legend = FALSE) 


## ------------------------------------------------------
map


## ----echo=FALSE----------------------------------------
ethos.loc <- read.csv("ethos.locations.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), remove = FALSE)


## ----echo=FALSE----------------------------------------
my.pal2 = brewer.pal(n=7, "GnBu")


## ----fgb=FALSE, warning=FALSE, echo=FALSE, include = TRUE----

mapview(shape, alpha.regions = 0, colour = "red", lwd = 2, layer.name = "Primary Health Networks, Australia") + 
mapview(ethos.loc, zcol="rna_prev", layer.name = "HCV RNA prevalence (%)", col.regions = my.pal2) + 
mapview(ethos.loc, zcol="recruitment", layer.name = "No. of participants recruited")




## ----echo=FALSE, include=FALSE-------------------------
pacman::p_load(
  ggplot2 ,    # plotting and graphing results
   rlang,      # read in excel files
   forcats,    # read in excel files
   broom,      # tidy code
   readxl,     # read in excel files
   pacman,     # loading and reading in packages
   rio,        # importing data  
   here,       # relative file pathways  
   janitor,    # data cleaning and tables
   lubridate,  # working with dates
   matchmaker, # dictionary-based cleaning
   epikit,     # age_categories() function
   tidyverse,  # data management and visualization
   gtsummary,  # logistic regression and plotting results
   gt,         # for gtsummary 
   flextable,  # table creation and manipulating
   car,        # data management and visualization
   readstata13,# read in Stata files
   finalfit,   # logistic regression and plotting results
   survminer,  # forest plots
   easystats, 
   BiocManager, 
   survival, 
   forestplot, 
   rticles,     # templates for scientific journal articles in RMarkdown
   jtools, 
   corrplot, 
   bstfun, 
   ggforestplot, 
   codebookr,
   codebook,
   sjlabelled, 
   likert, 
   kableExtra, 
   haven, 
   DT
)



## ------------------------------------------------------
ethos<- read_dta("enrollment survey POC merged_final.dta") 


## ------------------------------------------------------
ethos <- ethos %>% 
  janitor::clean_names()


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(waves = case_when(
    wave2 == 0 ~ "Wave 1",
    wave2 == 1 ~ "Wave 2"
  ))


## ------------------------------------------------------
age <- as.numeric(ethos$surveyage)

ethos <- ethos %>%
  mutate(age = age)



## ----include = FALSE-----------------------------------
ethos <- ethos %>% 
  mutate(age.group = case_when(
    surveyage > 18 & surveyage < 30    ~ "< 29",
    surveyage >= 30 &  surveyage < 40  ~ "30 - 39",
    surveyage >= 40 &  surveyage < 50  ~ "40 - 49",
    surveyage >= 50             ~ "> 50",
    is.na(NA)                     ~ NA_character_,
    TRUE                  ~ "Check me"))



## ----include = FALSE-----------------------------------
# Reorder the variable "age.group" so that the levels are in descending order

ethos <- ethos %>% 
  mutate(age.group = fct_relevel(age.group,
                                   "< 29",
                                   "30 - 39",
                                   "40 - 49",
                                   "> 50"))


## ----include = FALSE-----------------------------------

ethos <- ethos %>% 
  mutate(homeless.fct= case_when(
    homeless == 0 ~ "No",
    homeless == 1 ~ "Yes"
        ))


## ------------------------------------------------------
tabyl(ethos$homeless.fct)%>%
  as_flextable


## ------------------------------------------------------
 ethos <- ethos %>%
   mutate(atsi = case_when(
     eatsi == 0              ~ "Non-indigenous",
     eatsi == 1              ~ "Aboriginal",
     eatsi == 2              ~ "Torres Strait Islander",
     eatsi == 3              ~ "Both Aboriginal and Torres Strait Islander",
     eatsi == 99              ~ "Unknown"))



## ------------------------------------------------------
 ethos <- ethos %>%
   mutate(atsi = fct_relevel(atsi,
                            "Non-indigenous",
                            "Aboriginal",
                            "Torres Strait Islander",
                            "Both Aboriginal and Torres Strait Islander",
                            "Unknown"
                            ))


## ------------------------------------------------------
 atsi_collapse <- fct_collapse(ethos$atsi, Aboriginal = c("Torres Strait Islander", "Both Aboriginal and Torres Strait Islander"))


## ------------------------------------------------------
 atsi <- ethos %>%
   mutate(atsi_collapse=(atsi_collapse))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(prison.yn = case_when(
    sapjjce == 1              ~ "Yes",
    sapjjce == 0              ~ "No"

    ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(prison.6month = case_when(
    sapjjcl6m == 0              ~ "No",
    sapjjcl6m == 1              ~ "Yes"
  ))


## ----include = FALSE-----------------------------------
  
  tbl_age <- ethos %>%
  tbl_summary(percent = "row", include = c(age, age.group
                                           ),
                    label = list(age ~ "Age",
                    age.group ~ "Age group"),missing = "no",
              statistic = all_continuous() ~ ("{median} ({p25} - {p75})")) %>%
  add_stat_label()%>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    label = "**Characteristic** \n (n = {N})")
 


## ------------------------------------------------------
tbl_age


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(inject.month = case_when(
    sbinj1m  == 0              ~ "No",
    sbinj1m == 1              ~ "Yes"
  ))



## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(inject.6month = case_when(
    sbinj6m  == 0              ~ "No",
    sbinj6m == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(inject.year = case_when(
    sbinj1y  == 0              ~ "No",
    sbinj1y == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(injectdrug.month = case_when(
    sbinjdrg  == 1      ~ "Heroin",
    sbinjdrg  == 2      ~ "Cocaine",
    sbinjdrg  == 3      ~ "Amphetamines", 
    sbinjdrg  == 4      ~ "Other opioids", 
    sbinjdrg  == 5      ~ "Benzodiazepines", 
    sbinjdrg  == 77     ~ "Other"))

# Reordering the levels of the drug specific injecting attributes

ethos <- ethos %>% 
  mutate(injectdrug.month = fct_relevel(injectdrug.month,
                                     "Heroin",
                                     "Other opioids",
                                     "Amphetamines",
                                     "Cocaine",
                                     "Benzodiazepines", 
                                     "Other"))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(injectfreq.month = case_when(
    sbidl1m == 1              ~ "More than 3 times a day",
    sbidl1m == 5              ~ "Weekly or less",
    sbidl1m == 4              ~ "More than weekly, not daily",
    sbidl1m == 3              ~ "Once daily",
    sbidl1m == 2              ~ "2 - 3 times a day"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(injectfreq.month = fct_relevel(injectfreq.month,
                                     "Once daily", 
                                     "2 - 3 times a day",
                                     "More than 3 times a day",
                                     "Weekly or less",
                                     "More than weekly, not daily"
  ))


## ------------------------------------------------------
injectdays.month <- as.numeric(ethos$sbdaysinj)

ethos <- ethos %>% 
mutate(injectdays.month = as.numeric(injectdays.month))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.prefer = case_when(
    sdoppr == 1              ~ "Methadone",
    sdoppr == 2              ~ "Buprenorphine (Subutex)",
    sdoppr == 3              ~ "Buprenorphine (Any formulation)",
    sdoppr == 4              ~ "Buprenorphine (long acting injectable)",
    sdoppr == 5              ~ "Buprenorphine-naloxone (Suboxone)",
    sdoppr == 6              ~ "Any - no preference",
    sdoppr == 7              ~ "None - no medication or treatment",
    sdoppr == 8              ~ "None - no appropriate medication",
    sdoppr == 9              ~ "None - not interested in treatment"))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(prefer.treat.yn = case_when(
    sdoppr == 1              ~ "Yes",
    sdoppr == 2              ~ "Yes",
    sdoppr == 3              ~ "Yes",
    sdoppr == 4              ~ "Yes",
    sdoppr == 5              ~ "Yes",
    sdoppr == 6              ~ "Yes",
    sdoppr == 7              ~ "No",
    sdoppr == 8              ~ "No",
    sdoppr == 9              ~ "No"))



## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.prefer.group = case_when(
    sdoppr == 1              ~ "Methadone",
    sdoppr == 2|sdoppr == 5  ~ "Buprenorphine (oral)",
    sdoppr == 3              ~ "Buprenorphine (oral - any formulation)",
    sdoppr == 4              ~ "Buprenorphine (LAIB)",
    sdoppr == 6              ~ "Any",
    sdoppr == 7              ~ "None",
    sdoppr == 8              ~ "None",
    sdoppr == 9              ~ "None"))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(prefer.metbup = case_when(
    sdoppr == 1              ~ "Methadone",
    sdoppr == 2|sdoppr == 5  ~ "Buprenorphine",
    sdoppr == 3              ~ "Buprenorphine",
    sdoppr == 4              ~ "Buprenorphine", 
    is.na(TRUE)  ~ NA_character_,
    FALSE                  ~ "NA"))


## ----echo = FALSE, include=FALSE-----------------------
fct_drop(ethos$prefer.metbup)


## ------------------------------------------------------
ethos <- ethos %>%
  mutate(prefer.metbup = fct_relevel(prefer.metbup,"Buprenorphine", "Methadone"))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.ever.yn = case_when(
    sdost_ever  == 0              ~ "No",
    sdost_ever == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.month.yn = case_when(
    sdost_mon  == 0              ~ "No",
    sdost_mon == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.6month.yn = case_when(
    sdost_6mon  == 0              ~ "No",
    sdost_6mon == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.year.yn = case_when(
    sdost_1yr  == 0              ~ "No",
    sdost_1yr == 1              ~ "Yes"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(oat.type = case_when(
    sdost_curr == 1       ~ "Methadone",
    sdost_curr == 2       ~ "Buprenorphine (Subutex)",
    sdost_curr == 3       ~ "Buprenorphine (Any formulation)",
    sdost_curr == 4       ~ "Buprenorphine (long acting injectable)",
    sdost_curr == 5       ~ "Buprenorphine-naloxone (Suboxone)",
    sdost_curr == 77      ~ "Other",
    sdost_curr == 99      ~ "Unknown"
    ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(methadonedose.yn = case_when(
    sdknddoes  == 0              ~ "No",
    sdknddoes == 1              ~ "Yes"
  ))


## ------------------------------------------------------
methadone.dose <- as.numeric(ethos$sdmetd)

ethos <- ethos %>% 
  mutate(methadone.dose = as.numeric(methadone.dose))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(methadonedose.yn = case_when(
    sdknddoes  == 0              ~ "No",
    sdknddoes == 1              ~ "Yes"
  ))



## ------------------------------------------------------
methadone.dose <- as.numeric(ethos$sdmetd)

ethos <- ethos %>% 
  mutate(methadone.dose = as.numeric(methadone.dose))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(bupe.laib = case_when(
    sdodlbp  == 1              ~ "Weekly",
    sdodlbp == 2              ~ "Monthly"
  ))


## ------------------------------------------------------
laib.dose.wk <- as.numeric(ethos$sdwbud)

ethos <- ethos %>% 
  mutate(laib.dose.wk = as.numeric(laib.dose.wk))



## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(laib.wk.yn = case_when(
    sdwbup  == 0              ~ "No",
    sdwbup == 1              ~ "Yezs"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(laib.month.yn = case_when(
    sdmbuk  == 0              ~ "No",
    sdmbuk == 1              ~ "Yes"
  ))


## ------------------------------------------------------
laib.dose.month <- as.numeric(ethos$sdmbud)

ethos <- ethos %>% 
  mutate(laib.dose.month = as.numeric(laib.dose.month))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(dose.satisfied = case_when(
    sddses  == 1              ~ "Yes",
    sddses  == 2              ~ "No, too low",
    sddses  == 3              ~ "No, too high"
  ))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(dose.location = case_when(
    sddsdis == 1 ~ "Public",
    sddsdis == 2 ~ "Private",
    sddsdis == 3 ~ "Doctor",
    sddsdis == 4 ~ "Pharmacy",
    sddsdis == 5 ~ "Prison", 
    sddsdis == 77 ~ "Other"))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(dose.takeaways.yn = case_when(
    sdtake  == 0              ~ "No",
    sdtake == 1              ~ "Yes"
  ))


## ------------------------------------------------------
collection.week <- as.numeric(ethos$sdlmdse)

ethos <- ethos %>% 
  mutate(collection.week = as.numeric(collection.week))


## ------------------------------------------------------
ethos <- ethos %>% 
  mutate(health.pain = case_when(
    sghcpd == 0 ~ "I have no pain or discomfort",
    sghcpd == 1 ~ "I have slight pain or discomfort",
    sghcpd == 2 ~ "I have moderate pain or discomfort",
    sghcpd == 3 ~ "I have severe pain or discomfort",
    sghcpd == 4 ~ "I have extreme pain or discomfort" 
    ))

