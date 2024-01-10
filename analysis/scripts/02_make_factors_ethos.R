# Created by Abe Kairouz on 21/01/2021, for Ethos II 'Preference' study.

# Analyses determined from 'Prefer Concept Sheet 15 Dec'. 

# Current analyses perofrmed on "enrollment survey POC merged_final".


# Load Packages

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
   here,
   flexdashboard, # dashboard versions of R Markdown reports
   plotly, # interactive plots
   shiny, 
   webshot, 
   webshot2, 
   grateful
 )

 
# Set working directory

# setwd("C:/Users/mjstowe/OneDrive - UNSW/Desktop/R")

# import the .dta file into R
# 
load(
  here::here("data_processed", "ethos_working.rda")
)

# ethos<- read_dta("enrollment survey POC merged_final.dta")
# 
# ethos. <- read_dta("W1 W2 complete.dta")
# 
# ethos.enroll <- read_dta("ethos_enroll.dta")

skimr::skim(ethos)
names(ethos)

# # MERGING THE DATASETS
# # Merge the two datasets by recid ID
# 
# ethos <- merge(ethos.survey, ethos.enroll, by = "ibarcode", 
#                      all.x = TRUE, all.y = TRUE)


# cleaning the data
# 
ethos <- ethos %>% 
  janitor::clean_names()



# Create a new dataset where all the "Yes"/"No" reference categories are in the order "Yes" -> "No"


# # Creating a new column to separate out wave 1 and wave 2
# ethos.enroll %<>% 
#   mutate(waves = case_when(
#     start <= ymd_hms("2019-10-01 07:00:00") ~ "Wave 1",
#       start >= ymd_hms("2019-11-01 07:00:00") ~ "Wave 2"
#   ))


# Re subsample (Table 1 & 2): I interpreted this analysis as concerning everybody enrolled in ETHOS II who had responded to our DV question (i.e. preference), excluding those who did not report recent (6m) OAT or Opioid Use (any ROA). JG Confirmed.

#  Age group

ethos_ny <- ethos %>% 
  mutate(age_group = case_when(
    surveyage > 18 & surveyage < 30    ~ "< 29 ",
    surveyage >= 30 &  surveyage < 40  ~ "30 - 39",
    surveyage >= 40 &  surveyage < 50  ~ "40 - 49",
    surveyage >= 50             ~ "> 50",
    is.na(NA)                     ~ NA_character_,
    TRUE                  ~ "Check me") %>%
      fct_relevel("< 29 ",
                  "30 - 39",
                  "40 - 49", 
                  "> 50"))

# 60 currently receiving OAT? 

ethos_ny <- ethos %>% 
  mutate(oat.yn = case_when(
    sdost_curr == 1 ~ "Yes", 
    sdost_curr == 2 ~ "Yes",
    sdost_curr == 3 ~ "Yes",
    sdost_curr == 4 ~ "Yes",
    sdost_curr == 5 ~ "Yes",
    sdost_curr == 77 ~ "Yes",
    sdost_curr == 99 ~ "Yes",
    is.na(TRUE)   ~ NA_character_,
    TRUE       ~ "No") %>%
      fct_relevel("No", 
                  "Yes"))



# The following variables require conversion to a numeric variable before they can be recoded in the code below. 

# 11. How many days have you injected drugs in the last month?

injectdays.month <- as.numeric(ethos$sbdaysinj)

# 61. What is your current daily dose of methadone (mg)

methadone.dose <- as.numeric(ethos$sdmetd)

# 62. What is your current daily dose of buprenorphine (mg)?

methadone.dose <- as.numeric(ethos$sdmetd)

# 64. What is your current dose of weekly long-acting injectable buprenorphine?

laib.dose.wk <- as.numeric(ethos$sdwbud)

# 65. What is your current dose of monthly long-acting injectable buprenorphine?

laib.dose.month <- as.numeric(ethos$sdmbud)

# 69. In the last month, how many times per week did you collect your dose? 

collection.week <- as.numeric(ethos$sdlmdse)

# Recode variables


ethos_ny <- ethos %>% 
  mutate(
    
    # Study waves
    
    waves = factor(wave2) %>%
      fct_recode("Wave 1" = "0",
                 "Wave 2" = "1") %>%
      ff_label("Study wave") %>%
      fct_relevel("Wave 1", 
                  "Wave 2"),
    
    # Gender 
    genderall.factor = factor(sex) %>%
      fct_recode("Male" = "0",
                 "Female"  = "1", 
                 "Transgender" = "2") %>% 
      ff_label("Gender"),
    
    # Homeless 
    homeless.fct = factor(homeless) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Homeless") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 4. Have you ever been in prison or a juvenile justicecentre?
    prison.yn = factor(sapjjce) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Ever incarcerated") %>%
      fct_relevel("No",
                  "Yes"),

    
    # Incarceration?
    
    
    prison_history = factor(prison)%>%
      fct_recode("Never" = "0",
                 "More than 1 year ago" = "1",
                 "Less than 1 year ago" = "2",  ) %>% 
      ff_label("Prison history") %>%
      fct_relevel("Never",
                  "More than 1 year ago",
                  "Less than 1 year ago"),
    
    # 5. Have you ever been in prison or a juvenile justice centre in the last 6 months?
    
    prison.6month = factor(sapjjcl6m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent incarceration (prior 6 months)") %>%
      fct_relevel("No",
                  "Yes"),

    # 6. Have you injected drugs in the last month? 
    
    inject.month = factor(sbinj1m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior month)") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 7. Have you injected drugs in the last six months? 
    
    inject.6month = factor(sbinj6m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior 6 months)") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 8. Have you injected drugs in the year? 
    
    inject.year = factor(sbinj1y)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior year)") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 9. What drug did you inject most often in the last month?Injecting - drug specific
    
    injectdrug.month = factor(sbinjdrg) %>%
      fct_recode("Heroin" = "1", 
                 "Cocaine" = "2",
                 "Amphetamines" = "3",
                 "Other opioids" = "4",
                 "Benzodiazepines" = "5",
                 "Other" = "77") %>%
      ff_label("Drugs injected (prior month)") %>%
      fct_relevel("Heroin",
                  "Other opioids",
                  "Amphetamines",
                  "Cocaine",
                  "Benzodiazepines", 
                  "Other"), 
    
    #  What drug group did you inject most often in the last month?Injecting - drug specific
    
    injectdrug_group.month = factor(sbinjdrg) %>%
      fct_recode("Opioids" = "1", 
                 "Stimulants" = "2",
                 "Stimulants" = "3",
                 "Opioids" = "4",
                 "Benzodiazepines" = "5",
                 "Other" = "77") %>%
      ff_label("Drug group injected (prior month)") %>%
      fct_relevel("Opioids",
                  "Stimulants",
                  "Benzodiazepines", 
                  "Other"),
    
    # 10. How often did you inject drugs in the last month?
    
    injectfreq.month = factor(sbidl1m) %>%
      fct_recode("More than 3 times a day" = "1",
                 "2 - 3 times a day" = "2",
                 "Once daily" = "3",
                 "More than weekly, not daily" = "4",
                 "Weekly or less" = "5") %>%
      ff_label("Drug injection frequency (prior month)") %>%
      fct_relevel("More than 3 times a day",
                  "Weekly or less",
                  "More than weekly, not daily",
                  "Once daily",
                  "2 - 3 times a day"),
    
    # Drug injection frequency collapsed
    
    injectfreq.month.daily = factor(sbidl1m) %>%
      fct_recode("Less than daily or daily" = "1",
                 "Less than daily or daily" = "2",
                 "Less than daily or daily" = "3",
                 "More than daily" = "4",
                 "More than daily" = "5") %>%
      ff_label("Drug injection frequency") %>%
      fct_relevel("Less than daily or daily",
                  "More than daily"),
    
    # 11. How many days have you injected drugs in the last month?
    
    injectdays.month = as.numeric(injectdays.month),
    
    # SECTION D: DRUG TREATMENT
    
    # 55. Of all the following medications used to treat OUD, which would you prefer? 
    
    oat.prefer = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (Subutex)" = "2",
                 "Buprenorphine (Any formulation)" = "3",
                 "Buprenorphine (long acting injectable)" = "4",
                 "Buprenorphine-naloxone (Suboxone)" = "5",
                 "Any - no preference" = "6",
                 "None - no medication or treatment" = "7",
                 "None - no appropriate medication" = "8",
                 "None - not interested in treatment" = "9") %>%
      ff_label("OAT preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (Subutex)",
                  "Buprenorphine (Any formulation)",
                  "Buprenorphine (long acting injectable)",
                  "Buprenorphine-naloxone (Suboxone)",
                  "Any - no preference",
                  "None - no medication or treatment",
                  "None - no appropriate medication",
                  "None - not interested in treatment"),
    
    # Prefere treatment [yes/no]
    
    prefer.treat.yn = factor(sdoppr) %>%
      fct_recode("Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4",
                 "Yes" = "5",
                 "Yes" = "6",
                 "No" = "7",
                 "No" = "8",
                 "No" = "9") %>%
      ff_label("Treatment preference") %>%
      fct_relevel("No",
                  "Yes"),
    
    # OAT preference simplified [yes/no]
    
    med_pref = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (oral)" = "2",
                 "Buprenorphine (oral - any formulation)" = "3",
                 "Buprenorphine (LAIB)" = "4",
                 "Buprenorphine (oral)" = "5",
                 "Any" = "6",
                 "None" = "7",
                 "None" = "8",
                 "None" = "9") %>%
      ff_label("Medication preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (oral)",
                  "Buprenorphine (oral - any formulation)",
                  "Buprenorphine (LAIB)",
                  "Any",
                  "None"),
    
    # OAT preference simplified [methadone/buprenorphine]
    
    methadonebupe_pref = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine" = "2",
                 "Buprenorphine" = "3",
                 "Buprenorphine" = "4",
                 "Buprenorphine" = "5",
                 "Any" = "6",
                 "None" = "7",
                 "None" = "8",
                 "None" = "9") %>%
      ff_label("Medication preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine",
                  "Any",
                  "None"),
    
    # Currently on OAT? 
    
    oat_current.yn = factor(sdost_mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Currently on OAT") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # Current OAT duration 
    
    oat_duration = factor(sdost_len)%>%
      fct_recode("Less than 3 months" = "1",
                 "3-6 months" = "2",
                 "6-12 months" = "3",
                 "1-2 years" = "4",
                 "More than 2 years" = "5") %>%
      ff_label("Current OAT duration") %>%
      fct_relevel("Less than 3 months",
                  "3-6 months",
                  "6-12 months",
                  "1-2 years",
                  "More than 2 years"),
    
    # Current OAT duration group 
    
    oat_duration_01 = factor(sdost_len)%>%
      fct_recode("Less than 3 months" = "1",
                 "3 months-2 years" = "2",
                 "3 months-2 years"  = "3",
                 "3 months-2 years"  = "4",
                 "More than 2 years" = "5") %>%
      ff_label("Current OAT duration period") %>%
      fct_relevel("Less than 3 months",
                  "3 months-2 years",
                  "More than 2 years"),
    
    # 56. Have you ever been on OAT? 
    
    oat.ever.yn = factor(sdost_ever)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Ever received OAT") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # 57. Have you been on OAT in the last month? 
    
    oat.month.yn = factor(sdost_mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior month)") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 58. Have you been on OAT in the last 6 months? 
    
    oat.6month.yn = factor(sdost_6mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior 6 months)") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 59. Have you been on OAT in the last year? 
    
    oat.year.yn = factor(sdost_1yr)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior year)") %>%
      fct_relevel("No",
                  "Yes"),
    
    
    # 60. Type of OAT currently receiving? 
    
    oat.type = factor(sdost_curr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (Subutex)" = "2",
                 "Buprenorphine (Any formulation)" = "3",
                 "Buprenorphine (long acting injectable)" = "4",
                 "Buprenorphine-naloxone (Suboxone)" = "5",
                 "Other" = "77",
                 "Unknown" = "99") %>%
      ff_label("Current OAT medication") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (Subutex)",
                  "Buprenorphine (Any formulation)",
                  "Buprenorphine (long acting injectable)",
                  "Buprenorphine-naloxone (Suboxone)",
                  "Other",
                  "Unknown"),
                 
    # Do you know your current methadone dose? 
    
    methadonedose.yn = factor(sdknddoes)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current methadone dose") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 61. What is your current daily dose of methadone (mg)
    
    methadone.dose = as.numeric(methadone.dose),
    
    # Do you know your current buprenorphine  dose? 
    
    bupedose.yn = factor(sdknddoes)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current buprenorphine dose") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 62. What is your current daily dose of buprenorphine (mg)?
    
    methadone.dose = as.numeric(methadone.dose),
    
    # 63. How often do you receive your dose of long-acting injectable buprenorphine? 
    
    bupe.laib = factor(sdodlbp) %>%
      fct_recode("Weekly" = "1",
                 "Monthly" = "2") %>%
      ff_label("Frequency of LAIB dose") %>%
      fct_relevel("Weekly", 
                  "Monthly"),

    # 64. What is your current dose of weekly long-acting injectable buprenorphine?
    
    laib.dose.wk = as.numeric(laib.dose.wk),
    
    
    # 64. Do you know your current dose of weekly long-acting injectable buprenorphine?
    
    laib.wk.yn = factor(sdwbup) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current LAIB weekly dose") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 65. Do you know your current dose of monthly long-acting injectable buprenorphine?
    
    laib.month.yn = factor(sdmbuk) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current LAIB weekly dose") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 65. What is your current dose of monthly long-acting injectable buprenorphine?
    
    laib.dose.month = as.numeric(laib.dose.month),
    
    # 66. Are you satisfied with your dose? 
    
    dose.satisfied = factor(sddses) %>% 
      fct_recode("Yes" = "1",
                 "No, too low" = "2",
                 "No, too high" = "3") %>% 
      ff_label("Dose satisfaction") %>% 
      fct_relevel("Yes", 
                  "No, too low",
                  "No, too high"),

    # 67. Where was your last dose dispensed? 
    
    
    dose.location = factor(sddsdis) %>%
      fct_recode("Public" = "1",
                 "Private" = "2",
                 "Doctor" = "3",
                 "Pharmacy" = "4",
                 "Prison" = "5",
                 "Other" = "77") %>%
      ff_label("Location last dose dispensed") %>%
      fct_relevel("Pharmacy",
                  "Public",
                  "Private",
                  "Doctor",
                  "Prison",
                  "Other"),

    # 68. Do you receive take-away doses? 
    
    dose.takeaways.yn = factor(sdtake) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Receives takeway doses") %>%
      fct_relevel("No",
                  "Yes"),
    
    # 69. In the last month, how many times per week did you collect your dose? 
    
    collection.week = as.numeric(collection.week),
    
    # 109. Please indicate which statement best describes your own health today? 
    
    health.pain = factor(sghcpd) %>%
      fct_recode("I have no pain or discomfort" = "0",
                 "I have slight pain or discomfort" = "1",
                 "I have moderate pain or discomfort" = "2",
                 "I have severe pain or discomfort" = "3",
                 "I have extreme pain or discomfort" = "4") %>% 
      ff_label("Current physical health") %>%
      fct_relevel("I have no pain or discomfort",
                 "I have slight pain or discomfort",
                 "I have moderate pain or discomfort",
                 "I have severe pain or discomfort",
                 "I have extreme pain or discomfort"), 
    
    # SECTION D: STIGMA AND DISCRIMINATION
    
    # 103. In the last 12 months, have you experienced any stigma or discrimination in relation to your use of drugs for injecting?
    
    stigma.inject = factor(ggstig) %>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("Injecting stigma") %>%
      fct_relevel("Never",
                 "Rarely",
                 "Sometimes",
                 "Often",
                 "Always"), 
    
    # Create a new yes/no variable that represents if someone has been stigmatised in relation to injecting drug use
    
      stigma.inject.yn = factor(ggstig)%>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced injecting stigma") %>%
      fct_relevel("No", 
                  "Yes"), 
    
    # 104. In the last 12 months, have you experienced any stigma or discrimination in relation to your hepatitis C status?
    
    stigma.hcv  = factor(ggstighc)%>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("HCV stigma") %>%
      fct_relevel("Never",
                 "Rarely",
                 "Sometimes",
                 "Often",
                 "Always"), 
    
    # Create a new yes/no variable that represents if someone has been stigmatised in relation to hcv
    
    stigma.hcv.yn  = factor(ggstighc) %>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced HCV stigma") %>%
      fct_relevel("No", 
                  "Yes"),

    
    # 105. In the last 12 months, to what extent do you agree that health workers treated you negatively or different to other people? 
    
    discrim = factor(ggstigw) %>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("HCV stigma") %>%
      fct_relevel("Never",
                 "Rarely",
                 "Sometimes",
                 "Often",
                 "Always"),
    
    # Create a new yes/no variable that represents if someone has been discriminated against by health workers
    
    discrim.yn = factor(ggstigw) %>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced discrimination") %>%
      fct_relevel("No", 
                  "Yes"))

# #########################################################################################################

# Create a new dataset where all the "Yes"/"No" reference categories are in the order "Yes" -> "No"


#  Age group

ethos_yn <- ethos %>% 
  mutate(age_group = case_when(
    surveyage > 18 & surveyage < 30    ~ "< 29 ",
    surveyage >= 30 &  surveyage < 40  ~ "30 - 39",
    surveyage >= 40 &  surveyage < 50  ~ "40 - 49",
    surveyage >= 50             ~ "> 50",
    is.na(NA)                     ~ NA_character_,
    TRUE                  ~ "Check me") %>%
      fct_relevel("< 29 ",
                  "30 - 39",
                  "40 - 49", 
                  "> 50"))

# 60 currently receiving OAT? 

ethos_yn <- ethos %>% 
  mutate(oat.yn = case_when(
    sdost_curr == 1 ~ "Yes", 
    sdost_curr == 2 ~ "Yes",
    sdost_curr == 3 ~ "Yes",
    sdost_curr == 4 ~ "Yes",
    sdost_curr == 5 ~ "Yes",
    sdost_curr == 77 ~ "Yes",
    sdost_curr == 99 ~ "Yes",
    is.na(TRUE)   ~ NA_character_,
    TRUE       ~ "No") %>%
      fct_relevel("No", 
                  "Yes"))


ethos_yn <- ethos %>% 
  mutate(
    
    # Study waves
    
    waves = factor(wave2) %>%
      fct_recode("Wave 1" = "0",
                 "Wave 2" = "1") %>%
      ff_label("Study wave") %>%
      fct_relevel("Wave 1", 
                  "Wave 2"),
    
    # Gender 
    genderall.factor = factor(sex) %>%
      fct_recode("Male" = "0",
                 "Female"  = "1", 
                 "Transgender" = "2") %>% 
      ff_label("Gender"),
    
    # Homeless 
    homeless.fct = factor(homeless) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Homeless") %>%
      fct_relevel("Yes",
                  "No"),
    
    # Incarceration
    
    prison_history = factor(prison)%>%
      fct_recode("Never" = "0",
                 "More than 1 year ago" = "1",
                 "Less than 1 year ago" = "2",  ) %>% 
      ff_label("Prison history") %>%
      fct_relevel("Never",
                  "More than 1 year ago",
                  "Less than 1 year ago"),
    
    # 4. Have you ever been in prison or a juvenile justicecentre?
    prison.yn = factor(sapjjce) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Ever incarcerated") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 5. Have you ever been in prison or a juvenile justice centre in the last 6 months?
    
    prison.6month = factor(sapjjcl6m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent incarceration (prior 6 months)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 6. Have you injected drugs in the last month? 
    
    inject.month = factor(sbinj1m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior month)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 7. Have you injected drugs in the last six months? 
    
    inject.6month = factor(sbinj6m)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior 6 months)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 8. Have you injected drugs in the year? 
    
    inject.year = factor(sbinj1y)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Recent injecting (prior year)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 9. What drug did you inject most often in the last month?Injecting - drug specific
    
    injectdrug.month = factor(sbinjdrg) %>%
      fct_recode("Heroin" = "1", 
                 "Cocaine" = "2",
                 "Amphetamines" = "3",
                 "Other opioids" = "4",
                 "Benzodiazepines" = "5",
                 "Other" = "77") %>%
      ff_label("Drugs injected (prior month)") %>%
      fct_relevel("Heroin",
                  "Other opioids",
                  "Amphetamines",
                  "Cocaine",
                  "Benzodiazepines", 
                  "Other"), 
    
    #  What drug group did you inject most often in the last month?Injecting - drug specific
    
    injectdrug_group.month = factor(sbinjdrg) %>%
      fct_recode("Opioids" = "1", 
                 "Stimulants" = "2",
                 "Stimulants" = "3",
                 "Opioids" = "4",
                 "Benzodiazepines" = "5",
                 "Other" = "77") %>%
      ff_label("Drug group injected (prior month)") %>%
      fct_relevel("Opioids",
                  "Stimulants",
                  "Benzodiazepines", 
                  "Other"),
    
    # 10. How often did you inject drugs in the last month?
    
    injectfreq.month = factor(sbidl1m) %>%
      fct_recode("More than 3 times a day" = "1",
                 "2 - 3 times a day" = "2",
                 "Once daily" = "3",
                 "More than weekly, not daily" = "4",
                 "Weekly or less" = "5") %>%
      ff_label("Drug injection frequency (prior month)") %>%
      fct_relevel("More than 3 times a day",
                  "Weekly or less",
                  "More than weekly, not daily",
                  "Once daily",
                  "2 - 3 times a day"),
    
    # Drug injection frequency collapsed
    
    injectfreq.month.daily = factor(sbidl1m) %>%
      fct_recode("Less than daily or daily" = "1",
                 "Less than daily or daily" = "2",
                 "Less than daily or daily" = "3",
                 "More than daily" = "4",
                 "More than daily" = "5") %>%
      ff_label("Drug injection frequency") %>%
      fct_relevel("Less than daily or daily",
                  "More than daily"),
    
    # 11. How many days have you injected drugs in the last month?
    
    injectdays.month = as.numeric(injectdays.month),
    
    # SECTION D: DRUG TREATMENT
    
    # 55. Of all the following medications used to treat OUD, which would you prefer? 
    
    oat.prefer = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (Subutex)" = "2",
                 "Buprenorphine (Any formulation)" = "3",
                 "Buprenorphine (long acting injectable)" = "4",
                 "Buprenorphine-naloxone (Suboxone)" = "5",
                 "Any - no preference" = "6",
                 "None - no medication or treatment" = "7",
                 "None - no appropriate medication" = "8",
                 "None - not interested in treatment" = "9") %>%
      ff_label("OAT preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (Subutex)",
                  "Buprenorphine (Any formulation)",
                  "Buprenorphine (long acting injectable)",
                  "Buprenorphine-naloxone (Suboxone)",
                  "Any - no preference",
                  "None - no medication or treatment",
                  "None - no appropriate medication",
                  "None - not interested in treatment"),
    
    # Prefere treatment [yes/no]
    
    prefer.treat.yn = factor(sdoppr) %>%
      fct_recode("Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4",
                 "Yes" = "5",
                 "Yes" = "6",
                 "No" = "7",
                 "No" = "8",
                 "No" = "9") %>%
      ff_label("Treatment preference") %>%
      fct_relevel("Yes",
                  "No"),
    
    # OAT preference simplified [yes/no]
    
    med_pref = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (oral)" = "2",
                 "Buprenorphine (oral - any formulation)" = "3",
                 "Buprenorphine (LAIB)" = "4",
                 "Buprenorphine (oral)" = "5",
                 "Any" = "6",
                 "None" = "7",
                 "None" = "8",
                 "None" = "9") %>%
      ff_label("Medication preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (oral)",
                  "Buprenorphine (oral - any formulation)",
                  "Buprenorphine (LAIB)",
                  "Any",
                  "None"),
    
    # OAT preference simplified [methadone/buprenorphine]
    
    methadonebupe_pref = factor(sdoppr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine" = "2",
                 "Buprenorphine" = "3",
                 "Buprenorphine" = "4",
                 "Buprenorphine" = "5",
                 "Any" = "6",
                 "None" = "7",
                 "None" = "8",
                 "None" = "9") %>%
      ff_label("Medication preference") %>%
      fct_relevel("Methadone",
                  "Buprenorphine",
                  "Any",
                  "None"),
    
    # Currently on OAT? 
    
    oat_current.yn = factor(sdost_mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Currently on OAT") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # Current OAT duration 
    
    oat_duration = factor(sdost_len)%>%
      fct_recode("Less than 3 months" = "1",
                 "3-6 months" = "2",
                 "6-12 months" = "3",
                 "1-2 years" = "4",
                 "More than 2 years" = "5") %>%
      ff_label("Current OAT duration") %>%
      fct_relevel("Less than 3 months",
                  "3-6 months",
                  "6-12 months",
                  "1-2 years",
                  "More than 2 years"),
    
    # Current OAT duration group 
    
    oat_duration_01 = factor(sdost_len)%>%
      fct_recode("Less than 3 months" = "1",
                 "3 months-2 years" = "2",
                 "3 months-2 years"  = "3",
                 "3 months-2 years"  = "4",
                 "More than 2 years" = "5") %>%
      ff_label("Current OAT duration period") %>%
      fct_relevel("Less than 3 months",
                  "3 months-2 years",
                  "More than 2 years"),
    # 56. Have you ever been on OAT? 
    
    oat.ever.yn = factor(sdost_ever)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Ever received OAT") %>%
      fct_relevel("Yes",
                  "No"), 
    
    # 57. Have you been on OAT in the last month? 
    
    oat.month.yn = factor(sdost_mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior month)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 58. Have you been on OAT in the last 6 months? 
    
    oat.6month.yn = factor(sdost_6mon)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior 6 months)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 59. Have you been on OAT in the last year? 
    
    oat.year.yn = factor(sdost_1yr)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Received OAT (prior year)") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 60. Type of OAT currently receiving? 
    
    oat.type = factor(sdost_curr) %>%
      fct_recode("Methadone" = "1",
                 "Buprenorphine (Subutex)" = "2",
                 "Buprenorphine (Any formulation)" = "3",
                 "Buprenorphine (long acting injectable)" = "4",
                 "Buprenorphine-naloxone (Suboxone)" = "5",
                 "Other" = "77",
                 "Unknown" = "99") %>%
      ff_label("Current OAT medication") %>%
      fct_relevel("Methadone",
                  "Buprenorphine (Subutex)",
                  "Buprenorphine (Any formulation)",
                  "Buprenorphine (long acting injectable)",
                  "Buprenorphine-naloxone (Suboxone)",
                  "Other",
                  "Unknown"),
    
    # Do you know your current methadone dose? 
    
    methadonedose.yn = factor(sdknddoes)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current methadone dose") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 61. What is your current daily dose of methadone (mg)
    
    methadone.dose = as.numeric(methadone.dose),
    
    # Do you know your current buprenorphine  dose? 
    
    bupedose.yn = factor(sdknddoes)%>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current buprenorphine dose") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 62. What is your current daily dose of buprenorphine (mg)?
    
    methadone.dose = as.numeric(methadone.dose),
    
    # 63. How often do you receive your dose of long-acting injectable buprenorphine? 
    
    bupe.laib = factor(sdodlbp) %>%
      fct_recode("Weekly" = "1",
                 "Monthly" = "2") %>%
      ff_label("Frequency of LAIB dose") %>%
      fct_relevel("Weekly", 
                  "Monthly"),
    
    # 64. What is your current dose of weekly long-acting injectable buprenorphine?
    
    laib.dose.wk = as.numeric(laib.dose.wk),
    
    
    # 64. Do you know your current dose of weekly long-acting injectable buprenorphine?
    
    laib.wk.yn = factor(sdwbup) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current LAIB weekly dose") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 65. Do you know your current dose of monthly long-acting injectable buprenorphine?
    
    laib.month.yn = factor(sdmbuk) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Knows current LAIB weekly dose") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 65. What is your current dose of monthly long-acting injectable buprenorphine?
    
    laib.dose.month = as.numeric(laib.dose.month),
    
    # 66. Are you satisfied with your dose? 
    
    dose.satisfied = factor(sddses) %>% 
      fct_recode("Yes" = "1",
                 "No, too low" = "2",
                 "No, too high" = "3") %>% 
      ff_label("Dose satisfaction") %>% 
      fct_relevel("Yes", 
                  "No, too low",
                  "No, too high"),
    
    # 67. Where was your last dose dispensed? 
    
    
    dose.location = factor(sddsdis) %>%
      fct_recode("Public" = "1",
                 "Private" = "2",
                 "Doctor" = "3",
                 "Pharmacy" = "4",
                 "Prison" = "5",
                 "Other" = "77") %>%
      ff_label("Location last dose dispensed") %>%
      fct_relevel("Pharmacy",
                  "Public",
                  "Private",
                  "Doctor",
                  "Prison",
                  "Other"),
    
    # 68. Do you receive take-away doses? 
    
    dose.takeaways.yn = factor(sdtake) %>%
      fct_recode("No" = "0",
                 "Yes"  = "1") %>% 
      ff_label("Receives takeway doses") %>%
      fct_relevel("Yes",
                  "No"),
    
    # 69. In the last month, how many times per week did you collect your dose? 
    
    collection.week = as.numeric(collection.week),
    
    # 109. Please indicate which statement best describes your own health today? 
    
    health.pain = factor(sghcpd) %>%
      fct_recode("I have no pain or discomfort" = "0",
                 "I have slight pain or discomfort" = "1",
                 "I have moderate pain or discomfort" = "2",
                 "I have severe pain or discomfort" = "3",
                 "I have extreme pain or discomfort" = "4") %>% 
      ff_label("Current physical health") %>%
      fct_relevel("I have no pain or discomfort",
                  "I have slight pain or discomfort",
                  "I have moderate pain or discomfort",
                  "I have severe pain or discomfort",
                  "I have extreme pain or discomfort"), 
    
    # SECTION D: STIGMA AND DISCRIMINATION
    
    # 103. In the last 12 months, have you experienced any stigma or discrimination in relation to your use of drugs for injecting?
    
    stigma.inject = factor(ggstig) %>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("Injecting stigma") %>%
      fct_relevel("Never",
                  "Rarely",
                  "Sometimes",
                  "Often",
                  "Always"), 
    
    # Create a new yes/no variable that represents if someone has been stigmatised in relation to injecting drug use
    
    stigma.inject.yn = factor(ggstig)%>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced injecting stigma") %>%
      fct_relevel("Yes",
                  "No"), 
    
    # 104. In the last 12 months, have you experienced any stigma or discrimination in relation to your hepatitis C status?
    
    stigma.hcv  = factor(ggstighc)%>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("HCV stigma") %>%
      fct_relevel("Never",
                  "Rarely",
                  "Sometimes",
                  "Often",
                  "Always"), 
    
    # Create a new yes/no variable that represents if someone has been stigmatised in relation to hcv
    
    stigma.hcv.yn  = factor(ggstighc) %>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced HCV stigma") %>%
      fct_relevel("Yes",
                  "No"),
    
    
    # 105. In the last 12 months, to what extent do you agree that health workers treated you negatively or different to other people? 
    
    discrim = factor(ggstigw) %>%
      fct_recode("Never" = "0",
                 "Rarely" = "1",
                 "Sometimes" = "2",
                 "Often" = "3",
                 "Always" = "4") %>%
      ff_label("HCV stigma") %>%
      fct_relevel("Never",
                  "Rarely",
                  "Sometimes",
                  "Often",
                  "Always"),
    
    # Create a new yes/no variable that represents if someone has been discriminated against by health workers
    
    discrim.yn = factor(ggstigw) %>%
      fct_recode("No" = "0",
                 "Yes" = "1",
                 "Yes" = "2",
                 "Yes" = "3",
                 "Yes" = "4") %>%
      ff_label("Ever experienced discrimination") %>%
      fct_relevel("Yes",
                  "No"))       

# Save
save(ethos_yn, ethos_ny, file = 
       here::here("data_processed", "ethos_factors.rda")
)



















































ethos$age.group <- as.factor(ethos$age.group)

describe(ethos$age.group)


knitr::kable(head(ethos))
sum(is.na(ethos))


ethos[ethos$gender == 0,]$sex <- "F" # Replacing 0 by F
ethos[ethos$gender == 1,]$sex <- "M" # Replacing 1 by M

#   Ethnicity
# ethos <- ethos %>%
#   mutate(atsi = case_when(
#     eatsi == 0              ~ "Non-indigenous",
#     eatsi == 1              ~ "Aboriginal",
#     eatsi == 2              ~ "Torres Strait Islander",
#     eatsi == 3              ~ "Both Aboriginal and Torres Strait Islander",
#     eatsi == 99              ~ "Unknown"))
#
#  Reorder the ethnicity variable
#
# ethos <- ethos %>%
#   mutate(atsi = fct_relevel(atsi,
#                            "Non-indigenous",
#                            "Aboriginal",
#                            "Torres Strait Islander",
#                            "Both Aboriginal and Torres Strait Islander",
#                            "Unknown"
#                            ))
#
#  Collapsing ethnicity categories into non-indigienous and aboriginal
#
# atsi_collapse <- fct_collapse(ethos$atsi, Aboriginal = c("Torres Strait Islander", "Both Aboriginal and Torres Strait Islander"))
#
#  Add to the dataset
#
# atsi <- ethos %>%
#   mutate(atsi_collapse=(atsi_collapse))

# 4. Have you ever been in prison or a juvenile justicecentre?

ethos <- ethos %>% 
  mutate(prison.yn = case_when(
    sapjjce == 0              ~ "No",
    sapjjce == 1              ~ "Yes"
    ))

# 5. Have you ever been in prison or a juvenile justice centre in the last 6 months?

ethos <- ethos %>% 
  mutate(prison.6month = case_when(
    sapjjcl6m == 0              ~ "No",
    sapjjcl6m == 1              ~ "Yes"
  ))


# SECTION B: INJECTING DRUG USE AND ALCOHOL

# 6. Have you injected drugs in the last month? 

ethos <- ethos %>% 
  mutate(inject.month = case_when(
    sbinj1m  == 0              ~ "No",
    sbinj1m == 1              ~ "Yes"
  ))

# 7. Have you injected drugs in the last month? 

ethos <- ethos %>% 
  mutate(inject.6month = case_when(
    sbinj6m  == 0              ~ "No",
    sbinj6m == 1              ~ "Yes"
  ))

# 8. Have you injected drugs in the year? 

ethos <- ethos %>% 
  mutate(inject.year = case_when(
    sbinj1y  == 0              ~ "No",
    sbinj1y == 1              ~ "Yes"
  ))

# 9. What drug did you inject most often in the last month?Injecting - drug specific

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

# 10. How often did you inject drugs in the last month?

ethos <- ethos %>% 
  mutate(injectfreq.month = case_when(
    sbidl1m == 1              ~ "More than 3 times a day",
    sbidl1m == 5              ~ "Weekly or less",
    sbidl1m == 4              ~ "More than weekly, not daily",
    sbidl1m == 3              ~ "Once daily",
    sbidl1m == 2              ~ "2 - 3 times a day"
  ))


# Reordering the level of the injecting frequency attributes

ethos <- ethos %>% 
  mutate(injectfreq.month = fct_relevel(injectfreq.month,
                                     "Once daily", 
                                     "2 - 3 times a day",
                                     "More than 3 times a day",
                                     "Weekly or less",
                                     "More than weekly, not daily"
  ))


# 11. How many days have you injected drugs in the last month?

injectdays.month <- as.numeric(ethos$sbdaysinj)

ethos <- ethos %>% 
mutate(injectdays.month = as.numeric(injectdays.month))

# SECTION D: DRUG TREATMENT

# 55. Of all the following medications used to treat OUD, which would you prefer? 

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

# Create a new variable "prefer.treat.yn" that describe if participants would prefer treatment [yes/no]

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


# Collapse the recoded variables into simplified groups

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

# 56. Have you ever been on OAT? 

ethos <- ethos %>% 
  mutate(oat.ever.yn = case_when(
    sdost_ever  == 0              ~ "No",
    sdost_ever == 1              ~ "Yes"
  ))

# 57. Have you been on OAT in the last month? 

ethos <- ethos %>% 
  mutate(oat.month.yn = case_when(
    sdost_mon  == 0              ~ "No",
    sdost_mon == 1              ~ "Yes"
  ))

# 58. Have you been on OAT in the last 6 months? 

ethos <- ethos %>% 
  mutate(oat.6month.yn = case_when(
    sdost_6mon  == 0              ~ "No",
    sdost_6mon == 1              ~ "Yes"
  ))

# 59. Have you been on OAT in the last year? 

ethos <- ethos %>% 
  mutate(oat.year.yn = case_when(
    sdost_1yr  == 0              ~ "No",
    sdost_1yr == 1              ~ "Yes"
  ))

# 60. Are you currently receiving OAT? 

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

# Do you know your current methadone dose? 

ethos <- ethos %>% 
  mutate(methadonedose.yn = case_when(
    sdknddoes  == 0              ~ "No",
    sdknddoes == 1              ~ "Yes"
  ))

# 61. What is your current daily dose of methadone (mg)

methadone.dose <- as.numeric(ethos$sdmetd)

ethos <- ethos %>% 
  mutate(methadone.dose = as.numeric(methadone.dose))

# Do you know your current buprenorphine  dose? 

ethos <- ethos %>% 
  mutate(methadonedose.yn = case_when(
    sdknddoes  == 0              ~ "No",
    sdknddoes == 1              ~ "Yes"
  ))

# 62. What is your current daily dose of buprenorphine (mg)?

methadone.dose <- as.numeric(ethos$sdmetd)

ethos <- ethos %>% 
  mutate(methadone.dose = as.numeric(methadone.dose))

# 63. How often do you receive your dose of long-acting injectable buprenorphine? 

ethos <- ethos %>% 
  mutate(bupe.laib = case_when(
    sdodlbp  == 1              ~ "Weekly",
    sdodlbp == 2              ~ "Monthly"
  ))

# 64. What is your current dose of weekly long-acting injectable buprenorphine?

laib.dose.wk <- as.numeric(ethos$sdwbud)

ethos <- ethos %>% 
  mutate(laib.dose.wk = as.numeric(laib.dose.wk))


# 64. Do you know your current dose of weekly long-acting injectable buprenorphine?

ethos <- ethos %>% 
  mutate(laib.wk.yn = case_when(
    sdwbup  == 0              ~ "No",
    sdwbup == 1              ~ "Yes"
  ))

# 65. Do you know your current dose of monthly long-acting injectable buprenorphine?

ethos <- ethos %>% 
  mutate(laib.month.yn = case_when(
    sdmbuk  == 0              ~ "No",
    sdmbuk == 1              ~ "Yes"
  ))

# 65. What is your current dose of monthly long-acting injectable buprenorphine?

laib.dose.month <- as.numeric(ethos$sdmbud)

ethos <- ethos %>% 
  mutate(laib.dose.month = as.numeric(laib.dose.month))

# 66. Are you satisfied with your dose? 

ethos <- ethos %>% 
  mutate(dose.satisfied = case_when(
    sddses  == 1              ~ "Yes",
    sddses  == 2              ~ "No, too low",
    sddses  == 3              ~ "No, too high"
  ))

# 67. Where was your last dose dispensed? 


ethos <- ethos %>% 
  mutate(dose.location = case_when(
    sddsdis == 1 ~ "Public",
    sddsdis == 2 ~ "Private",
    sddsdis == 3 ~ "Doctor",
    sddsdis == 4 ~ "Pharmacy",
    sddsdis == 5 ~ "Prison", 
    sddsdis == 77 ~ "Other"))

# 68. Do you receive take-away doses? 

ethos <- ethos %>% 
  mutate(dose.takeaways.yn = case_when(
    sdtake  == 0              ~ "No",
    sdtake == 1              ~ "Yes"
  ))

# 69. In the last month, how many times per week did you collect your dose? 

collection.week <- as.numeric(ethos$sdlmdse)

ethos <- ethos %>% 
  mutate(collection.week = as.numeric(collection.week))

# 109. Please indicate which statement best describes your own health today? 

ethos <- ethos %>% 
  mutate(health.pain = case_when(
    sghcpd == 0 ~ "I have no pain or discomfort",
    sghcpd == 1 ~ "I have slight pain or discomfort",
    sghcpd == 2 ~ "I have moderate pain or discomfort",
    sghcpd == 3 ~ "I have severe pain or discomfort",
    sghcpd == 4 ~ "I have extreme pain or discomfort" 
    ))

# SECTION D: STIGMA AND DISCRIMINATION

# 103. In the last 12 months, have you experienced any stigma or discrimination in relation to your use of drugs for injecting?

ethos <- ethos %>% 
  mutate(stigma.inject = case_when(
    ggstig == 0 ~ "Never",
    ggstig == 1 ~ "Rarely",
    ggstig == 2 ~ "Sometimes",
    ggstig == 3 ~ "Often",
    ggstig== 4 ~ "Always" 
  ))

# Create a new yes/no variable that represents if someone has been stigmatised in relation to injecting drug use

ethos <- ethos %>% 
  mutate(stigma.inject.yn = case_when(
    ggstig == 0 ~ "No",
    ggstig == 1 ~ "Yes",
    ggstig == 2 ~ "Yes",
    ggstig == 3 ~ "Yes",
    ggstig== 4 ~ "Yes" 
  ))

# 104. In the last 12 months, have you experienced any stigma or discrimination in relation to your hepatitis C status?

ethos <- ethos %>% 
  mutate(stigma.hcv  = case_when(
    ggstighc == 0 ~ "Never",
    ggstighc == 1 ~ "Rarely",
    ggstighc == 2 ~ "Sometimes",
    ggstighc == 3 ~ "Often",
    ggstighc == 4 ~ "Always" 
  ))

# Create a new yes/no variable that represents if someone has been stigmatised in relation to hcv

ethos <- ethos %>% 
  mutate(stigma.hcv.yn  = case_when(
    ggstighc == 0 ~ "No",
    ggstighc == 1 ~ "Yes",
    ggstighc == 2 ~ "Yes",
    ggstighc == 3 ~ "Yes",
    ggstighc == 4 ~ "Yes" 
  ))

# 105. In the last 12 months, to what extent do you agree that health workers treated you negatively or different to other people? 

ethos <- ethos %>% 
  mutate(discrim = case_when(
    ggstigw == 0 ~ "Never",
    ggstigw == 1 ~ "Rarely",
    ggstigw == 2 ~ "Sometimes",
    ggstigw == 3 ~ "Often",
    ggstigw == 4 ~ "Always" 
  ))

# Create a new yes/no variable that represents if someone has been discriminated against by health workers

ethos <- ethos %>% 
  mutate(discrim.yn = case_when(
    ggstigw == 0 ~ "No",
    ggstigw == 1 ~ "Yes",
    ggstigw == 2 ~ "Yes",
    ggstigw == 3 ~ "Yes",
    ggstigw == 4 ~ "Yes"
  ))




# ##############################################################################################################################################################
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

#
















Likert

# Functions

# This function will be used to store likert questions into separate groups:

likert_store_groups <- function(db, groups) {
  attr(db, "likert.groups") <- groups
  db 
} 

# This function is helpful to plot likert questions based on the groups you defined using the above function
plot_likert_groups <- function(db, all=F, groups=NA, ...) {
  attrgroups <- attr(db, "likert.groups")
  
  if(is.null(attrgroups)) {
    stop("You have not stored any groups using likert_store_groups")
  }
  
  if(all) { 
    groups <- names(attrgroups) 
  }
  
  if(all(is.na(groups))) {
    stop("You have not specified a group name using groups=, or all=F")
  }
  
  for(e in groups) {
    group <- attrgroups[[e]]
    ligroup <- likert(db[,group], ...)
    print(plot(ligroup) + ggtitle(names(attrgroups[e])) )
  }
  
} 


# Function to round all the numbers in a df:
round_df <- function(ethos.subset, digits = 3) {
  nums <- vapply(ethos.subset, is.numeric, FUN.VALUE = logical(1))
  
  ethos.subset[,nums] <- round(ethos.subset[,nums], digits = digits)
  
  (ethos.subset)
}

ethos.subset <- ethos %>%
  dplyr::select(stigma.inject, 
                stigma.hcv,
                discrim, oat.yn, oat.prefer.group)

ethos.subset <- as_tibble(ethos.subset)

# split - separate likert questions, important they are saved as data.frame object
items <- as.data.frame(ethos.subset[, 1:3])

# apply - apply any functions/loops to split data. 
# 1: Create a vector of the text descriptions of likert choices 1-5
choices  = c("Never", "Rarely", "Sometimes", "Often", "Always")

# 2: Run for loop over likert data to change "1" -> "highly disagree", etc
for(i in 1:ncol(items)) {
  items[,i] = factor(items[,i], levels=1:5, labels=choices, ordered=TRUE)
}

# combine - put the data back together
ethos.subset <- bind_cols(ethos.subset[, 1:3], items)

head(ethos.subset)



# ######################
# MAPPING OF ETHOS SITES 
# ######################

library(nswgeo)
library(ggplot2)
library(ggautomap)

# Example data
overdose_data <- data.frame(
  Region = c("Sydney CBD", "Parramatta", "Newcastle", "Wollongong", "Central Coast", 
             "Bondi Beach", "Manly", "Darling Harbour", "Surry Hills", "Chatswood",
             "Liverpool", "Penrith", "Blacktown", "Campbelltown", "Hurstville",
             "Randwick", "Strathfield", "Bankstown", "Sutherland", "Mascot"),
  OverdoseCount = c(5, 8, 3, 10, 7, 4, 6, 2, 9, 5, 8, 3, 6, 4, 7, 5, 8, 9, 2, 6)
)



sydney_locations <- data.frame(
  Location = c("Location1", "Location2", "Location3", "Location4", "Location5",
               "Location6", "Location7", "Location8", "Location9", "Location10",
               "Location11", "Location12", "Location13", "Location14", "Location15"),
  Latitude = c(-33.8688, -33.8548, -33.8696, -33.8711, -33.8682,
               -33.8674, -33.8644, -33.8628, -33.8604, -33.8583,
               -33.8563, -33.8542, -33.8523, -33.8502, -33.8484),
  Longitude = c(151.2093, 151.2083, 151.2105, 151.2112, 151.2117,
                151.2123, 151.2141, 151.2159, 151.2181, 151.2207,
                151.2234, 151.2264, 151.2300, 151.2338, 151.2383))

nsw_map <- nswgeo::nsw_lga_sf()

nsw_map <- nswgeo::nsw_lga_sf()


ggplot(nswgeo::suburbs) + geom_sf(aes(fill = suburbname), show.legend = FALSE)


ggplot(suburbs) + geom_sf(aes(fill = suburbname))


# SCATTER MAPS

covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), sample_type = "random", size = 0.5) +
  coord_automap(feature_type = "nswgeo.lga", xlim = c(147, 153), ylim = c(-33.7, -29)) +
  guides(colour = guide_legend(override.aes = list(size = 1))) +
  theme_void()

# INSETS

covid_cases_nsw %>%
  ggplot(aes(location = lga)) +
  geom_boundaries(feature_type = "nswgeo.lga") +
  geom_geoscatter(aes(colour = type), size = 0.5) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lga", inset = configure_inset(
    centre = "Blacktown", radius = 40, units = "km",
    scale = 7, translation = c(400, -100)
  )) +
  theme_void()

# PACKED POINTS 

covid_cases_nsw %>%
  dplyr::filter(year >= 2021) %>%
  ggplot(aes(location = lhd)) +
  geom_boundaries(feature_type = "nswgeo.lhd") +
  geom_centroids(aes(colour = type), position = position_circle_repel_sf(scale = 35), size = 1) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Sydney", radius = 80, units = "km", feature_type = "nswgeo.lhd",
    scale = 6, translation = c(650, -100)
  )) +
  facet_wrap(vars(year)) +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(strip.text = element_text(size = 12))

# CHLOROPLETH MAP

covid_cases_nsw %>%
  ggplot(aes(location = lhd)) +
  geom_choropleth() +
  geom_boundaries(
    feature_type = "nswgeo.lhd", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lhd", inset = configure_inset(
    centre = "Western Sydney", radius = 60, units = "km",
    scale = 5, translation = c(400, -100)
  )) +
  scale_fill_steps(low = "#e6f9ff", high = "#00394d", n.breaks = 5, na.value = "white") +
  theme_void()

summarised_data <- data.frame(
  lhd = c("Western Sydney", "Sydney", "Far West", "Mid North Coast", "South Western Sydney"),
  cases = c(250, 80, 20, NA, 100)
)

# CHLOROPLEHT WITH SUMMARISED DATA

summarised_data <- data.frame(
  lhd = c("Western Sydney", "Sydney", "Far West", "Mid North Coast", "South Western Sydney"),
  cases = c(250, 80, 20, NA, 100)
)

summarised_suburbs <- data.frame(
  lhd = c("Northern Beaches", "Blacktown", "Sydney", "Randwick"),
  Overdoses = c(250, 80, 20, 130)
)

summarised_suburbs %>%
  ggplot(aes(location = lhd)) +
  geom_sf_inset(aes(fill = Overdoses), stat = "automap", colour = NA) +
  geom_boundaries(
    feature_type = "nswgeo.lga", colour = "black", linewidth = 0.1,
    outline.aes = list(colour = NA)
  ) +
  geom_inset_frame() +
  coord_automap(feature_type = "nswgeo.lga", inset = configure_inset(
    centre = "Blacktown", radius = 60, units = "km",
    scale = 3.5, translation = c(350, 0)
  )) +
  scale_fill_gradient(low = "#e6f9ff", high = "#00394d", na.value = "grey90") +
  theme_void() +
  xlim(147, 158) +
  ylim(-38, -30) +
  labs(x="Latitude",
       y="Longitude") +                    # clears other plot elements
  theme_bw() 
theme(panel.background = element_rect(fill = "lightblue"))








library(cartographer)
head(covid_cases_nsw)

covid_cases_nsw |>
  dplyr::count(lga) |>
  add_geometry(lga, feature_type = "nswgeo.lga") |>
  ggplot() +
  geom_sf(aes(fill = n)) +
  geom_sf(fill = NA, data = map_sf("nswgeo.lga")) +
  theme_void()

ggplot(lhd) + geom_sf(aes(fill = lhd_name, =), show.legend = TRUE) + theme_test()+
  geom_rect(aes(xmin = 147, xmax = 154, ymin = -36, ymax = -30), color = "red", fill = NA) 



ggplot(lhd)  + lhd_name = ("Hunter New England")

str(lhd)

ggplot(states)+ geom_sf(aes(fill = STE_NAME21), show.legend = TRUE) + theme_bw()

?theme


theme(panel.background = element_rect(fill = "lightblue")) +
  guides(size = "none") +
  ggsn::north(location = "topleft", scale = 0.8, symbol = 12,
              x.min = 151.5, x.max = 152.5, y.min = -36, y.max = -38) +
  ggsn::scalebar(location = "bottomleft", dist = 100,
                 dist_unit = "km", transform = TRUE, 
                 x.min=150.5, x.max=152, y.min=-38, y.max=-30,
                 st.bottom = FALSE, height = 0.025,
                 st.dist = 0.05, st.size = 2.5)

str(suburbs)






# NOTES

## laib.dose variable returned NULL - check


Install.packages("RPubs")

# This is function to extract code from RPubs
#
#' @title Extract Code From RPubs Article
#' @description Extract code from an RPubs article.
#' @param url Character. URL of RPubs article, e.g. url = "https://rpubs.com/aephidayatuloh/sendgmail".
#' @param path Character. File name for the extracted code as R script, e.g. \code{code.R}.
#' @param output Logical. Should extraction include output of the code? Default to \code{FALSE}, means only R script will be extracted.
#' @import rvest
#' @importFrom xml2 read_html
#' @return vector
#' @details If \code{files = NULL} then the extracted script will be print on console or as vector if you assign to an object. One code block is one element of vector.
#' @examples
#' rpubs_code(url = "https://rpubs.com/aephidayatuloh/sendgmail", path = NULL, output = FALSE)
#'
#'
#'
#' @export
rpubs_code <- function(url, path = NULL, output = FALSE){
  if(substr(sub("https?://(www\\.)?", "", url), 1, 9) != "rpubs.com"){
    stop("Only support article from https://rpubs.com")
  }
  
  pg <- read_html(url)
  
  iframe_link <- paste0("http:",
                        html_attr(
                          html_nodes(
                            html_nodes(
                              html_nodes(
                                html_nodes(pg, "body"),
                                "div#pagebody"),
                              "div#payload"),
                            "iframe"),
                          "src")
  )
  
  node <- ifelse(output, "pre", "pre.r")
  
  code <- html_text(
    html_nodes(
      read_html(iframe_link),
      node)
  )
  
  script <- paste0(sprintf("# %s\n\n", url), paste(gsub("\n", "", code), collapse = "\n\n"))
  
  if(is.null(path)){
    return(script)
  } else {
    writeLines(text = script, con = path)
  }
}


install.packages("read_htmlm")
library(rpubs)
require(rpubs)
library(read_html)

install.packages('xml2')
library('xml2')

help(rpubs_code)

install.packages("rvest")
library(rvest)

# install.packages("devtools")
devtools::install_github("aephidayatuloh/rpubs")

library(rpubs)
article <- "http://rpubs.com/Umer_Farooq/1038792"
rpubs_code(url = article, path = "C:/Users/mjstowe/OneDrive - UNSW/Desktop/R/1038792.R", output = FALSE)


ethos %>% 
  pivot_wider(
    rows = starts_with("ibarcode")
  )

View(ethos)
# library
library(likert) 

# Use a provided dataset
data(pisaitems) 
items28 <- ethos[, substr(names(ethos), 1, 387) == "stigma.inject"] 

# Build plot
p <- likert(items28) 
plot(p)
