# Prefer project
## Create factors


load(
  here::here("data_processed", "prefer_working.rda")
    )

library(rlang)
library(finalfit)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rlang)
library(broom)
library(tidyverse)
library(readxl)
library(purrr)
library(pacman)
library(skimr)
library(haven)
library(RStata)
library(freqtables)
library(forcats)
library(lmtest)
library(survival)
library(survminer)
library(epiR)
library(Epi)
library(broom)
library(broom.mixed)
library(janitor)
library(psych)
library(epiDisplay)
library(gmodels)
library(finalfit)
library(jsmodule)
library(tableone)
library(effsize)
library(dynpred)
library(mod)
library(car)
library(gtsummary)
library(gt)
library(officer)
library(sjlabelled)
library(codebook)
library(codebookr)
library(likert)

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
  haven
)


theme_set(theme_bw())


# Select variables that will be used from the dataset

prefer <- prefer %>%
  dplyr::select(age, age.group, edu.yr10, gender,income, accom, prefer.mb, 
                gender.all, gender.mf, hccard, prison, chronicpain.yn,
                income.yn, hccard.yn, accom.type, accom.yn, homeless,
                homeless.factor, prison.yn, chronicpain.yn, prefer.mb,
                oat_pref, oat_pref_cat, oat_pref_only, pref_oat,
                prefertreat.yn, heroin.month, prescribedmethadone.month, 
                nonmethadone.month, prescribedsuboxone.month, 
                nonsuboxone.month, bupe1month, otheropi.month, 
                cocaine.month, meth.month, benzo.month, anyinject.yn,
                injectingfreq.month, injectingheroin.month,
                injectingnonmethadone.month, injecting_methadonemonth,
                injectingnonsuboxone.month,injecting_bupemonth,
                injectingnonopi.month, injectingcocaine.month,
                injectingmeth.month, injectingbenzo.month, oat.yn,
                currentoat_treat, oatcurrentmed, dosing.point, 
                clinic.attend, drugsoat, heroinoat, opioidsoat, methoat, 
                benzooat, missd.month, misseddose, dose.km, kmdosingsite,
                timetodose,paytx.month, paytx.yn, oatever.yn, 
                methadoneever.yn, bupeever.yn, doselocation.group, oat.yn,
                q0ost, injfreq, ldoseloc, usedrugs.oat, drgstx_1, drgstx_2,
                drgstx_3, drgstx_4, drgstx_5, drgstx_6
                ) 
                

prefer %>% glimpse()

prefer %>% ff_glimpse()

# Recode variables

prefer <- prefer %>%
  mutate(
    # recoding level of education
    edu.factor = factor(edu.yr10) %>% 
      fct_recode("Completed ≥ 10 years school education" = 
                   "Completed ≥ 10 years school education",
                 "Completed < 10 years school education" =
                   "Completed < 10 years school education") %>% 
      ff_label("Level of education"), 
    
    # gender 
    genderall.factor = factor(gender) %>%
      fct_recode("Male" = "1",
                 "Female"  = "2", 
                 "Transgender" = "4") %>% 
      ff_label("Gender"),
    
    #recoding income
    income.factor.yn = factor(income) %>%
      fct_recode("Paid" = "2",
                 "Other"  = "1") %>% 
      ff_label("Income"),

    #recoding healthcare card
    hccard.factor.yn = factor(hccard) %>%
      fct_recode("Yes" = "1",
                 "No"  = "2") %>% 
      ff_label("Healthcare card"),
    
    #recoding accomodation
    homeless.factor.yn = factor(accom) %>%
      fct_recode("Yes" = "1",
                 "Yes"  = "2",
                 "Yes"  = "3",
                 "Yes"  = "4",
                 "Yes"  = "5",
                 "Yes"  = "6",
                 "Yes"  = "7",
                 "Yes"  = "8",
                 "Yes"  = "9",
                 "No"  = "10",
                 "Yes"  = "11") %>% 
      ff_label("Homelessness") %>%
      fct_relevel("Yes", "No"),
    
    #recoding incarceration
    prison.factor = factor(prison) %>%
      fct_recode("No" = "1",
                 "Yes > 6 months"  = "2",
                 "Yes < 6 months"  = "3") %>% 
      ff_label("Incarceration") %>%
      fct_relevel("No",
                  "Yes > 6 months",
                  "Yes < 6 months"), 
    
    #recoding and releveling chronicpain
    chronicpain.factor.yn = factor(chronicpain.yn) %>%
      ff_label("Chronic pain") %>%
      fct_relevel("Yes",
                  "No"), 
    
    #recoding and releveling OAT preference
    prefer.factor = factor(prefer.mb) %>%
      ff_label("OAT preference") %>%
      fct_relevel("Buprenorphine",
                  "Methadone"), 
    
    oat.pref.all = factor(q0ost) %>%
      fct_recode(
        "Methadone or biodone syrup" = "1",
        "Buprenorphine oral" = "2",
        "Buprenorphine long acting injectable" = "3",
        "any - no preference" = "4",
        "none - no medication" = "5",
        "none - no appropriate medication" = "6",
        "none - not interested in treatment" = "7")%>%
      ff_label("OAT preference all"),
    
    # #recoding and releveling OAT preference collapsed
    # prefer.collapsed.factor = factor(oat_pref_cat) %>%
    #   ff_label("OAT preference collapsed") %>%
    #   fct_relevel("Methadone",
    #               "Buprenorphine",
    #               "Any",
    #               "None"),
    # 
    # past month heroin use
    heroin.month.f = factor(heroin.month) %>%
      ff_label("Recent heroin use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month non-prescribed methadone use
    nonmethadone.month.f = factor(nonmethadone.month) %>%
      ff_label("Recent non-prescribed methadone use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month prescribed methadone use
    prescribedmethadone.month.f = factor(prescribedmethadone.month) %>%
      ff_label("Recent prescribed methadone use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month non-prescribed suboxone use
    nonsuboxone.month.f = factor(nonsuboxone.month) %>%
      ff_label("Recent non-prescribed use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month prescribed suboxone use
    prescribedsuboxone.month.f = factor(prescribedsuboxone.month) %>%
      ff_label("Recent prescribed suboxone use") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # past month cocaine use
    cocaine.month.f = factor(cocaine.month) %>%
      ff_label("Recent cocaine use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month methamphetamine use
    meth.month.f = factor(meth.month) %>%
      ff_label("Recent methamphetamine use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month benzodiazepine use
    benzo.month.f = factor(benzo.month) %>%
      ff_label("Recent benzodiazepine use") %>%
      fct_relevel("No",
                  "Yes"),
    
    # Current OAT medication prescribed
    oatcurrentmed.f = factor(oatcurrentmed)%>%
      ff_label("Current OAT medication") %>%
    fct_relevel("Methadone",
                "Suboxone",
                "Subutex",
                "Buvidal", 
                "None"),
    
    # Current OAT [yes/no]
    oat.yn.f = factor(oat.yn)%>%
      ff_label("Currently receiving OAT") %>%
      fct_relevel("No", 
                  "Yes"),
    
    # Injecting drug use
    anyinject.yn.f = factor(anyinject.yn)%>%
      ff_label("Drug injecting") %>%
      fct_relevel("No", 
                  "Yes"),
    
    # Injecting drug use frequency
    injfreq.factor = factor(injfreq)%>%
      fct_recode(
        "Did not inject" = "1",
        "Weekly or less" = "2",
        "More than weekly, not daily" = "3",
        "Once daily" = "4",
        "2 - 3 times a day" = "5",
        "More than 3 times a day" = "6")%>%
      ff_label("Drug injecting frequency"),
    
    # Injecting drug use frequency collapsed
    injfreq.collapsed.f = factor(injfreq)%>%
      fct_recode(
        "None" = "1",
        "< Daily" = "2",
        "< Daily" = "3",
        ">= Daily" = "4",
        ">= Daily" = "5",
        ">= Daily" = "6")%>%
      ff_label("Drug injecting frequency grouped"),
    
    # past month heroin injecting
    injectingheroin.month.f = factor(injectingheroin.month) %>%
      ff_label("Recent heroin injecting") %>%
      fct_relevel("No",
                  "Yes"),  
    

    # past month non-prescribed methadone injecting
    injectingnonmethadone.month.f = factor(injectingnonmethadone.month) %>%
      ff_label("Recent non-prescribed methadone injecting") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month prescribed methadone injecting
    injectingprescribedmethadone.month.f = factor(injecting_methadonemonth) %>%
      ff_label("Recent prescribed methadone injecting") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month non-prescribed suboxone injecting
    injectingnonsuboxone.month.f = factor(injectingnonsuboxone.month) %>%
      ff_label("Recent non-prescribed suboxone injecting") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month prescribed suboxone injecting
    injectingprescribedsuboxone.month.f = factor(injecting_bupemonth) %>%
      ff_label("Recent prescribed suboxone injecting") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # past month cocaine injecting
    injectjngcocaine.month.f = factor(injectingcocaine.month) %>%
      ff_label("Recent cocaine injecting") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month methamphetamine injecting
    injectingmeth.month.f = factor(injectingmeth.month) %>%
      ff_label("Recent methamphetamine injecting") %>%
      fct_relevel("No",
                  "Yes"),
    
    # past month benzodiazepine use
    injectingbenzo.month.f = factor(injectingbenzo.month) %>%
      ff_label("Recent benzodiazepine injecting") %>%
      fct_relevel("No",
                  "Yes")

    # OAT dose collection site
    
    doselocation.factor = factor(ldoseloc)%>%
      fct_recode(
        "Public clinic" = "1",
        "Private clinic" = "2",
        "Other" = "3",
        "Pharmacy" = "4",
        "Other" = "5",
        "Other" = "6", 
        "Other" = "6")%>%
      ff_label("OAT dosing site")%>%
      fct_relevel("Pharmacy", 
                  "Public clinic", 
                  "Private clinic", 
                  "Other"),
    
    
        # Injecting drug use frequency
    injfreq.factor = factor(injfreq)%>%
      fct_recode(
        "Did not inject" = "1",
        "Weekly or less" = "2",
        "More than weekly, not daily" = "3",
        "Once daily" = "4",
        "2 - 3 times a day" = "5",
        "More than 3 times a day" = "6")%>%
      ff_label("Drug injecting frequency"),
    
    # OAT ever
    oatever.factor = factor(oatever.yn) %>%
      ff_label("Ever received OAT") %>%
      fct_relevel("No",
                  "Yes"), 
    
    # Methadone ever
    mwthadoneever.factor = factor(methadoneever.yn) %>%
      ff_label("Ever received methadone") %>%
      fct_relevel("No",
                  "Yes"), 
    
    
    # Buprenorphine ever
    bupeever.factor = factor(bupeever.yn) %>%
      ff_label("Ever received buprenorphine") %>%
      fct_relevel("No",
                  "Yes"),
    
    # Pay for OAT
    paytx.yn.f = factor(paytx.yn) %>%
      ff_label("Pay for OAT") %>%
      fct_relevel("No",
                  "Yes"),
    
    # Average cost
    paytx.factor.month = factor(paytx.month)%>%
      fct_relevel(
        "1 - 99 AUD",
        "100 - 149 AUD",
        "150 - 399 AUD")%>%
      ff_label("Average monthly OAT cost"),
    

# Average time taken to travel to dosing site

timedose.factor = factor(timetodose)%>%
  fct_relevel(
    "< 15 minutes",
    "15 - < 30 minutes",
    "30 minutes - < 2 hoursMore than 5 days")%>%
  ff_label("Average time to dosing site"),

# Average no. of days when a dose was missed

dose.km.factor = factor(dose.km)%>%
  fct_relevel(
    "Less than 5 km",
    "5 - 9 km",
    "More than 5 days")%>%
  ff_label("Average distance to dosing site"))
    
# Recode the variable drug use while on OAT

    prefer <- prefer %>% 
      mutate(usedrugs.oat = case_when(
        drgstx_1 == 1  ~ "No",
        drgstx_2 == 1  ~ "Heroin",
        drgstx_3 == 1  ~ "Pharmaceutical opioids",
        drgstx_4 == 1  ~ "Methamphetamine",
        drgstx_5 == 1  ~ "Benzodiazepines",
        drgstx_6 == 1  ~ "Other"
      ))

# Relevel the variable 
    
    prefer <- prefer %>% 
      mutate(usedrugs.oat = fct_relevel(usedrugs.oat,
                                        "No",
                                        "Heroin",
                                        "Pharmaceutical opioids",
                                        "Methamphetamine",
                                        "Benzodiazepines",
                                        "Other"))

# Save
save(prefer, file = 
       here::here("data_processed", "prefer_working.rda")
)