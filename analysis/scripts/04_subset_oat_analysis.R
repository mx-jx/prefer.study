# PREFER 
# Analysis of select variables as a subset of the complete PREFER dataset


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

## Create theme for all 'gtsummary' objects [that is in line with most            manuscript recommendations, e.g., Times New Roman font; justified alignment    and compact spacing  ]

apa_theme <- function (ft)  {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>% 
    flextable::fontsize(size = 10, part = "all") %>% 
    flextable::align(align = "left", part = "all") %>% 
    flextable::align(align = "left", part = "header") %>% 
    flextable::border_remove() %>% 
    flextable::hline_top(border = officer::fp_border(width = 1.5), part = "all") %>% 
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "header") %>% 
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "footer") %>% 
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::autofit()
}

## Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())

## Load the files

load(
  here::here("data_processed", "prefer_working.rda"))

## Select a subset of variables that simplifies the full dataset and also excludes variables that relate exclusively to those currently receiving OAT [see "04_subset_oat_analysis.R" for analysis of OAT-associated variables] 

p.oat <- p %>%
  dplyr::select(age, age.group, edu.factor,genderall.factor, gender.mf,
                income.factor.yn, homeless.factor.yn,         
                prison.factor, chronicpain.factor.yn, prefer.factor,
                heroin.month.f, prescribedmethadone.month.f, 
                nonmethadone.month.f, prescribedsuboxone.month.f,
                nonsuboxone.month.f, prescribedsuboxone.month.f, 
                cocaine.month.f, meth.month.f, benzo.month.f,
                oat.yn.f, anyinject.yn.f,
                injfreq.factor, injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, doselocation.factor, oatever.factor, mwthadoneever.factor, bupeever.factor, paytx.yn.f, paytx.factor.month, timedose.factor, dose.km.factor, usedrugs.oat 
  ) 

# Generate a table describing the characteristics of the participants 

p.oat.character <-  
  p.oat %>% 
  filter(oat.yn.f == "Yes") %>%
  tbl_summary(include = c(age, age.group, edu.factor,genderall.factor, 
                          gender.mf,
                          income.factor.yn, homeless.factor.yn,         
                          prison.factor, chronicpain.factor.yn, prefer.factor,
                          heroin.month.f, prescribedmethadone.month.f, 
                          nonmethadone.month.f, prescribedsuboxone.month.f,
                          nonsuboxone.month.f, prescribedsuboxone.month.f, 
                          cocaine.month.f, meth.month.f, benzo.month.f,
                          oat.yn.f, anyinject.yn.f,
                          injfreq.factor, injectingheroin.month.f,
                          injectingnonmethadone.month.f, 
                          injectingprescribedmethadone.month.f,
                          injectingprescribedsuboxone.month.f, 
                          injectingnonsuboxone.month.f,
                          injectjngcocaine.month.f,
                          injectingmeth.month.f, injectingbenzo.month.f,
                          doselocation.factor, oatever.factor, mwthadoneever.factor, bupeever.factor, paytx.yn.f, paytx.factor.month, timedose.factor, dose.km.factor, usedrugs.oat
  ),
  type = all_categorical()~"categorical",
  missing = "no",
  statistic = all_continuous() ~ ("{median} ({p25} - {p75})")) %>%
  add_stat_label()%>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = "**Enrolled** \n **(n = {N})**",
  ) %>%
  modify_caption("Table 1. Participant Characteristics")

# Convert to flextable [the suffix "ft" = "flextable"]

p.oat.character.ft <- p.oat.character %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

p.oat.character.ft

# Save the table as an a) .docx file; and b) .html file 

# a)




Save the script file
save(p, file = 
       here::here("data_processed", "prefer_working.rda")
)

