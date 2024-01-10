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


  here::i_am("data_processed/prefer_working.rda")
  
load(
  here::here("data_processed", "prefer_working.rda"))

## Select a subset of variables that simplifies the full dataset and also excludes variables that relate exclusively to those currently receiving OAT [see "04_subset_oat_analysis.R" for analysis of OAT-associated variables] 

p.sub <- p %>%
  dplyr::select(age, agegroup, edu.factor ,gender.all, gender.mf,
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
                injectingmeth.month.f, injectingbenzo.month.f,
                age, age.group, edu.factor ,genderall.factor, gender.mf,
                income.factor.yn, homeless.factor.yn,         
                prison.factor, chronicpain.factor.yn, prefer.factor,
                heroin.month.factor, prescribedmethadone.month.f, 
                nonmethadone.month.factor, prescribedsuboxone.month.f,
                nonbupe.month.factor, prescribedsuboxone.month.f, 
                cocaine.month.factor, meth.month.factor, benzo.month.factor, otheropi.month.factor, 
                oat.yn.f, anyinject.yn.f,
                injfreq.factor, injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, methadoneever.yn, 
                bupeever.factor, oatpref.all
  ) 



# Generate a table describing the characteristics of the participants stratified by their preference for burpenorphine or methadone. 

# Formatting
## In this table stratified by preference for methadone or buprenorphine, we want the order the preferences are presented as follows: Overall>Methadone>Buprenorphine. To acheive this we need to relevel the preference variable. 


p.sub <- p.sub %>%
  mutate(prefer.factor = fct_relevel(prefer.factor,
                                     "Methadone", 
                                     "Buprenorphine"))

# Table: Participant characteristics stratified by preference for methadone or buprenorphine

tbl.stratified <-  
  p.sub %>% 
  tbl_summary(by = prefer.factor, 
              include = c(age, age.group, edu.yr10, genderall.factor, 
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
                          injectingmeth.month.f, injectingbenzo.month.f 
  ),
  type = all_categorical()~"categorical",
  missing = "no",
  percent = "row",
  statistic = all_continuous() ~ ("{median} ({p25} - {p75})")) %>%
  add_stat_label()%>%
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = '**Enrolled** \n **(n = {n})**',
    stat_1 = '**Methadone** \n **(n = {n})**',
    stat_2 = '**Buprenorphine** \n **(n = {n})**'
  ) %>%
  modify_caption("Table X. Participant characteristics stratified by preference for methadone or buprenorphine.")


# Convert to flextable [the suffix "ft" = "flextable"]

tbl.stratified.ft <- tbl.stratified %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

tbl.stratified.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
tbl.stratified.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics stratified by OAT preference.html")

# b)
tbl.stratified.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics stratified by oat preference.docx")


stratified.model <-  
  p.sub %>% 
  tbl_summary(by = prefer.factor, 
              include = c(age, age.group, 
                          gender.mf, edu.factor,
                          income.factor.yn, homeless.factor.yn,         
                          prison.factor, chronicpain.factor.yn,
                          heroin.month.factor,  
                          nonmethadone.month.factor,
                          nonbupe.month.factor, 
                          cocaine.month.factor, meth.month.factor, 
                          benzo.month.factor,
                          oat.yn.f, methadoneever.yn, bupeever.factor
              ),
              type = all_categorical()~"categorical",
              missing = "no",
              percent = "row",
              statistic = all_continuous() ~ ("{median} ({p25} - {p75})")) %>%
  add_stat_label()%>%
  add_overall() %>% 
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = '**Enrolled** \n **(n = {n})**',
    stat_1 = '**Methadone** \n **(n = {n})**',
    stat_2 = '**Buprenorphine** \n **(n = {n})**'
  ) %>%
  modify_caption("Table 2. Participant characteristics stratified by preference for methadone or buprenorphine.")


# Convert to flextable [the suffix "ft" = "flextable"]

stratified.model.ft <- stratified.model %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

stratified.model.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
stratified.model.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/subset participant characteristics stratified by OAT preference.html")

# b)
stratified.model.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/subset participant characteristics stratified by oat preference.docx")


# Create a .xls file


library(YesSiR) # to export a flextable into MS Excel: exportxlsx() function

# export your flextable as a .xlsx in the current working directory
exportxlsx(stratified.model.ft, path = "subset participant characteristics stratified by oat preference.xlsx")

# Save
save(p, file = 
       here::here("data_processed", "prefer_subset_working.rda")
)
