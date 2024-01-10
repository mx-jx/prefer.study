# PREFER
# Tables

# Load packages
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
  tableone
)

## Load the files

load(
  here::here("data_processed", "prefer_tables_working.rda"))

load(
  here::here("data_processed", "prefer_working.rda"))

load(
  here::here("data_processed", "prefer_subset_working.rda"))

load(
  here::here("data_processed", "prefer_logistic_working.rda"))


# Set the theme to compact [reduce spacing around text and numbers]

## Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())

# Selecting variables that relate to OAT among participants who are currently receiving OAT

# With OAT related variables

oat <- p.relevel %>%
  filter(oat.yn.f == "Yes")%>% # By filtering for oat.yn.f = "yes" we are selecting on those participants currently receiving OAT
  dplyr::select(age, agegroup, edu.factor ,genderall.factor, gender.mf,
                income.factor.yn, homeless.factor.yn, homeless.factor,        
                prison.factor, chronicpain.factor.yn, prefer.factor,
                heroin.month.factor, prescribedmethadone.month.f, 
                nonmethadone.month.factor, prescribedsuboxone.month.f,
                nonbupe.month.factor, prescribedsuboxone.month.f, otheropi.month.factor, 
                cocaine.month.factor, meth.month.factor, benzo.month.factor, 
                anyinject.yn.f,otheropi1month,
                injfreq.factor, injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, methadoneever.yn, 
                bupeever.factor, oatcurrentmed.f, usedrugs.oat, paytx.yn.f, 
                paytx.factor.month,doselocation.factor, dose.km.factor, timedose.factor, oatpref.all, kmdose) 


oat <- p %>%
  filter(oat.yn.f == "Yes")%>% # By filtering for oat.yn.f = "yes" we are selecting on those participants currently receiving OAT
  dplyr::select(age, agegroup, edu.factor ,genderall.factor, gender.mf,
                income.factor.yn, homeless.factor.yn, homeless.factor,        
                prison.factor, chronicpain.factor.yn, prefer.factor,
                heroin.month.factor, prescribedmethadone.month.f, 
                nonmethadone.month.factor, prescribedsuboxone.month.f,
                nonbupe.month.factor, prescribedsuboxone.month.f, otheropi.month.factor, 
                cocaine.month.factor, meth.month.factor, benzo.month.factor, 
                anyinject.yn.f,otheropi1month,
                injfreq.factor, injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, methadoneever.yn, 
                bupeever.factor, oatcurrentmed.f, usedrugs.oat, paytx.yn.f, 
                paytx.factor.month,doselocation.factor, dose.km.factor, 
                timedose.factor, oatpref.all, kmdose) 


# OAT only variables and participants

# All variables

pts_oat_full <-  
  oat %>% 
  tbl_summary(include = c(age,agegroup, genderall.factor, edu.factor, 
                          
                          income.factor.yn, homeless.factor,         
                          prison.factor, chronicpain.factor.yn, 
                          heroin.month.factor,  
                          nonmethadone.month.factor,
                          nonbupe.month.factor,  otheropi.month.factor, 
                          cocaine.month.factor, meth.month.factor, benzo.month.factor,
                          anyinject.yn.f,
                          injfreq.factor, injectingheroin.month.f,
                          injectingnonmethadone.month.f, 
                          injectingnonsuboxone.month.f,
                          injectjngcocaine.month.f,
                          injectingmeth.month.f,
                          methadoneever.yn, bupeever.factor,
                          oatcurrentmed.f, usedrugs.oat, paytx.yn.f, 
                          paytx.factor.month,doselocation.factor, kmdose, 
                          dose.km.factor, timedose.factor
  ),
  # type = all_categorical() ~ "categorical",
  label = 
    list(age ~ "Age",
         agegroup ~ "Age group",
         usedrugs.oat ~ "Drugs used while on OAT",
         methadoneever.yn ~ "Ever received methadone", 
         homeless.factor ~ "Currently homeless",
         kmdose ~ "Distance to OAT collection site"),
  missing = "no",
  statistic = all_continuous() ~ ("{median} ({p25} - {p75})"),
  digits = list(all_categorical() ~ c(0)
  ))%>%
  add_stat_label()%>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = "**Enrolled** \n **(n = {N})**",
  )%>%
  remove_row_type(variable = oatcurrentmed.f, 
                  type = "level", level_value = "None")
# modify_caption("Table 1. Participant Characteristics")


# Convert to flextable [the suffix "ft" = "flextable"]

pts_oat_full.ft <- pts_oat_full %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

pts_oat_full.ft

# Save the table as an a) .docx file; and b) .html file 

# a)

pts_oat_full.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_full.html")

# b)
pts_oat_full.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_full.docx")

# c)
pts_oat_full.ft %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_full.png")

# OAT variables and participants

# Subset - select variables only

pts_oat_sub <-  
  oat %>% 
  tbl_summary(include = c(age, genderall.factor,
                          methadoneever.yn, bupeever.factor,
                          oatcurrentmed.f, usedrugs.oat, paytx.yn.f, 
                          clinic.attend.factor, 
  ),                          dose.km.factor, dosetime.factor

  # type = all_categorical() ~ "categorical",
  label = 
    list(age ~ "Age",
         # agegroup ~ "Age group",
         usedrugs.oat ~ "Drugs used while on OAT",
         methadoneever.yn ~ "Ever received methadone"), 
  # homeless.factor ~ "Currently homeless"),
  missing = "no",
  statistic = all_continuous() ~ ("{median} ({p25} - {p75})"),
  digits = list(all_categorical() ~ c(0)
  ))%>%
  add_stat_label()%>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = "**Enrolled** \n **(n = {N})**",
  )%>%
  remove_row_type(variable = oatcurrentmed.f, 
                  type = "level", level_value = "None")
# modify_caption("Table 1. Participant Characteristics")


# Convert to flextable [the suffix "ft" = "flextable"]

pts_oat_sub.ft <- pts_oat_sub %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

pts_oat_sub.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
pts_oat_sub.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_subset.html")

# b)
pts_oat_sub.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_subset.docx")


# c)
pts_oat_sub.ft %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_subset.png")



# Subset - select variables only stratified by OAT preference

oat <- oat %>%
  mutate(prefer.factor = fct_relevel(prefer.factor, "Methadone", "Buprenorphine"))




pts_oatfull_stratified <-  
  oat %>% 
  tbl_summary(by = prefer.factor, include = c(age,agegroup, genderall.factor, edu.factor, 
                                              
                                              income.factor.yn, homeless.factor,         
                                              prison.factor, chronicpain.factor.yn, 
                                              heroin.month.factor,  
                                              nonmethadone.month.factor,
                                              nonbupe.month.factor,  
                                              otheropi.month.factor, 
                                              cocaine.month.factor, 
                                              meth.month.factor, benzo.month.factor,
                                              anyinject.yn.f,
                                              injfreq.factor, injectingheroin.month.f,
                                              injectingnonmethadone.month.f, 
                                              injectingnonsuboxone.month.f,
                                              injectjngcocaine.month.f,
                                              injectingmeth.month.f,
                                              methadoneever.yn, bupeever.factor,
                                              oatcurrentmed.f, usedrugs.oat, paytx.yn.f, 
                                              paytx.factor.month,doselocation.factor, 
                                              dose.km.factor, timedose.factor
  ),
  # type = all_categorical() ~ "categorical",
  label = 
    list(age ~ "Age",
         agegroup ~ "Age group",
         usedrugs.oat ~ "Drugs used while on OAT",
         methadoneever.yn ~ "Ever received methadone", 
         homeless.factor ~ "Currently homeless"),
  type = all_categorical()~"categorical",
  missing = "no",
  percent = "row",
  statistic =(all_continuous()  ~ "{median} ({p25} - {p75})"))%>%
  add_stat_label()%>%
  add_overall(statistic = list(all_continuous()  ~ "{median} ({p25} - {p75})",
                               all_categorical() ~ "{n}")) %>% 
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = '**Overall** \n **(n = {n})**',
    stat_1 = '**Methadone** \n **(n = {n})**',
    stat_2 = '**Buprenorphine** \n **(n = {n})**'
  ) %>%
  bold_labels()%>%
  remove_row_type(variable = oatcurrentmed.f, 
                  type = "level", level_value = "None")


# Convert to flextable [the suffix "ft" = "flextable"]

pts_oatfull_stratified.ft <- pts_oatfull_stratified %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

pts_oatfull_stratified.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
pts_oatfull_stratified.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oatfull_stratified by OAT preference.html")

# b)
pts_oat_stratified.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oatfull_stratified by OAT preference.docx")


# c)
pts_oat_stratified.ft %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oatfull_stratified by OAT preference.png")



pts_oat_stratified <-  
  oat %>% 
  tbl_summary(by = prefer.factor, include = c(age, gender.mf, heroin.month.factor,
                                              nonmethadone.month.factor, nonbupe.month.factor, oatcurrentmed.f,
                                              methadoneever.yn, bupeever.factor,
                                              usedrugs.oat, paytx.yn.f, 
                                              doselocation.factor, clinic.attend, 
                                               kmdose,
                                              , dosetime.factor, prefer.factor
  ),
  # type = all_categorical() ~ "categorical",
  label = 
    list(age ~ "Age",
         gender.mf ~ "Gender",
         usedrugs.oat ~ "Drugs used while on OAT",
         methadoneever.yn ~ "Ever received methadone"),
  type = all_categorical()~"categorical",
  missing = "no",
  percent = "row",
  statistic =(all_continuous()  ~ "{median} ({p25} - {p75})"))%>%
  add_stat_label()%>%
  add_overall(statistic = list(all_continuous()  ~ "{median} ({p25} - {p75})",
                               all_categorical() ~ "{n}")) %>% 
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = '**Overall** \n **(n = {n})**',
    stat_1 = '**Methadone** \n **(n = {n})**',
    stat_2 = '**Buprenorphine** \n **(n = {n})**'
  ) %>%
  bold_labels()%>%
  remove_row_type(variable = oatcurrentmed.f, 
                  type = "level", level_value = "None")


# Convert to flextable [the suffix "ft" = "flextable"]

pts_oat_stratified.ft <- pts_oat_stratified %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

pts_oat_stratified.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
pts_oat_stratified.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_stratified by OAT preference.html")

# b)
pts_oat_stratified.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_stratified by OAT preference.docx")


# c)
pts_oat_stratified.ft %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics oat_stratified by OAT preference.png")




# Final model


oat_mod_final <- tbl_regression(oat_model_final, exponentiate = TRUE,  
                                label = 
                                  list(age ~ "Age",
                                       gender.mf ~ "Gender",
                                       methadoneever.yn ~ "Ever received methadone" 
                                  ),
                                add_estimate_to_reference_rows = FALSE,
                                # show_single_row = c(age, gender.mf, heroin.month.factor),
                                pvalue_fun = function(x) style_pvalue(x, digits = 3))%>%
  # pvalue_fun = ~style_sigfig(., digits = 4)                              )%>%
  modify_header(
    label = '**Variable**',
    estimate = '**aOR**',
    ci = '**95% CI**',
    p.value = '**p-value**'
  ) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE) %>%
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate} ({conf.low} - {conf.high})"
  ) %>%
  modify_header(estimate ~ "**aOR (95% CI)**") %>%
  modify_column_hide(c(ci)) %>% 
  bold_labels() %>%
  modify_footnote(everything() ~ NA, abbreviation = TRUE)

# Convert to flextable

oat_mod_final.ft <- oat_mod_final %>%
  as_flex_table()%>%
  apa_theme()

# Display the table
oat_mod_final.ft


# Save the table as an a) .docx file; and b) .html file 

# a)
oat_mod_final.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/final_model_oat.html")

# b)
oat_mod_final.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/final_model_oat.docx")


# c)
oat_mod_final.ft %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/final_model_oat.png")


## Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())

# Merged tables - OAT participants

oat_table_merged <-
  tbl_merge(
    tbls = list(pts_oat_stratified, oat_cor, oat_mod_final),
    tab_spanner = c("** **", "**Unadjusted model**", "**Adjusted model**")) %>%
  modify_header(label = "**Variable**") %>% # update the column header
  modify_footnote(everything() ~ NA)%>% 
  as_flex_table()%>%
  apa_theme()

# Display the table
oat_table_merged

# Save the table as a) .html file; b) .docx file; and c).png image file
# a html file
oat_table_merged %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/oat demographics_cOR_aOR.html")

# b) .docx file
oat_table_merged %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/oat demographics_cOR_aOR.docx")

# c .pnh image fi
oat_table_merged %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/oat demographics_cOR_aOR.png")



p1 <- plot(oat_mod_final, vline_colour = "red", errorbar_height = 0.3)



# Save
save(pts_oat_stratified.ft, pts_oatfull_stratified.ft, forestplot_no_ref, forestplot_no_ref, oat_table_merged, oat_mod_final.ft, table_merged, model.final.ft, stratified.model.ft,subset_cor.ft, tbl_character.ft, file = 
       here::here("data_processed", "prefer_tables_oat_working.rda")
)


