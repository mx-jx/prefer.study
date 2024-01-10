

install.packages("GGally")

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
  tableone,
  GGally
)

## Load the files

load(
  here::here("data_processed", "prefer_subset_working.rda"))

load(
  here::here("data_processed", "prefer_working.rda"))

# Selecting the variables

p.sub <- p %>%
  dplyr::select(age, age.group, edu.factor ,genderall.factor, gender.mf,
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

# Create a subset of only participants who prefer methadone or buprenorphine.Call this variable "prefer.subset"  

p.sub <- p.sub %>%
  filter(prefer.factor == "Buprenorphine" | prefer.factor == "Methadone")



p.sub <- p.sub %>%
  mutate(prescribedmethadone.month.f = fct_relevel("No", "Yes"))
# Create a codebook for this dataset. I have created two coodebooks for the dataset. They include"

# 1) A simple codebook with the variables and their labels

codebook <- map_df(p.sub, function(x) attributes(x)$label) %>% 
  gather(key = Code, value = Label)

## View the codebook

codebook 

# 2) A more detailed codebook. Using the 'makeCodebook' function from the "dataMaid" package an RMarkdown file is generated that includes summary statistics of each variable in the dataset/Codebook 

library(dataMaid)

makeCodebook(p.log, replace=TRUE)

# Logistic Regression Modelling

# Modelling strategy for binary outcomes

# A statistical model is a tool to understand the world. The better your model describes your data, the more useful it will be. Fitting a successful statistical model requires decisions around which variables to include in the model. Our advice regarding variable selection follows the same lines as in the linear regression chapter.

# 1. As few explanatory variables should be used as possible (parsimony);
# 2. Explanatory variables associated with the outcome variable in previous studies should be accounted for;
# 3. Demographic variables should be included in model exploration;
# 4. Population stratification should be incorporated if available;
# 5. Interactions should be checked and included if influential;
# 6. Final model selection should be performed using a “criterion-based approach”
# 7. minimise the Akaike information criterion (AIC)
# 8. maximise the c-statistic (area under the receiver operator curve).
# 9. We will use these principles through the next section.

# UNADJUSTED LOGISTIC REGRESSION ------------------------------------------

# PREFERENCE FOR METHADONE LOGISTIC REGRESSION ------------------------------------

# Checking all and the variables with p value >0.2 will go into the final model
# Method of model building usually doesn't matter with a large sample
# For this analysis the sample size was ~100,000 individuals
# For smaller samples that are more sensitive to slight variations it may make a difference.
# In this case backwards step-wise regression gives a bit more control to the model builder


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

# Reorder the level of any variables that need to be reordered

# 
p.sub <- p.sub %>%
  mutate(prefer.factor = fct_relevel("Buprenorphine", "Methadone"))

# Chronic pain
p.sub <- p.sub %>%
  mutate(
    chronicpain.factor.yn = factor(chronicpain.factor.yn) %>%
      ff_label("Chronic pain") %>%
      fct_relevel("No",
                  "Yes"))
# Ever received methadone
p.sub <- p.sub %>%
  mutate(
    methadoneever.yn = factor(methadoneever.yn) %>%
      ff_label("Ever received methadone") %>%
      fct_relevel("No",
                  "Yes"))

# Chronic pain
p.sub <- p.sub %>%
  mutate(
    gender.mf = factor(gender.mf) %>%
      ff_label("Gender") %>%
      fct_relevel("Female",
                  "Male"))

# Other opioid use
p.sub <- p %>%
  mutate(
    otheropi.month.factor = factor(otheropi1month) %>%
      ff_label("Non-prescribed pharmaceutical use") %>%
      fct_relevel("No",
                  "Yes"))
# Homelessness
p.sub  <- p.sub  %>%
  mutate(homeless.factor.yn  = fct_relevel(homeless.factor.yn, "No", "Yes"))

# AGe group

p.sub <- p.sub %>% 
  mutate(age_group = case_when(
    # criteria            # new value if TRUE
    age < 36              ~ "18 - 35 years",
    age >= 36 & age < 46  ~ "36 - 45 years",
    age >= 46   ~ ">45 years",
    ))


p.sub  <- p.sub  %>%
  mutate(age_group  = fct_relevel(age_group, "18 - 35 years",
                                  "36 - 45 years",
                                  ">45 years", 
                                  ">55 years"
                                  ))

# UNADJUSTED LOGISTIC REGRESSION ------------------------------------------
# 
subset_cor <-
  tables %>%
  dplyr::select(age, gender.mf,edu.factor, income.factor.yn, homeless.factor.yn,  
                prison.factor, chronicpain.factor.yn, heroin.month.factor, 
                nonmethadone.month.factor,
                nonbupe.month.factor, otheropi.month.factor, 
                cocaine.month.factor, meth.month.factor, benzo.month.factor, oat.yn.f, methadoneever.yn, bupeever.factor, prefer.factor) %>%
  tbl_uvregression(
    method = glm,
    y = prefer.factor, 
    label = list(age ~ "Age",
    #              # age.group ~ "Age group"
    #              
    #              # edu.yr10 ~ "Formal education",
                  gender.mf ~ "Gender", 
                  methadoneever.yn ~ "Ever received OAT with methadone", 
                  bupeever.factor ~ "Ever received OAT with buprenorphine", 
    benzo.month.factor ~ "Benzodiazepine use"),
    #              prison.factor ~ "Ever incarcerated", 
    #              homeless.factor.yn ~ "Currently in stable housing",
    #              income.factor.yn   ~ "Currently employed",
    #              chronicpain.factor.yn  ~ "Chronic pain",
    #              heroin.month.factor ~ "Heroin use",
    #              nonmethadone.month.factor ~ "Non-prescribed methadone use", 
    #              nonbupe.month.factor ~ "Non-prescribed suboxone use",
    #              otheropi.month.factor ~ "Non-prescribed pharmaceutical opioid use", 
    #              cocaine.month.factor ~ "Cocaine use", 
    #              meth.month.factor ~ "Methamphetamine use", 
    #              benzo.month.factor ~ "Benzodiazepine use",
    #              oat.yn.f  ~ "Currently receiving OAT"),
    method.args = list(family = binomial),
    exponentiate = TRUE,
    hide_n = TRUE,
    pvalue_fun = function(x) style_pvalue(x, digits = 3)
  ) %>%
  modify_header(
    label = '**Variable**',
    estimate = '**cOR**',
    ci = '**95% CI**',
    p.value = '**p-value**'
  ) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE) %>%
  modify_table_styling(
    column = estimate,
    rows = !is.na(estimate),
    cols_merge_pattern = "{estimate} ({conf.low} - {conf.high})"
  ) %>%
  modify_header(estimate ~ "**cOR (95% CI)**") %>%
  modify_header(label ~ "**Enrolled (n = 400)**") %>%
  modify_column_hide(c(ci)) %>% 
  # bold_p()%>% # bold p-values under a given threshold (default 0.05)
  #bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

# Convert to flextable [the suffix "ft" = "flextable"]

subset_cor.ft <- subset_cor %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

subset_cor.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
subset_cor.ft%>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/subset odds ratios.html")

# b)
subset_cor.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/subset odds ratios.docx")


# ADJUSTED LOGISTIC REGRESSION ------------------------------------------

# PREFERENCE FOR METHADONE LOGISTIC REGRESSION ------------------------------------

# Activate the package
library("report")
library("performance")

# Remove scientific notation 

options(scipen = 999)

# Adjusted model

# Using backwards stepwise selection [which uses AIC values to determine] to build the final adjusted model that describes the associations between a preference for methadone [dependent variable] and several factors [explanatory variables]

# Start with all non-colinear variables in the model 

subset_mod_all <- glm(prefer.factor ~ age + gender.mf + edu.factor+ income.factor.yn + homeless.factor.yn  + prison.factor + chronicpain.factor.yn + heroin.month.factor + nonmethadone.month.factor + nonbupe.month.factor + cocaine.month.factor + benzo.month.factor + otheropi.month.factor + oat.yn.f + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Check the VIF

subset_mod_all_vif <- car::vif(glm(prefer.factor ~ age + gender.mf + edu.factor+ income.factor.yn + homeless.factor.yn  + prison.factor + chronicpain.factor.yn + heroin.month.factor + nonmethadone.month.factor + nonbupe.month.factor + cocaine.month.factor + benzo.month.factor + otheropi.month.factor + oat.yn.f + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial))

# Display Variance inflation factor

subset_mod_all_vif

# Backwards stepwisif the full model= e selection

library(MASS)

subset_mod_final_aic = step(subset_mod_all, direction = "backward")

# Generate and run the final model 


subset_mod_final <- glm(prefer.factor ~ age + gender.mf  + heroin.month.factor + nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# subset_mod_final <- glm(prefer.factor ~ age.group + gender.mf + heroin.month.factor, nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Check the VIF

subset_mod_final_vif <- car::vif(glm(prefer.factor ~ age + gender.mf + nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial))

# Insepct the final model and generate a report of the model
report(subset_mod_final)

# Inspecting the model performace

model_performance(subset_mod_final)


ggcoef_model(subset_mod_final)+
  xlab("Coefficients") 

models <- list("Adjusted model" = subset_mod_final, "Unadjusted model" = subset_mod_all)

ggcoef_compare(models, type = "faceted")

# Comparing models with the Likelihood ratio test
# Model 1[with heroin use] versus Model 2 [wihtout heroin use]

library(lmtest)

# Model 1 - final model without past month heroin use
model_heroin <- glm(prefer.factor ~ age + gender.mf  + heroin.month.factor + nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Model 2 - final model with heroin use included

model_no_heroin <- glm(prefer.factor ~ age + gender.mf + nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Testing the difference between the two models Likeliehood ratio test
lrtest(model_heroin, model_no_heroin)


# Using the 'easystats' packages

easystats::install_latest()
library(easystats)

test_likelihoodratio(model_heroin, model_no_heroin, estimator = "ML")


# Comparing models with the Likelihood ratio test
# Model 1[with nonprescribed methadone use] versus Model 2 [wihtout nonprescribed methadone use]

library(lmtest)

# Model 1 - final model without past month heroin use
model_nonmethadone <- glm(prefer.factor ~ age + gender.mf  + heroin.month.factor + nonmethadone.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Model 2 - final model with heroin use included

model_no_nonmethadone <- glm(prefer.factor ~ age + gender.mf + heroin.month.factor + nonbupe.month.factor + otheropi.month.factor + methadoneever.yn + bupeever.factor, data = p.sub, family = binomial)

# Testing the difference between the two models Likeliehood ratio test
lrtest(model_nonmethadone, model_no_nonmethadone)


# Using the 'easystats' packages

easystats::install_latest()
library(easystats)

test_likelihoodratio(model_nonmethadone, model_no_nonmethadone, estimator = "ML")


ggcoef_model(mod_labelled)

















