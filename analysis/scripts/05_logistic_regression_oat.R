##################################################################################

## Load the files

load(
  here::here("data_processed", "prefer_tables_working.rda"))

load(
  here::here("data_processed", "prefer_logistic_working.rda"))

load(
  here::here("data_processed", "prefer_subset_working.rda"))

load(
  here::here("data_processed", "prefer_working.rda"))

# Selecting variables that relate to OAT among participants who are currently receiving OAT

# With OAT related variables

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
                paytx.factor.month,doselocation.factor, dose.km.factor, dosetime.factor, oatpref.all, oat.yn.f, kmdose, clinic.attend, clinic.attend.factor, dosinglocation.factor) 


# Collapse the timedose.factor variable from 3 levels into 2 levels; 1) < 30 minutes; 2) >= 30 minutes

 dosetime.factor <- fct_collapse(oat$timedose.factor, Thirty = c("< 15 minutes", "15 - < 30 minutes"))

 oat <- oat %>%
   mutate(dosetime.factor = factor(dosetime.factor))
 
 oat <- p %>%
 mutate(clinic.attend.factor = factor(clinicattendance)%>%
   fct_recode(
     "Daily or several times per week" = "1",
     "Weekly or less frequently" = "2")%>%
   ff_label("OAT collection frequency")%>%
   fct_relevel(
     "Daily or several times per week",
     "Weekly or less frequently"))
 

# Participants currently receiving OAT

# UNADJUSTED LOGISTIC REGRESSION ------------------------------------------
# 
oat <- oat %>%
  mutate(prefer.factor = fct_relevel(prefer.factor, "Buprenorphine", "Methadone"))
oat_cor <-
  oat %>%
  filter(prefer.factor == "Methadone" | prefer.factor == "Buprenorphine")%>%
  dplyr::select(age, gender.mf, nonmethadone.month.factor, 
                nonbupe.month.factor,heroin.month.factor,
                methadoneever.yn, bupeever.factor,
                usedrugs.oat, paytx.yn.f,
                clinic.attend.factor, kmdose,
                dosetime.factor, prefer.factor) %>%
  tbl_uvregression(
    method = glm,
    y = prefer.factor, 
    label = list(age ~ "Age",
                 usedrugs.oat ~ "Drugs used while on OAT",
                 methadoneever.yn ~ "Ever received methadone",
                 gender.mf ~ "Gender", 
                 clinic.attend.factor ~ "Dose collection frequency", 
                 kmdose ~ "Travel distance to OAT collection site"),
    
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
  modify_header(label ~ '**Variable**') %>%
  modify_column_hide(c(ci, p.value)) %>%
  bold_p()%>% # bold p-values under a given threshold (default 0.05)
  #bold_p(t = 0.10, q = TRUE) %>% # now bold q-values under the threshold of 0.10
  bold_labels()

# Convert to flextable [the suffix "ft" = "flextable"]

oat_cor.ft <- oat_cor %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

oat_cor.ft

# Save the table as an a) .docx file; and b) .html file 

# a)
oat_cor.ft%>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/oat_cOR.html")

# b)
oat_cor.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/oat_cOR.docx")

# Forest plot of cOR

oat_cor%>%
  plot

# ###############################################



oat <- oat %>%
  mutate(prefer.factor = fct_relevel(prefer.factor, "Buprenorphine", "Methadone")) %>%
  mutate(fct_drop(prefer.factor))


oat <- oat %>%
  mutate(nonmethadone.month.factor = fct_relevel(nonmethadone.month.factor, "No", "Yes")) %>%
  mutate(fct_drop(nonmethadone.month.factor))


oat <- oat %>%
  mutate(gender.mf = fct_relevel(gender.mf, "Female", "Male"))

model_oat_vif <- car::vif(glm(prefer.factor ~ age + gender.mf + nonmethadone.month.factor + nonbupe.month.factor + methadoneever.yn + bupeever.factor +
                                usedrugs.oat + paytx.yn.f + kmdose + clinic.attend +
                                dosetime.factor + otheropi.month.factor, 
                              data=oat, family=binomial))

model_oat_fin <- glm(prefer.factor ~ age + gender.mf + nonmethadone.month.factor + nonbupe.month.factor + methadoneever.yn + bupeever.factor +
                                usedrugs.oat + paytx.yn.f + kmdose + clinic.attend +
                                dosetime.factor 
                              ,data=oat, family=binomial)

vif <- model_oat_vif %>%
  as_tibble()


oat <- oat %>%
  mutate(gender.mf = fct_relevel(gender.mf, "Male", "Female"))

model_oat <- glm(prefer.factor ~ age + gender.mf + heroin.month.factor + nonmethadone.month.factor + 
                   nonbupe.month.factor + methadoneever.yn + bupeever.factor +
                   usedrugs.oat +  paytx.yn.f + 
                   kmdose + dosetime.factor + clinic.attend + otheropi.month.factor
                 , data=oat, family=binomial)

# doselocation and paytx.yn = high VIF

oat_mod_final_aic = step(model_oat_fin, direction = "backward")

library(MASS)
oat_mod_final_aic = stepAIC(model_oat_fin, direction = "backward")

library(MASS)
library(leaps)
all<-regsubsets(prefer.factor ~ age + gender.mf + heroin.month.factor + nonmethadone.month.factor + 
                  nonbupe.month.factor + methadoneever.yn + bupeever.factor +
                  usedrugs.oat +  paytx.yn.f + 
                  kmdose + dosetime.factor + clinic.attend + 
                  otheropi.month.factor
                ,data=oat,nbest=1, nvmax=8)
info <- summary(all)
cbind(info$which, round(cbind(rsq=info$rsq,adjr2=info$adjr2, cp=info$cp, bic=info$bic, rss=info$rss), 3))









oat <- oat %>%
  mutate(gender.mf = fct_relevel(gender.mf, "Male", "Female"))

oat <- oat %>%
  drop_na(nonmethadone.month.factor)

oat_model_final <- glm(prefer.factor ~ age + gender.mf +  nonmethadone.month.factor + methadoneever.yn + bupeever.factor + paytx.yn.f + 
                         kmdose + clinic.attend
                       , data=oat, family=binomial)


# Insepct the final model and generate a report of the model
report(oat_model_final)

# Inspecting the model performace

model_performance(oat_model_final)

# Look into creating a new time variable that adds the hours and minutes together into a new column


p$timetx_hrs
p$timetx_mins






# Save
save(oat_cor, subset_mod_final, subset_mod_all, subset_cor, subset_mod_final, oat_model_final, model_oat, oat_cor.ft, p.sub, subset_mod_final_vif,
     subset_mod_all_vif, subset_cor.ft,  file = 
       here::here("data_processed", "prefer_logistic_working.rda")
)




# # If you don't have the "leaps" package installed yet, uncomment and run the line below
install.packages("leaps")
library(leaps)

subset_mod_final_aic

# Selecting the variables

p.glm <- p %>%
  dplyr::select(age, age.group, edu.factor ,genderall.factor, gender.mf,
                income.factor.yn, homeless.factor.yn,         
                prison.factor, chronicpain.factor.yn, 
                heroin.month.factor,
                nonmethadone.month.factor, prefer.factor,
                nonbupe.month.factor,
                cocaine.month.factor, meth.month.factor, benzo.month.factor, otheropi.month.factor, 
                oat.yn.f, anyinject.yn.f,
                injfreq.factor, injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, methadoneever.yn, 
                bupeever.factor
  )


breg_full = regsubsets(prefer.factor ~ age + gender.mf + nonmethadone.month.factor + 
                         nonbupe.month.factor + methadoneever.yn + bupeever.factor +
                         usedrugs.oat + paytx.yn.f + paytx.factor.month +
                         doselocation.factor + timedose.factor 
                       ,data=oat)
breg_summary = summary(breg_full)
breg_summary


names(breg_summary)

(t(sprintf("%0.2f%%", breg_summary$rsq * 100)))

# Heroin

log.heroin <- glm(prefer.factor ~ heroin.month.factor , data = prefer.subset, family = binomial)
summary(exp(coef(log.heroin)))
summary(log.heroin)
print(exp(coef(log.heroin)))
print(exp(confint(log.heroin)))



tbl.pts.subset.ft 

data <- read_csv("raw_data/prefer_analysis19Aug2023")

data <- data %>% 
  janitor::clean_names()









library(GGally)

p.cor1 %>% cor_test(method = "pearson")

library(ggcorrplot)

model.matrix(~0+., data=opi) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=3)


opi %>% 
  cor(use="pairwise.complete.obs", method='spearman') %>% 
  ggcorrplot(show.diag = F, 
             type="lower", 
             lab=TRUE, 
             lab_size=2)

# Install and load the package 'ggcorrplot' 

install.packages("ggcorrplot")
library(ggcorrplot)

# Select the variables from the dataset. Correlations between these variabes wil be determined using the 'ggcorrplot' package

p.cor1 <-
  p.log %>%   
  dplyr::select(prescribedmethadone.month.f, injectingprescribedmethadone.month.f)


nonmethadone.month.f, prescribedsuboxone.month.f,
nonsuboxone.month.f, injectingnonmethadone.month.f, 
injectingprescribedmethadone.month.f,
injectingprescribedsuboxone.month.f, 
injectingnonsuboxone.month.f)



heroin.month.f, ,
injfreq.factor, injectingheroin.month.f,
, injectjngcocaine.month.f,
injectingmeth.month.f, injectingbenzo.month.f)

zap_labels(p.cor1)

p.cor1 <-
  p.log %>%   
  dplyr::select(prison.factor, chronicpain.factor.yn,
                heroin.month.f, prescribedmethadone.month.f, 
                nonmethadone.month.f, prescribedsuboxone.month.f,
                nonsuboxone.month.f,
                injectingheroin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f
  )




# Generate the correlation matrix table

model.matrix(~0+., data=p.cor1) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)



# Looking at the variance infaltion factors among the varables in the final model

```{r}

```

cor.sub1 <-
  p.log %>%   
  dplyr::select(age, age.group,heroin.month, 
                prescribedmethadone.month, nonmethadone.month,
                prescribedsuboxone.month, nonsuboxone.month, otheropi.month, 
                cocaine.month, meth.month, benzo.month, oat.yn) 


study_codebook <- codebook::codebook(p.sub)

library(officer)
library(codebookr)
study_codebook <- codebook(
  p.sub = p.sub,
  title = "My Example Study",
  subtitle = "A Subtitle for My Example Study Codebook",
  description = "Brief (or long) description of the data."
)


