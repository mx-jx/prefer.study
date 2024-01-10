
mod2eg <- glm(prefer.mb ~ age.group + gender.mf + edu.yr10 + income.yn + accom.yn + prison.yn +chronicpain.yn + clinic.attend + heroinoat + opioidsoat + methoat + benzooat + injectingfreq.month + injectingheroin.month + injectingnonmethadone.month + injectingnonsuboxone.month + injectingnonopi.month + injectingcocaine.month + injectingmeth.month + methadoneever.yn + bupeever.yn+doselocation.group+paytx.yn+dose.km+missd.month, data=data.prefer, family = "binomial")


#OAT Preference methadone or buprenorphine

data.prefer. <- data.prefer. %>% 
  mutate(prefer.mb = case_when(
    q0ost == 1              ~ "Methadone",
    q0ost == 2 ~ "Buprenorphine",
    q0ost == 3 ~ "Buprenorphine",
    is.na(TRUE)  ~ NA_character_,
    TRUE                  ~ "NA"))

data.prefer.$prefer.mb

data.prefer.



#count unique values for each variable
sapply(lapply(data.prefer., unique), length)

#view in a table

model.aic.backward <- step(table_or.eg, direction = "backward", trace = 1)

# Create a csv file with the subsetted dataset 
write.csv(modelsubset,"data.prefer._9 Aug 2023.csv")

# Read in prefer_subset
prefer_subset<- read.csv("data.prefer._9 Aug 2023.csv", stringsAsFactors = TRUE)
glimpse(prefer_subset)




library(finalfit)
data.prefer.$prefer.mb <- as.factor(data.prefer.$prefer.mb)

explanatory = c("age.group", "edu.yr10", "gender.mf", "income.yn", "accom.yn", "prison.yn", "chronicpain.yn", "heroin.month", "prescribedmethadone.month", "prescribedsuboxone.month", "otheropi.month", "cocaine.month", "meth.month", "benzo.month", "anyinject.yn", "oat.yn", "clinic.attend", "heroinoat", "opioidsoat", "methoat", "missd.month", "dose.km", "paytx.month", "paytx.yn", "oatever.yn", "methadoneever.yn", "bupeever.yn", "doselocation.group")
dependent = "prefer.mb"
data.prefer. %>%
  finalfit(dependent, explanatory) -> t1


# + edulevel + gender_all +	prison_cat +
#   homeless + income_group + heroin1month + 	
#   methadone1month + bupe1month +	otheropi1month + 	
#   cocaine1month +	methamph1month +	benzos1month +	injany +
#   injecting_freq +	injheroinmonth +	injmethadonemonth +
#   injbupemonth + injothermonth +	injcocainemonth +
#   injmethamphmonth + injbenzosmonth +	oat_ever +
#   methadoneever + bupeever + currentoat_treat +
#   heroinwhileoat +  opioidswhileoat +
#   methamphwhileoat +	benzoswhileoat +	dosingpoint +
#   clinicattend + kmdosingsite + timetodose +
#   misseddosedays +	payoutofpocket + costtreat,

#OAT Preference methadone or buprenorphine 

prefer_subset <- prefer_subset %>% 
  mutate(prefer_methadonebupe = case_when(
    q0ost == 1              ~ "Methadone",
    q0ost == 2 ~ "Buprenorphine",
    q0ost == 3 ~ "Buprenorphine",
    is.na(TRUE)  ~ NA_character_,
    FALSE                  ~ "NA"))

tabyl(data.prefer.$prefer_methadonebupe, useNA = "always")
data.prefer. <- data.prefer. %>%
  mutate(oat_mb=(oat_mb))

prefer_subset <- filter(data.prefer., pref_oat == "Methadone" | pref_oat == "Buprenorphine")

data.prefer. <- filter(data.prefer., pref_oat == "Methadone" | pref_oat == "Buprenorphine") 

models <- data.prefer. %>% 
  filter(pref_oat == "Methadone" | pref_oat == "Buprenorphine")%>%
  mutate(pref_oat1 = fct_drop(pref_oat))

clear()
data.prefer. <- data.prefer. %>%
  mutate(pref_oat = fct_relevel(pref_oat,"Buprenorphine","Methadone"))

dosingpoint <- data.prefer. %>%
  mutate(dosingpoint = fct_relevel(dosingpoint, 
                                   "")
         
         fct(data.prefer.$payoutofpocket = character(), levels = NULL, na = character())
         
         data.prefer.. = data.prefer. %>% 
           filter(!is.na(costtreat), !is.na(payoutofpocket), !is.na(timetodose), !is.na(kmdosingsite)) %>% 
           mutate(Misseddose = recode_factor(missdose, 
                                             `0` = "None", 
                                             `1` = "1 - 5 doses",
                                             `2` = "> 5 doses"))
         
         
         data.prefer. = data.prefer. %>%        
           mutate(prefer.mb = recode_factor(prefer.mb, 
                                            "Methadone" = `0`, 
                                            "Buprenorphine" = `1`)        
                  
                  
                  
                  
                  
                  
                  
                  model_alldemo <- glm(prefer.mb ~ age.group + gender.mf + edu.yr10 + income.yn + accom.yn + prison.yn + heroin.month + nonmethadone.month  + meth.month + anyinject.yn +drugsoat + missd.month + dose.km + paytx.yn + doselocation.group, data=data.prefer, family = "binomial")
                  
                  
                  
                  
                  
                  
                  
                  
                  # benzos1month + methamph1month + cocaine1month + heroin1month +methadone1month +bupe1month + otheropi1month + income_group + edulevel + un_house+ prison_cat + gender_simpli + injany + methadoneever  + drugswhileoat + heroinwhileoat + opioidswhileoat + methamphwhileoat + benzoswhileoat + timetodose + misseddosedays + dosingpoint + kmdosingsite + clinicattend + costtreat + age_group, data = data.prefer., family = "binomial")
                  t
                  modelone <- glm(as.factor(prefer_methadonebupe) ~ age_group + gender_simpli + edulevel + prison_cat + homeless + income_group + chronicpain + heroin1month +methadone1month + bupe1month + otheropi1month + methamph1month + cocaine1month +  benzos1month + injany + methadoneever + bupeever + heroinwhileoat + opioidswhileoat + methamphwhileoat + benzoswhileoat + timetodose + misseddose + dosingpoint + kmdosingsite + clinicattend + costtreat, data = data.prefer., family = "binomial")
                  
                  
                  
                  modeleg <- glm(prefer.mb ~ age.group + gender.all + edu.yr10 + prison.yn + accom.yn + income.yn + chronicpain.yn + heroin.month + prescribedmethadone.month + nonsuboxone.month + otheropi.month + meth.month + cocaine.month +  benzo.month + anyinject.yn + oat.yn+ methadoneever.yn + bupeever.yn + drugsoat, data = data.prefer., family = "binomial")