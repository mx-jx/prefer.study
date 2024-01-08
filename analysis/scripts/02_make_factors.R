# Making factors for the South African drug market dataset

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

# load data
#
load(
here::here("data_processed", "drug_market_working.rda"))

load(
here::here("data_processed", "drug_market_data.rda"))

# shape <- read_sf(dsn = "C:/Users/mjstowe/OneDrive - UNSW/Desktop/R", layer = "Primary_Health_Networks")
#
# ethos.loc <- read.csv("~/GitHub/ethos/raw_data/ethos_locations.csv")

# cleaning the data
#
drugs  <- drugs  %>%
  janitor::clean_names()

# Make factors

yrs_working <- as.numeric(drugs$x9_years_working)


drug_market <- drugs %>%

  mutate(
    # City/sampling site
    city.factor = factor(city) %>%
      fct_recode("Cape Town" = "cpt",
                 "Durban" = "dbn",
                 "Pitermaritzberg" = "pmb",
                 "Port Elizabeth" = "pe",
                 "Bloemfontein" = "blm",
                 "Pretoria" = "pta",
                 "Johannesburg" = "jhb",
                 "Mbombela" = "mbo",
                 "Polokwane" = "plk") %>%
      ff_label("City")%>%
      fct_relevel("Cape Town",
                  "Durban",
                  "Pretoria",
                  "Johannesburg",
                  "Port Elizabeth",
                  "Pitermaritzberg",
                  "Bloemfontein",
                  "Mbombela",
                  "Polokwane"),

    # Gender
    gender.factor = factor(gender) %>%
      fct_recode("Female"="2",
                 "Male"="1",
                 "Transgender"="3") %>%
      ff_label("Gender") %>%
      fct_relevel("Female",
                  "Male",
                  "Transgender"),

    # Sellers level code
    # sellers_code = factor(sellers_level_code) %>%
    #   fct_recode()

    # Primary drug sold
    primary_drug = factor(primary_drug_sold)%>%
      fct_recode("Heroin" = "1",
                 "Crack cocaine" = "2",
                 "Powder cocaine" = "3",
                 "Methamphetamine" = "4") %>%
      ff_label("Primary drug sold") %>%
      fct_relevel("Heroin",
                  "Crack cocaine",
                  "Powder cocaine",
                  "Methamphetamine"),

    # Secondary drug sold
    secondary_drug = factor(secondary_drug_sold)%>%
      fct_recode("Only sold 1 drug" = "0",
                 "Heroin" = "1",
                 "Crack cocaine" = "2",
                 "Powder cocaine" = "3",
                 "Methamphetamine" = "4") %>%
      ff_label("Primary drug sold") %>%
      fct_relevel("Only sold 1 drug",
                  "Heroin",
                  "Crack cocaine",
                  "Powder cocaine",
                  "Methamphetamine"),

    # Sell heroin
    sell_heroin = case_when(
      primary_drug_sold == 1 ~ "Yes",
      secondary_drug_sold == 1 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell heroin") %>%
    fct_relevel("No",
                "Yes"),

    # Sell crack cocaine
    sell_crack_coke = case_when(
      primary_drug_sold == 2 ~ "Yes",
      secondary_drug_sold == 2 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell crack cocaine") %>%
    fct_relevel("No",
                "Yes"),

    # Sell powder cocaine
    sell_powder_coke = case_when(
      primary_drug_sold == 3 ~ "Yes",
      secondary_drug_sold == 3 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell powder cocaine") %>%
    fct_relevel("No",
                "Yes"),

    # Sell cocaine
    sell_cocaine = case_when(
      primary_drug_sold == 3 ~ "Yes",
      secondary_drug_sold == 3 ~ "Yes",
      primary_drug_sold == 2 ~ "Yes",
      secondary_drug_sold == 2 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell cocaine") %>%
    fct_relevel("No",
                "Yes"),

    # Sell methamphetamine
    sell_meth = case_when(
      primary_drug_sold == 4 ~ "Yes",
      secondary_drug_sold == 4 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell methamphetamine") %>%
      fct_relevel("No",
                  "Yes"),

    # Sells multiple substances
    sell_polydrug = case_when(
      is.na(primary_drug_sold) == FALSE & secondary_drug_sold != 0 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell >1 drug type") %>%
      fct_relevel("No",
                  "Yes"),

    # Stock heroin COVID-19
      stock_heroin_c19 = factor(x3_stock_heroin_covid) %>%
      fct_recode("No change"="0",
                 "Increased"="1") %>%
      ff_label("Impact of COVID-19 on heroin stock")%>%
      fct_relevel("No change",
                  "Increased"),

    # Stock cocaine COVID-19
    stock_cocaine_c19 = factor(x3_stock_cocaine_covid) %>%
      fct_recode("No change"="0",
                 "Increased"="1",
                 "Decreased"="2") %>%
      ff_label("Impact of COVID-19 on cocaine stock")%>%
      fct_relevel("No change",
                  "Increased",
                  "Decreased"),

    # Stock methamphetamine COVID-19
    stock_meth_c19 = factor(x3_stock_meth_covid) %>%
      fct_recode("No change"="0",
                 "Increased"="1") %>%
      ff_label("Impact of COVID-19 on methamphetamine stock")%>%
      fct_relevel("No change",
                  "Increased"),

    # Sales heroin COVID-19
    sales_heroin_c19 = factor(x5_heroin_sales_covid) %>%
      fct_recode("No change"="0",
                 "Increased"="1",
                 "Decreased"="2")%>%
      ff_label("Impact of COVID-19 on heroin sales")%>%
      fct_relevel("Decreased",
                  "Increased",
                  "No change"),

    # Sales cocaine COVID-19
    sales_coke_c19 = factor(x5_cocaine_sales_covid) %>%
      fct_recode("No change"="0",
                 "Increased"="1",
                 "Decreased"="2")%>%
      ff_label("Impact of COVID-19 on cocaine sales")%>%
      fct_relevel("Decreased",
                  "Increased",
                  "No change"),

    # Sales methamphetamine COVID-19
    sales_meth_c19 = factor(x5_meth_sales) %>%
      fct_recode("No change"="0",
                 "Increased"="1",
                 "Decreased"="2")%>%
      ff_label("Impact of COVID-19 on methamphetamine sales")%>%
      fct_relevel("Decreased",
                  "Increased",
                  "No change"),
    # Sell to sellers
    sell_sellers = factor(x6_sell_to_sellers)%>%
      fct_recode("No"="0",
                 "Yes"="1")%>%
      ff_label("Sell to sellers") %>%
      fct_relevel("No",
                  "Yes"),
    # Sell to sellers only
    sell_seller_only = case_when(
      x6_sell_to_sellers == 1 & x7_sell_to_users == 0 ~ "Yes",
      is.na(TRUE)   ~ NA_character_,
      TRUE       ~ "No") %>%
      ff_label("Sell to sellers only") %>%
      fct_relevel("No",
                  "Yes"),
    # Grams of heroin sold to sellers per week
    heroin_sellers_g = as_numeric(x4_g_heroin_sold_weekly_gms)%>%
      ff_label("Heroin sold to sellers (g/wk)"),
    # Grams of cocaine sold to sellers per week
    coke_sellers_g = as_numeric(x4_g_cocaine_sold_weekly_gms)%>%
      ff_label("Cocaine sold to sellers (g/wk)"),
    # Grams of methamphetamine sold to sellers per week
    meth_sellers_g = as_numeric(x4_g_meth_sold_weekly_gms)%>%
      ff_label("Methamphetamine sold to sellers (g/wk)"),

    # No. of sellers heroin is sold to
    heroin_sellers = as_numeric(x6a_herion)%>%
      ff_label("No. of sellers heroin sold to"),

    # No. of sellers cocaine is sold to
    cocaine_sellers = as_numeric(x6a_cocaine)%>%
      ff_label("No. of sellers cocaine sold to"),

    # No. of sellers methamphetamine is sold to
    meth_sellers = as_numeric(x6a_meth)%>%
      ff_label("No. of sellers meth sold to"),

    # Grams of heroin sold to sellers per week
    heroin_seller_g = as_numeric(x6b_herion_g)%>%
      ff_label("Quantity of heroin sold to sellers (grams)"),

    # Grams of cocaine sold to sellers per week
    cocaine_seller_g = as_numeric(x6b_cocaine_g)%>%
      ff_label("Quantity of cocaine sold to sellers (grams)"),

    # Grams of methamphetamine is sold to
    meth_seller_g = as_numeric(x6b_meth_grams)%>%
      ff_label("Quantity of meth sold to sellers (grams)"),

    # Units of heroin sold to sellers per week
    heroin_seller_unit = as_numeric(x6b_herion_units)%>%
    ff_label("Quantity of heroin sold to sellers (units)"),

    # Units of cocaine sold to sellers per week
    cocaine_seller_unit = as_numeric(x6b_cocaine_units)%>%
    ff_label("Quantity of cocaine sold to sellers (units)"),

    # Units of methamphetamine sold to sellers per week
    meth_seller_unit = as_numeric(x6b_meth_units)%>%
      ff_label("Quantity of meth sold to sellers (units)"),

    # Sell to users
    sell_users = factor(x7_sell_to_users)%>%
      fct_recode("No"="0",
                 "Yes"="1")%>%
      ff_label("Sell to consumers") %>%
      fct_relevel("No",
                  "Yes"),

    # No. of consumers heroin is sold to
    heroin_users = as_numeric(x7a_herion)%>%
      ff_label("No. of consumers heroin sold to"),

    # No. of consumers cocaine is sold to
    cocaine_users = as_numeric(x7a_cocaine)%>%
      ff_label("No. of consumers cocaine sold to"),

    # No. of consumers methamphetamine is sold to
    meth_users = as_numeric(x7a_meth)%>%
      ff_label("No. of consumers meth sold to"),

    # Grams of heroin sold to consumers per week
    heroin_user_g = as_numeric(x7b_herion_g)%>%
      ff_label("Quantity of heroin sold to sellers (grams)"),

    # Grams of cocaine sold to consumers per week
    cocaine_user_g = as_numeric(x7b_cocaine_g)%>%
      ff_label("Quantity of cocaine sold to sellers (grams)"),

    # Grams of methamphetamine sold to consumers per week
    meth_user_g = as_numeric(x7b_meth_g)%>%
      ff_label("Quantity of meth sold to sellers (grams)"),

    # Units of heroin sold to consumers per week
    heroin_user_unit = as_numeric(x7b_herion_units)%>%
      ff_label("Quantity of heroin sold to sellers (grams)"),

    # Units of cocaine sold to consumers per week
    cocaine_user_unit = as_numeric(x7b_cocaine_units)%>%
      ff_label("Quantity of cocaine sold to sellers (grams)"),

    # Units of methamphetamine sold to consumers per week
    meth_user_unit = as_numeric(x7b_meth_units)%>%
      ff_label("Quantity of meth sold to sellers (grams)"),

    # Years working

    yrs_working = as_numeric(x9_years_working) %>%
       ff_label("Years working"),

    # No. of selllers on same level known by participant``

    seller_same_level = as_numeric(x10_number_sellers_same_level)%>%
      ff_label("No. of known sellers on same market level"),

    # Can connect interviewer with other sellers
    link.yn = factor(x11_can_link) %>%
      fct_recode("No"="0",
                 "Yes"="1")%>%
      ff_label("Able to connect to other sellers?") %>%
      fct_relevel("No",
                  "Yes"))


# Save
save(drug_market, file =
       here::here("data_processed", "drug_market_factors.rda")
)


ff_glimpse(drug_market)

dfSummary(drug_market)
ff_glimpse(drug_market)

glimpse(drug_market)




summary(drug_market$heroin_sellers_g
        , na.rm=TRUE)

summary(drug_market$x9_years_working
        , na.rm=TRUE)




drug_market <- as_numeric(drug_market$x9_years_working)

drug_market <- drug_market %>%
  rename(x9_years_working= (years_working))

summary(drug_market$meth_sellers_g
        , na.rm=TRUE)

summary(drug_market$meth_sellers
        , na.rm=TRUE)

summary(drug_market$coke_sellers_g
        , na.rm=TRUE)

summary(drug_market$cocaine_sellers
        , na.rm=TRUE)

tabyl(drug_market$sell_polydrug
        , na.rm=TRUE)

mean(drug_market$yrs_working, na.rm=TRUE)
