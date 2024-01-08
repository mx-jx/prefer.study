# Drug Market Tables
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
  tableone,
  summarytools
)

# load data
#
load(
  here::here("data_processed", "drug_market_working.rda"))

load(
  here::here("data_processed", "drug_market_factors.rda"))

# Create a theme for figures, tables and all processed text

apa_theme <- function (ft)  {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 10, part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::align(align = "left", part = "header") %>%
    #flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = officer::fp_border(width = 1.5), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "all") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "header") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 1.5), part = "footer") %>%
    flextable::hline_bottom(border = officer::fp_border(width = 2), part = "all") %>%
    flextable::autofit()
}

# Set the theme to compact [reduce spacing around text and numbers]

## Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())


print(summarytools::dfSummary(sa_drugmarket, method="rmarkdown", style="multiline"))

summarytools::ctable(sa_drugmarket, style = "rmakrdown")

ggplot(sa_drugmarket) +
  geom_bar(aes(x = stock_heroin_c19), position = "dodge") +
  xlab("Device") + ylab("Count")


summarytools::ctable(sa_drugmarket$sell_sellers, sa_drugmarket$sell_users, style = "rmarkdown", totals = FALSE, headings = FALSE, prop = "n",
                     OR = TRUE, RR = TRUE)

drugmarket.xlsx <- writexl::write_xlsx(sa_drug_market, "sa_drug_market.xlsx")

library(readxl)
drug_market <- read_excel("drug_market.xlsx")
ff_glimpse(drug_market)

mean(drug_market$x9_years_working, na.rm=T)
str(drug_market)
writexl::write_xlsx(drug_market, "drug_market.xlsx")




drug_market <- drug_market %>%
  mutate(yrs_work = as.numeric(x9_years_working))

summary(drug_market$yrs_work, na.rm=TRUE)

summarytools::dfSummary(drug_market)
