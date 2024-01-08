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
  rio,             # importing data
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
  codebookr,
  codebook,
  sjlabelled,
  likert,
  kableExtra,
  haven,
  tableone
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


# Table 1
# Participant characteristics [subset]

# Generate a table describing the characteristics of the participants


tbl_character <-
  drug_market %>%
  tbl_summary(include = c(city.factor, gender.factor, primary_drug,
                          secondary_drug, sell_heroin, sell_crack_coke, sell_powder_coke, sell_cocaine,
                          sell_meth, stock_heroin_c19,stock_cocaine_c19,stock_meth_c19,sales_heroin_c19, sales_coke_c19, sales_meth_c19, sell_sellers, sell_users, yrs_working, link.yn),
  # type = all_categorical() ~ "categorical",
  # label =
  #   list(age ~ "Age",
  #        agegroup ~ "Age group",
  #        # gender.mf ~ "Gender",
  #        methadoneever.yn ~ "Ever received methadone",
  #        homeless.factor ~ "Currently homeless"),
  missing = "no",
  statistic = all_continuous() ~ ("{median} ({p25} - {p75})"),
  digits = list(all_categorical() ~ c(0)))%>%
                # injectingbenzo.month.f ~ c(1))) %>%
  add_stat_label()%>%
  modify_footnote(update = everything() ~ NA, abbreviation = FALSE) %>%
  modify_header(
    stat_0 = "**Enrolled** \n **(n = {N})**",
  )%>%
  modify_caption("Table 1. Participant Characteristics")


# Convert to flextable [the suffix "ft" = "flextable"]

tbl_character.ft <- tbl_character %>%
  as_flex_table()%>%
  apa_theme()

# Display the table

tbl_character.ft

# Save the table as an a) .docx file; and b) .html file

# a)
tbl_character.ft %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics.html")

# b)
tbl_character.ft %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/participant characteristics.docx")


# Table stratified

tables <- tables %>%
  mutate(prefer.factor = fct_relevel(prefer.factor, "Methadone", "Buprenorphine"))

tables <- tables %>%
  mutate(homeless.factor.yn  = fct_relevel(homeless.factor.yn, "No", "Yes"))

stratified.model <-
  tables %>%
  tbl_summary(by = prefer.factor,
              include = c(age, age_group,
                          gender.mf, edu.factor,
                          income.factor.yn, homeless.factor.yn,
                          prison.factor, chronicpain.factor.yn,
                          heroin.month.factor,
                          nonmethadone.month.factor,
                          nonbupe.month.factor,
                          otheropi.month.factor,
                          cocaine.month.factor, meth.month.factor,
                          benzo.month.factor,
                          oat.yn.f, methadoneever.yn, bupeever.factor
              ),
              label =
                list(age ~ "Age",
                     age_group ~ "Age group",
                     gender.mf ~ "Gender",
                     methadoneever.yn ~ "Ever received OAT with methadone",
                     bupeever.factor ~ "Ever received OAT with buprenorphine",
                     benzo.month.factor ~ "Benzodiazepine use"),
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
  bold_labels()
# modify_caption("Table 2. Participant characteristics stratified by preference for methadone or buprenorphine.")


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


# Final logistic regression table

model.final <- tbl_regression(subset_mod_final, exponentiate = TRUE,
                              label =
                                list(age ~ "Age",
                                     gender.mf ~ "Gender",
                                     methadoneever.yn ~ "Ever received OAT with methadone",
                                     bupeever.factor ~ "Ever received OAT with buprenorphine"),
                              add_estimate_to_reference_rows = FALSE,
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
  # bold_labels() %>%
  modify_footnote(everything() ~ NA, abbreviation = TRUE)

# add_significance_stars(
#   hide_p = FALSE,
#   hide_se = TRUE,
#   pattern = "{p.value}{stars}")



# Convert to flex table

model.final.ft <- model.final %>%
  as_flex_table()%>%
  apa_theme()

model.final.ft

# Merged tables

table_merged <-
  tbl_merge(
    tbls = list(stratified.model, subset_cor, model.final),
    tab_spanner = c("** **", "**Unadjusted model**", "**Adjusted model**")) %>%
  modify_header(label = "**Variable**") %>% # update the column header
  modify_footnote(everything() ~ NA)%>%
  as_flex_table()%>%
  apa_theme()


# a)
table_merged %>%
  flextable::save_as_html(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/demographics_cOR_aOR.html")

# b)
table_merged %>%
  flextable::save_as_docx(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/demographics_cOR_aOR.docx")

# c
table_merged %>%
  flextable::save_as_image(path = "C:/Users/mjstowe/OneDrive - UNSW/Documents/GitHub/projects/prefer_project/tables/demographics_cOR_aOR.png")




# ##################################################################################
# FOREST PLOT

# With reference rows

p1 <- plot(oat_mod_final, vline_colour = "red", errorbar_height = 0.3, xlab(" "))
p2 <- plot(model.final, vline_colour = "red", errorbar_height = 0.3, remove_reference_rows = FALSE)


forestplot <- cowplot::plot_grid(p2, p1, nrow=2, align="v", labels=c("A", "B"), label_fontface = "bold", label_size = 17)

# Without reference rows

p3 <- plot(oat_mod_final, vline_colour = "red", errorbar_height = 0.3, remove_reference_rows = TRUE)
p4 <- plot(model.final, vline_colour = "red", errorbar_height = 0.3, remove_reference_rows = TRUE)


forestplot_no_ref <- cowplot::plot_grid(p4, p3, nrow=2, align="v", labels=c("A", "B"), label_fontface = "bold", label_size = 17)


# Save
