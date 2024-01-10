# PREFER 
# Multivariable logistic regressions

# Load the various packagesaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

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
  here::here("data_processed", "prefer_tables_working.rda"))

load(
  here::here("data_processed", "prefer_logistic_working.rda"))

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
                oat.yn.f, anyinject.yn.f
                injfreq.factor, injectinghe,roin.month.f,
                injectingnonmethadone.month.f, 
                injectingprescribedmethadone.month.f,
                injectingprescribedsuboxone.month.f, 
                injectingnonsuboxone.month.f, injectjngcocaine.month.f,
                injectingmeth.month.f, injectingbenzo.month.f, methadoneever.yn, 
                bupeever.factor, oatpref.all
  )


# Forestplots 

# https://larmarange.github.io/ggstats/articles/ggcoef_model.html'

library(ggstats)

# Forestplot using the final model that included a subset of the variables

final_mod <- ggcoef_table(subset_mod_final, exponentiate = TRUE,
                          table_stat = c("ci", "p.value"),
                          ci_pattern = "{estimate} ({conf.low} - {conf.high})",
                          table_stat_label = list(
                            estimate = scales::label_number(accuracy = .1),
                            conf.low = scales::label_number(accuracy = .01),
                            conf.high = scales::label_number(accuracy = .01),
                            std.error = scales::label_number(accuracy = .001),
                            label = toupper
                          ),   
                          add_reference_rows = FALSE,
                          categorical_terms_pattern = "{level} (ref: {reference_level})",
                          table_header = c("aOR (95% CI)", "p-value"),
                          table_witdhs = c(0.7, 0.5),
                          vline_colour = "red", 
                          shape="red",
                          colour="grey29", 
                          point_size=0.5, 
                          errorbar_height=0.3,
                          variable_labels = c(
                            age = "Age",
                            gender.mf = "Gender",
                            heroin.month.factor = "Recent heroin use",
                            nonmethadone.month.factor = "Recent extramedical methadone use", 
                            methadoneever.yn = "Previous OAT with methadone",
                            bupeever.factor = "Previous OAT with buprenorphine"
                          ),
                          facet_labeller = ggplot2::label_wrap_gen(25)
)

# Forestplot using the final model that included a subset of the variables

oat_mod <- ggcoef_table(oat_model_final, exponentiate = TRUE,
                        table_stat = c("ci", "p.value"),
                        ci_pattern = "{estimate} ({conf.low} - {conf.high})",
                        table_stat_label = list(
                          estimate = scales::label_number(accuracy = .1),
                          conf.low = scales::label_number(accuracy = .01),
                          conf.high = scales::label_number(accuracy = .01),
                          std.error = scales::label_number(accuracy = .001),
                          label = toupper
                        ),
                        add_reference_rows = FALSE,
                        categorical_terms_pattern = "{level} (ref: {reference_level})",
                        table_header = c("aOR (95% CI)", "p-value"),
                        table_witdhs = c(0.7, 0.5),
                        vline_colour = "red", 
                        shape="red",
                        colour="grey29", 
                        point_size=0.5, 
                        errorbar_height=0.3, 
                        variable_labels = c(
                          age = "Age",
                          gender.mf = "Gender",
                          nonmethadone.month.factor = "Extramedical methadone use", 
                          methadoneever.yn = "Previous OAT with methadone",
                          paytx.yn.f = "Payment required for OAT", 
                          doselocation.factor = "OAT collection point", 
                          timedose.factor = "Travel time"),
  # cowplot::draw_figure_label(label = "Figure 1", size = 17)
                          # labs(tag = "A", title = "Figure 2. Forest plot displaying the adjusted odds ratios, 95% confidence interval and p-value 
                          #      from a multivariable logistic regression of preference for methadone against predictors among participants 
                          #      who are currently receiving opioid agonist therapy", caption =  "Abbreviations: OAT; opioid agonist therapy, aOR; adjusted odds ratio")
                          facet_labeller = ggplot2::label_wrap_gen(25))
                         
oat_mod 



library(patchwork)
final_mod/oat_mod 

design <- c(
  area(1, 1, 1),
  area(1, 1, 1))


# Combining the forestplots using cowplotr

par(
  mar = c(3, 3, 1, 1),
  mgp = c(2, 1, 0)
)
forestplots <- cowplot::plot_grid(final_mod, oat_mod, axis= "lr", align = "v", ncol=1,  labels=c("AUTO"), label_fontface = "bold", label_size = 17,
                                   byrow=FALSE, hjust = -1)
forestplots
