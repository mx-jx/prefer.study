
# PREFER project
## Source all

source( here::here("scripts", "01_read_data.R") )

source( here::here("scripts", "02_make_factors.R") )

load(
  here::here("data_processed", "prefer_working.rda"))

library(Hmisc)
library(tidyverse)
library(finalfit)


# Create theme for all 'gtsummary' objects [that is in line with most manuscript recommendations, e.g., Times New Roman font; justified alignment and compact spacing  ]

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

# Set the theme as a compact [spacing]
set_gtsummary_theme(theme_gtsummary_compact())

# And black and white

theme_set(theme_bw())


p %>% glimpse()

p %>% ff_glimpse()

load(
  here::here("data_processed", "prefer_working.rda"))


p <- p %>% 
  mutate(
    age.factor = 
      age %>%
      cut(4)
  )
p$age.factor %>%
  summary()

# Converting the "age" variable [numeric] into a factor wit bins at the factiles

p <- p %>% 
  mutate(
    age.factor = 
      age %>%
      Hmisc::cut2(g=4) # Note, cut2 comes from the Hmisc package
  )
# View the numnber of partcipants each category

p$age.factor %>% 
  summary()


p <- p %>% 
  mutate(
    age.factor = 
      age %>%
      cut(breaks = c(4,20,40,60,95), include.lowest = TRUE) %>% 
      fct_recode(
        "≤20"      =  "[4,20]",
        "21 to 40" = "(20,40]",
        "41 to 60" = "(40,60]",
        ">60"      = "(60,95]"
      ) %>% 
      ff_label("Age (years)")
  )
head(p$age.factor)




p1 <- p %>% 
  drop_na(prefer.factor) %>%
  ggplot(aes(x = prefer.factor, fill =  age.factor), na.rm=TRUE) + 
  geom_bar() + 
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

p2 <- p %>% 
  drop_na(prefer.factor) %>%
  ggplot(aes(x = prefer.factor, fill = age.factor), na.rm=TRUE) + 
  geom_bar(position = "fill") + 
  ylab("proportion")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

library(patchwork)
p1 + p2


p1 <- p %>% 
  drop_na(prefer.factor)%>%
  ggplot(aes(x = prefer.factor, fill=oatever.yn)) + 
  geom_bar(na.rm = TRUE, position = position_stack(reverse = TRUE)) +
  facet_grid(methadoneever.yn ~ bupeever.yn) + 
  theme(legend.position = "none")

p2 <- p %>% 
  drop_na(prefer.factor)%>%
  ggplot(aes(x = prefer.factor, fill=oatever.yn)) + 
  geom_bar(na.rm = TRUE, position = position_fill(reverse = TRUE)) +
  facet_grid(methadoneever.yn ~ bupeever.yn)+ 
  theme(legend.position = "bottom")

p1 / p2



library(finalfit)
dependent <- "prefer.factor"
explanatory <- c("age.factor", "oatever.yn")
p %>% 
  summary_factorlist(dependent, explanatory, p = TRUE,
                     add_dependent_label = TRUE)



p %$%        # note $ sign here
  table(age.factor, prefer.factor) %>% 
  chisq.test()

p %$%        # note $ sign here
  table(age.factor, prefer.factor) %>% 
  fisher.test()

p %>%
  count(oatever.yn, prefer.factor) %>%
  group_by(prefer.factor) %>%
  mutate(total = sum(n)) %>%
  mutate(percentage = round(100*n/total, 1)) %>% 
  mutate(count_perc = paste0(n, " (", percentage, ")")) %>% 
  select(-total, -n, -percentage) %>% 
  spread(prefer.factor, count_perc)

p %>% 
  summary_factorlist(dependent   = "prefer.factor", 
                     explanatory = "oatever.yn")

fit1 <- glm(prefer.factor ~ oat.yn, data = p, family = binomial)
summary(fit1)
coef(fit1) %>% exp()
confint(fit1) %>% exp()

library(broom)
fit1 %>% 
  tidy(conf.int = TRUE, exp = TRUE)
fit1 %>% 
  glance()


library(finalfit)
dependent <- "prefer.factor"
explanatory <- c("methadoneever.yn", "age", "bupeever.yn", "gender.mf")
fit2 = p %>% 
  finalfit(dependent, explanatory, metrics = TRUE)


p %>% 
  or_plot(dependent, explanatory, 
          breaks = c(0.5, 1, 5, 10, 20, 30),
          table_text_size = 3.5)



# Activate the package
library("report")


model <- glm(prefer.factor ~ methadoneever.yn + bupeever.yn +chronicpain.yn, data = p, family = "binomial")

# Print model parameters and additional information
report(model)


# Install the activate the package
install.packages("performance")
library("performance")

# Model performance summaries
model_performance(model)

# Check for heteroskedasticity
check_heteroscedasticity(model)

# Comprehensive visualization of model checks
check_model(model)



