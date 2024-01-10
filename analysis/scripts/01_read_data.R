# PREFER project
## Data pull

# Get data

# install.packages("readr")

library(here)
library(readr)
library(readstata13)


ethos <- read_dta(
  here::here("raw_data", "enrollment survey POC merged_final.dta")
)

wave_2 <- read_dta(
  here::here("raw_data", "Wave II complete.dta")
)

wave_complete <- read_dta(
  here::here("raw_data", "W1 W2 complete.dta")
)

# ethos<- read_dta("enrollment survey POC merged_final.dta")

spec(ethos)


# Other basic reccoding or renaming functions here

# Save
save(ethos, wave_2, wave_complete, file = 
       here::here("data_processed", "ethos_working.rda")
)
