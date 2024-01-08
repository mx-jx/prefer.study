# South African Drug Market Study
#
## Data pull

# Get data

 # install.packages("readstata13")

library(here)
library(readr)
library(readstata13)



 # drugs <- readr::read_csv(
 #  here::here("raw_data", "drugseller.csv"))

 drugs <- readr::read_delim(
  here::here("raw_data", "drugseller.csv"), delim=";")


drugseller <- read_delim("raw_data/drugseller.csv", delim=";")

spec(drugs)

glimpse(drugseller)

# Save
save(drugs, drugseller, file =
       here::here("data_processed", "drug_market_data.rda")
)


# Other basic reccoding or renaming functions here
