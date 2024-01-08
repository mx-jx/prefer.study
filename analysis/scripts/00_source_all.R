# South African Drug Market Study
#
## Source all

source( here::here("scripts", "01_read_data.R") )
source( here::here("scripts", "02_make_factors.R") )
source( here::here("scripts", "03_descriptive_analysis.R") )

# Save
save(prefer, file =
       here::here("data_processed", "prefer_final.rda")
)
