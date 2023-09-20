library(tidyverse)
library(acledR)

py_df <- read.csv("/Users/lfagli/Documents/Github/acledPy/data/acledpy_countries.csv")

new_df <- acledR::acled_countries %>%
    left_join(py_df, by = c("country"))

save(new_df, file = "acled_countries.rda")
