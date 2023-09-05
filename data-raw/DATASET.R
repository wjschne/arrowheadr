## code to prepare `DATASET` dataset goes here
icons <- readr::read_csv("icons.csv")
usethis::use_data(icons, overwrite = TRUE, internal = TRUE)
