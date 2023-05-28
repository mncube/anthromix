## code to prepare `discrimination` dataset goes here
require(dplyr)
require(stringr)
require(readr)
discrimination <- readr::read_csv("./rawdatafiles/discrimination_main.csv")

usethis::use_data(discrimination, overwrite = TRUE)
