## code to prepare `tab2` dataset goes here
require(dplyr)
require(stringr)
require(readr)
require(tidyr)
tab2 <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                              sheet = "OM Full Comparison") %>%
  dplyr::select(Cemetary, UniqueID = `Unique ID`, d15N = δ15N, d13C = δ13C,
                Arm = `Arm Pos/Period`, Stat) %>%
  dplyr::mutate(dplyr::across(starts_with("d1"),
                              ~stringr::str_replace(.,"−", "-"))) %>%
  dplyr::mutate(d15N = readr::parse_number(d15N),
                d13C = readr::parse_number(d13C)) %>%
  tidyr::drop_na(d15N, d13C)

usethis::use_data(tab2, overwrite = TRUE)
