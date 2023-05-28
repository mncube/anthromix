## code to prepare `tab3` dataset goes here
require(dplyr)
require(stringr)
require(readr)
require(tidyr)
tab3 <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                           sheet = "OM Peasant-Time Comparison") %>%
  dplyr::select(Site = Cemetary, UniqueID = `Unique ID`, d15N = δ15N, d13C = δ13C,
                Period = `Arm Pos/Period`, Stat) %>%
  dplyr::mutate(dplyr::across(starts_with("d1"),
                              ~stringr::str_replace(.,"−", "-"))) %>%
  dplyr::mutate(d15N = readr::parse_number(d15N),
                d13C = readr::parse_number(d13C)) %>%
  tidyr::drop_na(d15N, d13C) %>%
  dplyr::mutate(Period = dplyr::case_when(Period == "a" ~ "A",
                                          Period == "b" ~ "B",
                                          Period == "c" ~ "C",
                                          Period == "d" ~ "D",
                                          TRUE ~ NA)) %>%
  dplyr::mutate(Period = factor(Period, levels = c("A", "B", "C", "D"),
                                labels = c("A", "B", "C", "D"))) %>%
  dplyr::mutate(Stat = dplyr::case_when(Stat == 0 ~ "Peasant",
                                        Stat == 1 ~ "Elite",
                                        Stat == 2 ~ "Monk",
                                        TRUE ~ NA)) %>%
  dplyr::mutate(Stat = factor(Stat, levels = c("Peasant", "Elite", "Monk"),
                              labels = c("Peasant", "Elite", "Monk")))

usethis::use_data(tab3, overwrite = TRUE)
