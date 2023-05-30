## code to prepare `tab2` dataset goes here
require(dplyr)
require(stringr)
require(readr)
require(tidyr)
tab2 <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                              sheet = "OM Full Comparison") %>%
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
                                   TRUE ~ Period)) %>%
  dplyr::mutate(Period = factor(Period, levels = c("A", "B", "C", "D"),
                                   labels = c("A", "B", "C", "D"))) %>%
  dplyr::mutate(Stat = dplyr::case_when(Stat == 0 ~ "Peasant",
                                        Stat == 1 ~ "Elite",
                                        Stat == 2 ~ "Monk",
                                        TRUE ~ NA)) %>%
  dplyr::mutate(Stat = factor(Stat, levels = c("Peasant", "Elite", "Monk"),
                                 labels = c("Peasant", "Elite", "Monk"))) %>%
  dplyr::mutate(Site = dplyr::case_when(Site == "OmKloster" ~ "OM Kloster",
                                        Site == "StMikkel" ~ "St. Mikkel",
                                        TRUE ~ Site)) %>%
  dplyr::mutate(`Stat Period` = ifelse(is.na(Stat) | is.na(Period), NA, paste(Stat, Period))) %>%
  dplyr::mutate(`Stat Period` = factor(`Stat Period`,
                                       levels = c("Peasant A", "Peasant B", "Peasant C", "Peasant D",
                                                                 "Elite A", "Elite B", "Elite C", "Elite D",
                                                                 "Monk A", "Monk B", "Monk C", "Monk D"),
                                       labels = c("Peasant A", "Peasant B", "Peasant C", "Peasant D",
                                                  "Elite A", "Elite B", "Elite C", "Elite D",
                                                  "Monk A", "Monk B", "Monk C", "Monk D")))

usethis::use_data(tab2, overwrite = TRUE)
