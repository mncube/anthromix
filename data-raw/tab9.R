## code to prepare `tab9` dataset goes here
require(dplyr)
require(stringr)
require(readr)
require(tidyr)
tab9 <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                           sheet = "All site Peasants-Time Comparis") %>%
  dplyr::select(Site = Cemetary, UniqueID = `Unique ID`, d15N = δ15N, d13C = δ13C,
                Period = `Arm Pos/Period`, Stat) %>%
  dplyr::mutate(dplyr::across(starts_with("d1"),
                              ~stringr::str_replace(.,"−", "-"))) %>%
  dplyr::mutate(d15N = readr::parse_number(d15N),
                d13C = readr::parse_number(d13C)) %>%
  tidyr::drop_na(d15N, d13C) %>%
  dplyr::mutate(Period = dplyr::case_when(Period == "a" ~ "Early",
                                          Period == "b" ~ "Middle",
                                          Period == "c" ~ "Late",
                                          Period == "d" ~ "Late",
                                          TRUE ~ Period)) %>%
  dplyr::mutate(Period = factor(Period, levels = c("Early", "Middle", "Late"),
                                labels = c("Early", "Middle", "Late"))) %>%
  dplyr::mutate(Stat = dplyr::case_when(Stat == 0 ~ "Peasant",
                                        Stat == 1 ~ "Elite",
                                        Stat == 2 ~ "Monk",
                                        TRUE ~ NA)) %>%
  dplyr::mutate(Stat = factor(Stat, levels = c("Peasant", "Elite", "Monk"),
                              labels = c("Peasant", "Elite", "Monk"))) %>%
  dplyr::mutate(Site = dplyr::case_when(Site == "OmKloster" ~ "Øm Kloster",
                                        Site == "StMikkel" ~ "St. Mikkel",
                                        TRUE ~ Site)) %>%
  dplyr::mutate(`Stat Period` = ifelse(is.na(Stat) | is.na(Period), NA, paste(Stat, Period))) %>%
  dplyr::mutate(`Stat Period` = factor(`Stat Period`,
                                       levels = c("Peasant Early", "Peasant Middle", "Peasant Late", "Peasant Late",
                                                  "Elite Early", "Elite Middle", "Elite Late", "Elite Late",
                                                  "Monk Early", "Monk Middle", "Monk Late", "Monk Late"),
                                       labels = c("Peasant Early", "Peasant Middle", "Peasant Late", "Peasant Late",
                                                  "Elite Early", "Elite Middle", "Elite Late", "Elite Late",
                                                  "Monk Early", "Monk Middle", "Monk Late", "Monk Late"))) %>%
  dplyr::mutate(Site = factor(Site))


usethis::use_data(tab9, overwrite = TRUE)
