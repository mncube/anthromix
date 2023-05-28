## code to prepare `refvals` dataset goes here
require(dplyr)
refvals <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                              sheet = "Reference Sources") %>%
  dplyr::rename(d15N = δ15N, d13C = δ13C) %>%
  dplyr::mutate(Group = ifelse(is.na(d15N) & is.na(d13C), Organism, NA)) %>%
  dplyr::filter(!is.na(Organism)) %>%
  dplyr::filter(Organism != "Organism") %>%
  tidyr::fill(Group, .direction = "down") %>%
  dplyr::filter(!(is.na(d15N) & is.na(d13C))) %>%
  dplyr::select(-`...5`, -`...6`, -`...7`, -`...8`) %>%
  dplyr::relocate(Group) %>%
  dplyr::mutate(dplyr::across(starts_with("d1"),
                              ~stringr::str_replace(.,"−", "-"))) %>%
  dplyr::mutate(d15N = readr::parse_number(d15N),
                d13C = readr::parse_number(d13C))

usethis::use_data(refvals, overwrite = TRUE)
