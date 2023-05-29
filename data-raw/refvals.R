## code to prepare `refvals` dataset goes here
require(dplyr)
require(stringr)
require(readr)
require(tidyr)
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
                d13C = readr::parse_number(d13C)) %>%
  dplyr::filter(Group != "C4 Plants") %>%
  tidyr::drop_na(d15N, d13C) %>%
  dplyr::mutate(PlotOrg = dplyr::case_when(
    stringr::str_detect(Organism, "Cat ") ~ "Cat",
    stringr::str_detect(Organism, "Cattle") ~ "Cattle",
    stringr::str_detect(Organism, "Chicken") ~ "Chicken",
    stringr::str_detect(Organism, "Cow") ~ "Cow",
    stringr::str_detect(Organism, "Deer") ~ "Deer",
    stringr::str_detect(Organism, "Dog") ~ "Dog",
    stringr::str_detect(Organism, "Domestic Fowl") ~ "Domestic Fowl",
    stringr::str_detect(Organism, "Donkey") ~ "Donkey",
    stringr::str_detect(Organism, "Elk") ~ "Elk",
    stringr::str_detect(Organism, "Goat") ~ "Goat",
    stringr::str_detect(Organism, "Horse") ~ "Horse",
    stringr::str_detect(Organism, "Pig") ~ "Pig",
    stringr::str_detect(Organism, "Sheep ") ~ "Sheep",
    stringr::str_detect(Organism, "Sheep/Goat") ~ "Sheep/Goat",
    stringr::str_detect(Organism, "Eel") ~ "Eel",
    stringr::str_detect(Organism, "Flatfish") ~ "Flatfish",
    stringr::str_detect(Organism, "Gadidae sp.") ~ "Gadidae sp.",
    stringr::str_detect(Organism, "Haddock") ~ "Haddock",
    stringr::str_detect(Organism, "Herring") ~ "Herring",
    stringr::str_detect(Organism, "Ling") ~ "Ling",
    stringr::str_detect(Organism, "Whiting") ~ "Whiting",
    stringr::str_detect(Organism, "Carp/Bream") ~ "Carp/Bream",
    stringr::str_detect(Organism, "Catfish") ~ "Catfish",
    stringr::str_detect(Organism, "Pig") ~ "Pig",
    stringr::str_detect(Organism, "Northern Pike") ~ "Northern Pike",
    stringr::str_detect(Organism, "Pike ") ~ "Pike",
    stringr::str_detect(Organism, "Pike-Perch") ~ "Pike-Perch",
    stringr::str_detect(Organism, "Sturgeon") ~ "Sturgeon",
    stringr::str_detect(Organism, "Tench") ~ "Tench",
    TRUE ~ Organism
  ))

usethis::use_data(refvals, overwrite = TRUE)
