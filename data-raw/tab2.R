## code to prepare `tab2` dataset goes here
require(dplyr)
tab2 <- readxl::read_excel("./rawdatafiles/Data for MixSIAR.xlsx",
                              sheet = "OM Full Comparison") %>%
  dplyr::select(Cemetary, UniqueID = `Unique ID`, d15N = δ15N, d13C = δ13C,
                ArmPosPeriod = `Arm Pos/Period`, Stat) %>%
  dplyr::mutate(d15N = as.numeric(d15N),
                d13C = as.numeric(d13C))

usethis::use_data(tab2, overwrite = TRUE)
