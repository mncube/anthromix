#' Reference Values
#'
#'
#' @format ## `refvals`
#' A data frame with 245 rows and 6 columns:
#' \describe{
#'   \item{Group}{#}
#'   \item{Organism}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Paper Source}{#}
#'   \item{PlotOrg}{#}
#' }
"refvals"

#' TDF Sider Discrimination
#'
#'
#' @format ## `discrimination`
#' A data frame with 4 rows and 5 columns:
#' \describe{
#'   \item{Group}{#}
#'   \item{Meand15N}{#}
#'   \item{SDd15N}{#}
#'   \item{Meand13C}{#}
#'   \item{SDd13C}{#}
#' }
"discrimination"

#' Tab 2: OM Full Comparison
#'
#' The Full comparison has the data broken up into peasants (0), elite (1), and
#' monks (2). This is going to compare these socioeconomic status to each other.
#' The time portion does not get utilized in this one.
#'
#'
#' @format ## `tab2`
#' A data frame with 98 rows and 6 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab2"

#' Tab 3: OM Peasant/Time Comparison
#'
#' This comparison is looking only at peasants (0) and comparing them through time
#' which is signified by the Arm Position/Periods A, B, and C/D.  Three time periods
#' of actual time are listed as Pre 1300 CE, 1300 CE - 1375 CE, and 1375 CE to close
#' of the period (I will get an exact date on this).
#'
#'
#' @format ## `tab3`
#' A data frame with 55 rows and 6 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab3"

#' Tab 4: OM Monk/Time Comparison
#'
#' Looking at only Monks (2) and comparing them over the three time periods
#' (A, B, and C/D)
#'
#'
#' @format ## `tab4`
#' A data frame with 13 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab4"

#' Tab 5:OM Elite/Time Comparison
#'
#' Looking at only Elite (1) and comparing them over the three time periods
#' (A, B, and C/D).
#'
#'
#' @format ## `tab5`
#' A data frame with 29 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab5"

#' Tab 6:SM Peasant/Time Comparison
#'
#' SM is St. Mikkel.  This is comparing Peasants (0) and comparing them over the
#' three time periods (A, B, and C/D).  There were no samples of monks or elite
#' in SM so no additional comparisons.
#'
#'
#' @format ## `tab6`
#' A data frame with 59 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab6"

#' Tab 7:Ribe Peasant/Time Comparison
#'
#' Ribe is Ribe (not short for anything).  This is comparing Peasants (0) and
#' comparing them over the three time periods (A, B, and C/D).  There were no
#' samples of monks or elite in Ribe so no additional comparisons.
#'
#'
#' @format ## `tab7`
#' A data frame with 54 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab7"

#' Tab 8:Tirup Peasant/Time Comparison
#'
#' Tirup is Tirup (again not short for anything). This is comparing Peasants (0)
#' and comparing them over the three time periods (A, B, and C/D).  There were no
#' samples of monks or elite in Tirup so no additional comparisons.
#'
#'
#' @format ## `tab8`
#' A data frame with 32 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab8"

#' Tab 9: All Site Peasants/Time Comparison
#'
#' This takes the Peasants (0) from all 4 sites (OM, SM, Tirup, Ribe) and compares
#' them over the three time periods (A, B, and C/D).
#'
#' @format ## `tab9`
#' A data frame with 200 rows and 7 columns:
#' \describe{
#'   \item{Site}{#}
#'   \item{UniqueID}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Period}{#}
#'   \item{Stat}{#}
#'   \item{Stat Period}{#}
#' }
"tab9"
