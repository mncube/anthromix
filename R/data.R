#' Reference Values
#'
#'
#' @format ## `refvals`
#' A data frame with 245 rows and 5 columns:
#' \describe{
#'   \item{Group}{#}
#'   \item{Organism}{#}
#'   \item{d15N}{#}
#'   \item{d13C}{#}
#'   \item{Paper Source}{#}
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
#' }
"tab3"
