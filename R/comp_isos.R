#' Compare Isos
#'
#' @param data Data frame
#' @param comp Outcome variable
#' @param by Main comparison variable
#' @param re Random intercept
#' @param mixed Set to TRUE to use nlme::lme for statistical test (default is
#' FALSE)
#'
#' @return Statistical Test results
#' @export
#'
#' @examples
#' #Add later
comp_isos <- function(data, comp, by, re = NULL, mixed = FALSE){
  facs <- length(unique(data[[by]]))

  if (facs == 2 & mixed == FALSE){
    res <- stats::t.test(stats::as.formula(paste(comp, "~", by)), data)
  } else if (facs > 2 & mixed == FALSE){
    mod <- stats::lm(stats::as.formula(paste(comp, "~", by)), data)
    res_anova <- stats::anova(mod)
    res_paired <- stats::pairwise.t.test(data[[comp]], data[[by]],
                                         p.adj = "bonf", pool.sd = FALSE)
    res <- list(res_anova = res_anova,
                res_paired = res_paired)
  } else if (mixed == TRUE){
    res_orig <- summary(nlme::lme(stats::as.formula(paste(comp, "~", by)), data = data,
                     random = stats::as.formula(paste("~ 1 |", re)),
                     na.action = stats::na.omit))

    data[[by]] <- forcats::fct_rev(data[[by]])

    res_rev <- summary(nlme::lme(stats::as.formula(paste(comp, "~", by)), data = data,
                             random = stats::as.formula(paste("~ 1 |", re)),
                             na.action = stats::na.omit))

    res <- list(res_orig = res_orig,
                res_rev = res_rev)
  }

  return(res)

}
