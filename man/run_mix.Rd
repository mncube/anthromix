% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_mix.R
\name{run_mix}
\alias{run_mix}
\title{Run MixSIAR Model}
\usage{
run_mix(
  mix_data,
  factors = c("Period"),
  fac_random = c(TRUE),
  fac_nested = c(FALSE),
  biplot_name = "biplot",
  alpha.prior = 1,
  priorplot_name = "priorplot",
  JAGSmodelname = "MixSIAR_model.txt",
  resid_err = TRUE,
  process_err = TRUE,
  run = "test",
  new_dir = "mixing_model_output",
  Sites = 1,
  timer = "Period",
  mixed = FALSE,
  rm_outs = FALSE
)
}
\arguments{
\item{mix_data}{mix data for MixSIAR::load_mix_data function}

\item{factors}{MixSIAR::load_mix_data factors parameter}

\item{fac_random}{MixSIAR::load_mix_data fac_random parameter}

\item{fac_nested}{MixSIAR::load_mix_data fac_nested parameter}

\item{biplot_name}{pass filename to MixSIAR::plot_data filename parameter}

\item{alpha.prior}{Pass alpha.prior value to MixSIAR::plot_prior and MixSIAR::run_model}

\item{priorplot_name}{Pass filename to MixSIAR::plot_prior filename parameter}

\item{JAGSmodelname}{Pass filename to MixSIAR::write_JAGS_model filename parameter}

\item{resid_err}{Pass to MixSIAR::write_JAGS_model resid_err parameter}

\item{process_err}{Pass to MixSIAR::write_JAGS_model process_err parameter}

\item{run}{Pass to MixSIAR::run_model run parameter}

\item{new_dir}{Pass create directory name to mixsiar_save}

\item{Sites}{Number of sites in plot (1 or more than one)}

\item{timer}{Timer variable (Stat or Period)}

\item{mixed}{Use nlme for stat test (default is FALSE)}

\item{rm_outs}{Remove outliers if TRUE (default is FALSE)}
}
\value{
MixSIAR Output
}
\description{
Run MixSIAR Model
}
\examples{
#See test-run_mix
}
