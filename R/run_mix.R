#' Run MixSIAR Model
#'
#' @param mix_data mix data for MixSIAR::load_mix_data function
#' @param factors MixSIAR::load_mix_data factors parameter
#' @param fac_random MixSIAR::load_mix_data fac_random parameter
#' @param fac_nested MixSIAR::load_mix_data fac_nested parameter
#' @param biplot_name pass filename to MixSIAR::plot_data filename parameter
#' @param alpha.prior Pass alpha.prior value to MixSIAR::plot_prior and MixSIAR::run_model
#' @param priorplot_name Pass filename to MixSIAR::plot_prior filename parameter
#' @param JAGSmodelname Pass filename to MixSIAR::write_JAGS_model filename parameter
#' @param resid_err Pass to MixSIAR::write_JAGS_model resid_err parameter
#' @param process_err Pass to MixSIAR::write_JAGS_model process_err parameter
#' @param run Pass to MixSIAR::run_model run parameter
#' @param new_dir Pass create directory name to mixsiar_save
#' @param Sites Number of sites in plot (1 or more than one)
#' @param timer Timer variable (Stat or Period)
#' @param mixed Use nlme for stat test (default is FALSE)
#' @param rm_outs Remove outliers if TRUE (default is FALSE)
#'
#' @return MixSIAR Output
#' @export
#'
#' @examples
#' #See test-run_mix
run_mix <- function(mix_data,
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
                    rm_outs = FALSE){
  #Drop NAs
  mix_data <- mix_data %>%
    tidyr::drop_na(d13C, d15N, tidyselect::any_of(timer))

  # Outliers
  outliers <- mix_data |>
    dplyr::filter(d13C %in% grDevices::boxplot.stats(mix_data[["d13C"]])$out |
                    d15N %in% grDevices::boxplot.stats(mix_data[["d15N"]])$out)

  if (rm_outs == TRUE){
    mix_data <- mix_data |>
      dplyr::mutate(d13C = ifelse(d13C %in% grDevices::boxplot.stats(mix_data[["d13C"]])$out, NA, d13C),
                    d15N = ifelse(d15N %in% grDevices::boxplot.stats(mix_data[["d15N"]])$out, NA, d15N)) |>
      tidyr::drop_na(d13C, d15N)

  }

  # Collect console output
  sink(file = "consoleoutput.txt")

  # Load Data
  utils::write.csv(mix_data, "tmpmix.csv", row.names = FALSE)
  md <- MixSIAR::load_mix_data(filename = "tmpmix.csv",
                               iso_names = c("d15N", "d13C"),
                               factors = factors,
                               fac_random = fac_random,
                               fac_nested = fac_nested,
                               cont_effects = NULL)
  utils::write.csv(refvals, "tmpsource.csv", row.names = FALSE)
  sd <- MixSIAR::load_source_data(filename = "tmpsource.csv",
                                  conc_dep = FALSE,
                                  data_type = "raw",
                                  mix = md)
  utils::write.csv(discrimination, "tmpdisc.csv", row.names = FALSE)
  dd <- MixSIAR::load_discr_data(filename = "tmpdisc.csv",
                                 mix = md)

  # Plot Data
  p <- MixSIAR::plot_data(filename = biplot_name,
                     plot_save_pdf = FALSE,
                     plot_save_png = FALSE,
                     mix = md,
                     source = sd,
                     discr = dd,
                     return_obj = TRUE)

  p <- p + ggplot2::scale_color_manual(values = rep("black", 300)) +
    ggplot2::scale_shape_manual(values = rep("circle", 300)) +
    ggplot2::theme(legend.position="none")

  ggplot2::ggsave(paste0(biplot_name, "_1_2.png"), p,
                  width = 2100,
                  height = 2100,
                  units = "px")

  # Plot Raw Source
  biplot_raw_source()

  # Plot Raw Source and Consumers
  biplot_raw_sourcon(mixdata = mix_data, Sites = Sites, timer = timer)
  biplot_raw_sourcon(mixdata = mix_data, Sites = Sites, timer = timer,
                     title = "IsoSpaceRawSourceandConsumerNoLabs",
                     labelID = FALSE)

  # Plot Source Summary
  source_biplot()

  # Plot Source Summary Raw Consumer
  sourcecon_biplot(mixdata = mix_data, Sites = Sites, timer = timer)
  sourcecon_biplot(mixdata = mix_data, Sites = Sites, timer = timer,
                   title = "IsoSpaceSSRCNoLabs",
                   labelID = FALSE)

  # Box Plot
  main_box(mixdata = mix_data)


  # Calculate Convex Hull Area
  CHA <- MixSIAR::calc_area(mix = md,
                     source = sd,
                     discr = dd)

  # Plot Prior
  MixSIAR::plot_prior(alpha.prior = alpha.prior,
                      source = sd,
                      plot_save_pdf = TRUE,
                      plot_save_png = FALSE,
                      filename = priorplot_name)

  # Choose Model Structure Options
  MixSIAR::write_JAGS_model(
    filename = JAGSmodelname,
    resid_err = resid_err,
    process_err = process_err,
    mix = md,
    source = sd)

  # Run Model
  mix_run <- MixSIAR::run_model(
    run = run,
    mix = md,
    source = sd,
    discr = dd,
    model_filename = JAGSmodelname,
    alpha.prior = alpha.prior,
    resid_err = NULL,
    process_err = NULL
  )

  MixSIAR::output_JAGS(
    jags.1 = mix_run,
    mix = md,
    source = sd,
    output_options = list(summary_save = TRUE, summary_name = "summary_statistics",
                          sup_post = FALSE, plot_post_save_pdf = TRUE, plot_post_name = "posterior_density",
                          sup_pairs = FALSE, plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", sup_xy
                          = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                            FALSE, geweke = TRUE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                            FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                            FALSE, diag_save_ggmcmc = TRUE)
  )

  # Move Output to New Directory
  new_new_dir <- new_dir
  mixsiar_save <- function(direct = new_new_dir,
                           isospace_filename = biplot_name,
                           model_filename = JAGSmodelname){

    #List files with auto-generated names
    file_list <- list("priorplot.pdf", "pairs_plot.pdf",
                      "summary_statistics.txt", "diagnostics.txt",
                      "diagnostics.pdf", "tmpdisc.csv", "tmpmix.csv",
                      "tmpsource.csv", "consoleoutput.txt",
                      "IsoSpaceFoodSourceDist.png",
                      "IsoSpaceFoodSourceDistlegend.png",
                      "IsoSpaceRawSourceandConsumer.png",
                      "IsoSpaceRawSourceandConsumerNoLabs.png",
                      "IsoSpaceSourceSummary.png",
                      "IsoSpaceSSRC.png",
                      "IsoSpaceSSRCNoLabs.png",
                      "d15N_box.png",
                      "d13C_box.png")

    #Append model_filename to file_list
    file_list <- append(file_list, model_filename)

    #Get posterior_density_diet files and append to file_list
    posterior_list <- list.files(path=".",
                                 pattern=utils::glob2rx("posterior_density_*"),
                                 full.names=FALSE)
    file_list <- append(file_list, posterior_list)

    #Get isospace_filename files and append to file_list
    isospace_filename <- paste0(isospace_filename, "_1_2.png")
    isospace_list <- list.files(path=".",
                                pattern=utils::glob2rx(isospace_filename),
                                full.names=FALSE)
    file_list <- append(file_list, isospace_list)

    #Create new directory
    dir.create(direct)

    #Get paths to new directories
    new_loc <- paste0(direct, "/")
    new_paths <- purrr::map2(new_loc, file_list, paste0)

    #Move files to new_dir
    purrr::map2(file_list, new_paths, file.copy)

    #Remove files from original directory
    purrr::map(file_list, file.remove)
  }

  old <- options(pillar.sigfig = 5)

  summarize_func <- function(data, x, grouper = NULL){


    if (is.null(grouper)){
      dtab <- data %>%
        dplyr::summarize(min = min(.data[[x]]),
                         q1 = stats::quantile(.data[[x]], 0.25),
                         median = stats::median(.data[[x]]),
                         q3 = stats::quantile(.data[[x]], 0.75),
                         max = max(.data[[x]]),
                         mean = mean(.data[[x]]),
                         sd = stats::sd(.data[[x]]),
                         n = dplyr::n()
                         )
    } else {
      dtab <- data %>%
        dplyr::group_by(.data[[grouper]]) %>%
        dplyr::summarize(min = min(.data[[x]]),
                         q1 = stats::quantile(.data[[x]], 0.25),
                         median = stats::median(.data[[x]]),
                         q3 = stats::quantile(.data[[x]], 0.75),
                         max = max(.data[[x]]),
                         mean = mean(.data[[x]]),
                         sd = stats::sd(.data[[x]]),
                         n = dplyr::n()
                         )
    }


    return(dtab)
  }

  source_d13C <- summarize_func(refvals, "d13C")
  source_d15N <- summarize_func(refvals, "d15N")
  source_d13C_Group <- summarize_func(refvals, "d13C", grouper = "Group")
  source_d15N_Group <- summarize_func(refvals, "d15N", grouper = "Group")
  cons_d13C <- summarize_func(mix_data, "d13C")
  cons_d15N <- summarize_func(mix_data, "d15N")
  cons_d13C_time <- summarize_func(mix_data, "d13C", grouper = timer)
  cons_d15N_timer <- summarize_func(mix_data, "d15N", grouper = timer)

  if (Sites != 1){
    cons_d13C_Site <- summarize_func(mix_data, "d13C", "Site")
    cons_d15N_Site <- summarize_func(mix_data, "d15N", "Site")
  }

  cat("Convex Hull Area: ", CHA, "\n")
  cat("Source Descriptives: d13C", "\n")
  print(source_d13C)
  cat("\n")
  cat("Source Descriptives: d15N", "\n")
  print(source_d15N)
  cat("\n")
  cat("Source Descriptives: d13C", "\n")
  print(source_d13C_Group)
  cat("\n")
  cat("Source Descriptives: d15N", "\n")
  print(source_d15N_Group)
  cat("\n")
  cat("Consumer Descriptives: d13C", "\n")
  print(cons_d13C)
  cat("\n")
  cat("Consumer Descriptives: d15N", "\n")
  print(cons_d15N)
  cat("\n")
  cat("Consumer Descriptives: d13C", "\n")
  print(cons_d13C_time)
  cat("\n")
  cat("Consumer Descriptives: d15N", "\n")
  print(cons_d15N_timer)
  cat("\n")

  if (Sites != 1){
    cat("Consumer Descriptives: d13C", "\n")
    print(cons_d13C_Site)
    cat("\n")
    cat("Consumer Descriptives: d15N", "\n")
    print(cons_d15N_Site)
    cat("\n")
  }


  cat("Consumer Test: d13C", "\n")
  print(comp_isos(mix_data, "d13C", timer, re = "Site", mixed = mixed))
  cat("\n")
  cat("Consumer Test: d15N", "\n")
  print(comp_isos(mix_data, "d15N", timer, re = "Site",  mixed = mixed))
  cat("\n")

  cat("Outliers Removed Before Running Analysis?", "\n",
      rm_outs, "\n")
  cat("Show outliers", "\n")
  print(outliers)
  cat("\n")

  options(old)

  out <- list(CHA = CHA, mix_run = mix_run)

  # Close sink
  sink(file = NULL)

  # Move output to new directory
  mixsiar_save()

  return(out)
}
