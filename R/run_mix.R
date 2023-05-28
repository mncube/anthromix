run_mix <- function(mix_data = tab3,
                    factors = c("Site", "Period"),
                    fac_random = c(TRUE, TRUE),
                    fac_nested = c(FALSE, TRUE),
                    biplot_name = "biplot",
                    alpha.prior = 1,
                    priorplot_name = "priorplot",
                    JAGSmodelname = "MixSIAR_model.txt",
                    resid_err = TRUE,
                    process_err = TRUE,
                    run = "test",
                    new_dir = "mixing_model_output"){

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
  MixSIAR::plot_data(filename = biplot_name,
                     plot_save_pdf = FALSE,
                     plot_save_png = TRUE,
                     mix = md,
                     source = sd,
                     discr = dd,
                     return_obj = FALSE)

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
                      "diagnostics.pdf", "tmpdisc.csv", "tmpmix.csv", "tmpsource.csv")

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

  mixsiar_save()

  cat("Convex Hull Area: ", CHA, "\n")

  out <- list(CHA = CHA, mix_run = mix_run)

  return(out)
}
