#' Raw Source Biplot
#'
#' @param data Reference Values
#' @param title title of plot
#' @param united Unite plot and legend?  Default is FALSE
#' @param shrink_leg Shrink legend?  Default is FALSE
#'
#' @return Plot
#' @export
#'
#' @examples
#' # biplot_raw_source()
biplot_raw_source <- function(data = refvals, title = "IsoSpaceFoodSourceDist", united = FALSE, shrink_leg = FALSE){

  data <- data %>% dplyr::select(-Organism) %>%
    dplyr::rename(Organism = PlotOrg)


  plotter <- ggplot2::ggplot(data, ggplot2::aes(x = d15N, y = d13C, colour = Organism, shape = Group)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical") +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2, byrow=TRUE)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::ylab(expression(paste(delta^{13}, "C (\u2030)")))

  if (shrink_leg == TRUE){
    plotter <- plotter +
      ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(size = 1))) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 0.7))) +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 8),
                     legend.text = ggplot2::element_text(size = 8))
  }

  #Create titles to save graphs
  title_plot <- paste0(title, ".png")
  title_leg  <- paste0(title, "legend", ".png")

  #Use unite to determine if plot and legend should be seperate
  if (united == TRUE){
    out <- list(plotter, ggplot2::ggsave(title_plot, plotter,
                                         width = 2100,
                                         height = 2100,
                                         units = "px"))
    return(out)
  } else {
    leg <- ggpubr::as_ggplot(ggpubr::get_legend(plotter)) #ggpubr functions
    graph <- plotter + ggplot2::theme(legend.position = "none")
    out <- list(graph, ggplot2::ggsave(title_plot, graph,
                                       width = 2100,
                                       height = 2100,
                                       units = "px"),
                leg, ggplot2::ggsave(title_leg, leg,
                                     width = 2100,
                                     height = 2100,
                                     units = "px"))
    return(out)
  }
}


#' Raw Source and Consumer Biplot
#'
#' @param refdata Source data
#' @param mixdata Cosumer data
#' @param title Title
#' @param Sites Number of sites
#' @param timer Time stratification variable
#' @param labelID Label ID (TRUE) or not (FALSE)?  Default is TRUE
#'
#' @return Plot
#' @export
#'
#' @examples
#' #Add later
biplot_raw_sourcon <- function(refdata = refvals, mixdata, title = "IsoSpaceRawSourceandConsumer", Sites = 1,
                               timer = "Stat", labelID = TRUE){


  data <- mixdata %>% dplyr::rename(Group = Site, ID = UniqueID) %>%
    dplyr::bind_rows(refdata %>% dplyr::mutate(Stat = "Source",
                                               Period = "Source",
                                               `Stat Period` = "Source") %>%
                       dplyr::mutate(dplyr::across(c("Stat", "Period", "Stat Period"), as.factor))) %>%
    dplyr::mutate(dplyr::across(c("Stat", "Period", "Stat Period"), ~forcats::fct_relevel(., "Source", after = Inf)))

  if (timer == "Site"){
    data <- data |> dplyr::mutate(Type = factor(ifelse(Stat == "Source", "Source", "Consumer")))
  }

  source_level <-
    c(unique(data$Group))[!(unique(data$Group) %in%
                                            c("Terrestrial Protein", "Marine Protein", "Freshwater Fish","C3 Plants"))]

  data <- data %>%
    dplyr::mutate(Group = factor(Group,
                                 levels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein",
                                                                   source_level),
                                                labels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein",
                                                           source_level)))

  if (timer == "Site"){
    p <- ggplot2::ggplot(data, ggplot2::aes(x = d15N, y = d13C, colour = Group, shape = Type))
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = d15N, y = d13C, colour = Group, shape = .data[[timer]]))
  }

  p <- p +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::theme(legend.position="bottom", legend.direction = "horizontal", legend.box = "vertical") +
    ggplot2::guides(fill=ggplot2::guide_legend(nrow=2, byrow=TRUE)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::ylab(expression(paste(delta^{13}, "C (\u2030)")))

  if (labelID == TRUE){
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label=ID), hjust=0, vjust=0, show.legend = FALSE)
  }

  # if (Sites == 1){
  #   # library(scales)
  #   # #extract hex color codes for a plot with three elements in ggplot2
  #   # hex <- hue_pal()(5)
  #   plotter <- plotter + ggplot2::scale_color_manual(values = c("#F8766D", "#A3A500", "#00A7FF", "#E76BF3","#00BF7D"))
  # }

  if (Sites == 1){
    if (source_level == "Øm Kloster"){
      conscolor <- c("Øm Kloster" = "#00BFC4")
    } else if (source_level == "St. Mikkel"){
      conscolor <- c("St. Mikkel" = "#D89000")
    } else if (source_level == "Ribe") {
      conscolor = c("Ribe" = "#39B600")
    } else if (source_level == "Tirup"){
      conscolor = c("Tirup" = "#FF62BC")
    }
  } else {
      conscolor = c("Øm Kloster" = "#00BFC4", "St. Mikkel" = "#D89000",
                    "Ribe" = "#39B600", "Tirup" = "#FF62BC")
      }

    p <- p +
      ggplot2::scale_color_manual(values =
                                    c("C3 Plants" = "#F8766D",
                                      "Freshwater Fish" = "#A3A500",
                                      "Marine Protein" = "#00A7FF", "Terrestrial Protein" = "#E76BF3",
                                      conscolor))

  if (timer == "Stat") {
    shapes <- c("Peasant" = "square",
                "Elite" = "triangle",
                "Monk" = "plus",
                "Source" = "circle")
  } else if (timer == "Period"){
    shapes <- c("Early" = "square",
                "Middle" = "triangle",
                "Late" = "plus",
                "Source" = "circle")
  } else if (timer == "Site"){
    shapes <- c("Consumer" = "square",
                "Source" = "circle")
  }

  p <- p + ggplot2::scale_shape_manual(values = c(shapes),
                                       name = ggplot2::waiver(),
                                       ggplot2::guide_legend(order = 1)) #+
    #ggplot2::guides(shape = ggplot2::guide_legend(order = 1))

  #Create titles to save graphs
  title_plot <- paste0(title, ".png")

  out <- list(p, ggplot2::ggsave(title_plot, p,
                                       width = 2100,
                                       height = 2100,
                                       units = "px"))
  return(out)


}


#' Source Biplot
#'
#' @param data Source data
#' @param Group Group variable
#' @param var1 var 1
#' @param var2 var 2
#' @param title Title
#'
#' @return Plot
#' @export
#'
#' @examples
#' # Add later
source_biplot <- function(data = refvals, Group = Group, var1 = d15N, var2 = d13C,
                          title = "IsoSpaceSourceSummary"){
  #Store tdf values
  tdf_d15N_mean <- discrimination$Meand15N[[1]]
  tdf_d15N_sd   <- discrimination$SDd15N[[1]]
  tdf_d13C_mean <- discrimination$Meand13C[[1]]
  tdf_d13C_sd   <- discrimination$SDd13C[[1]]

  data <- refvals %>%
    dplyr::mutate(Group = factor(Group,
                                 levels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein"),
                                 labels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein")))

  data_summary <- data %>%
    dplyr::group_by({{Group}}) %>%
    dplyr::mutate(var1_mean = mean({{var1}}),
           var2_mean = mean({{var2}}),
           var1_sd = stats::sd({{var1}}),
           var2_sd = stats::sd({{var2}})) %>%
    dplyr::ungroup() %>%
    dplyr::distinct({{Group}}, .keep_all = TRUE) %>%
    dplyr::select({{Group}}, var1_mean, var1_sd, var2_mean, var2_sd) %>%
    dplyr::mutate(var1_mean = var1_mean + tdf_d15N_mean,
                  var2_mean = var2_mean + tdf_d13C_mean,
                  var1_sd = var1_sd + tdf_d15N_sd,
                  var2_sd = var2_sd + tdf_d13C_sd)

  p <- ggplot2::ggplot(data = data_summary, ggplot2::aes(x=var1_mean, y=var2_mean, colour={{Group}})) +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(ymin=var2_mean-var2_sd, ymax=var2_mean+var2_sd)) +
    ggplot2::geom_linerange(ggplot2::aes(xmin=var1_mean-var1_sd, xmax=var1_mean+var1_sd)) +
    ggplot2::xlab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::ylab(expression(paste(delta^{13}, "C (\u2030)")))

  #Create titles to save graphs
  title_plot <- paste0(title, ".png")

  out <- list(p, ggplot2::ggsave(title_plot, p,
                                 width = 2100,
                                 height = 2100,
                                 units = "px"))
  return(out)

}


#' Source and Consumer Biplot
#'
#' @param refdata Source data
#' @param mixdata Consumer data
#' @param Sites Number of sites
#' @param title Title
#' @param timer Time stratification variable
#' @param labelID Label ID (TRUE) or not (FALSE)?  Default is TRUE
#'
#' @return A plot
#' @export
#'
#' @examples
#' #Add later
sourcecon_biplot <- function(refdata = refvals, mixdata, Sites = 1, title = "IsoSpaceSSRC",
                             timer = "Stat", labelID = TRUE){

  # Get edge data for sources
  combined_summary <- refdata %>%
    dplyr::group_by(Group) %>%
    dplyr::mutate(d13C_mean = mean(d13C),
           d15N_mean = mean(d15N),
           d13C_sd = stats::sd(d13C),
           d15N_sd = stats::sd(d15N)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Group, .keep_all = TRUE) %>%
    dplyr::select(Group, d13C_mean, d13C_sd, d15N_mean, d15N_sd) %>%
    dplyr::mutate(d13C_mean = d13C_mean + discrimination$Meand13C[[1]],
           d15N_mean = d15N_mean + discrimination$Meand15N[[1]],
           d13C_sd = d13C_sd + discrimination$SDd13C[[1]],
           d15N_sd = d15N_sd + discrimination$SDd15N[[1]])

  #Get the edge data points for sources
  combined_edge_cmin <- combined_summary %>%
    dplyr::mutate(d13C_edge = d13C_mean - d13C_sd,
                  d15N_edge = d15N_mean) %>%
    dplyr::select(Group, d13C_edge, d15N_edge)

  combined_edge_cmax <- combined_summary %>%
    dplyr::mutate(d13C_edge = d13C_mean + d13C_sd,
                  d15N_edge = d15N_mean) %>%
    dplyr::select(Group, d13C_edge, d15N_edge)

  combined_edge_nmin <- combined_summary %>%
    dplyr::mutate(d13C_edge = d13C_mean,
                  d15N_edge = d15N_mean - d15N_sd) %>%
    dplyr::select(Group, d13C_edge, d15N_edge)

  combined_edge_nmax <- combined_summary %>%
    dplyr::mutate(d13C_edge = d13C_mean,
                  d15N_edge = d15N_mean + d15N_sd) %>%
    dplyr::select(Group, d13C_edge, d15N_edge)

  combined_edge <- dplyr::bind_rows(combined_edge_cmin, combined_edge_cmax,
                                    combined_edge_nmin, combined_edge_nmax)

  # Combine edge points with summary data
  all_source <- combined_summary %>% dplyr::left_join(combined_edge, by = "Group")

  # Process Consumer data
  mixdata <- mixdata %>% dplyr::rename(Group = Site, ID = UniqueID) %>%
    #filter(group == "Urban Europe") %>%
    dplyr::group_by(Group) %>%
    dplyr::mutate(d13C_mean = d13C,
           d13C_sd = 0,
           d15N_mean = d15N,
           d15N_sd = 0,
           d13C_edge = d13C,
           d15N_edge = d15N) %>%
    dplyr::ungroup() %>%
    dplyr::select(-d13C, -d15N)

  # Combine Source and Consumer Data
  data <- mixdata %>% dplyr::bind_rows(all_source %>% dplyr::mutate(Stat = "Source",
                                       Period = "Source",
                                       `Stat Period` = "Source") %>%
                                         dplyr::mutate(dplyr::across(c("Stat", "Period", "Stat Period"), as.factor))) %>%
    dplyr::mutate(dplyr::across(c("Stat", "Period", "Stat Period"), ~forcats::fct_relevel(., "Source", after = Inf)))

  # Process Factor
  source_level <-
    c(unique(data$Group))[!(unique(data$Group) %in%
                              c("Terrestrial Protein", "Marine Protein", "Freshwater Fish","C3 Plants"))]

  data <- data %>%
    dplyr::mutate(Group = factor(Group,
                                 levels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein",
                                            source_level),
                                 labels = c("C3 Plants", "Freshwater Fish", "Marine Protein", "Terrestrial Protein",
                                            source_level)))

  if (timer == "Site"){
    data <- data |> dplyr::mutate(Type = factor(ifelse(Stat == "Source", "Source", "Consumer")))
  }

  #Create raw data isospace plot
  if (timer == "Site"){
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x= d15N_edge, y= d13C_edge, colour =Group, shape = Type))
  } else {
    p <- ggplot2::ggplot(data = data, ggplot2::aes(x= d15N_edge, y= d13C_edge, colour =Group, shape = .data[[timer]]))
  }
  p <- p +
    ggplot2::geom_point(size = 0.1, stroke = 0) +
    ggplot2::geom_point(ggplot2::aes(x=d15N_mean, y= d13C_mean, colour=Group)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin=d13C_mean-d13C_sd, ymax=d13C_mean+d13C_sd)) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin=d15N_mean-d15N_sd, xmax=d15N_mean+d15N_sd)) +
    ggplot2::geom_point(ggplot2::aes(x=d15N_mean, y=d13C_mean, colour=Group)) +
    ggplot2::xlab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::ylab(expression(paste(delta^{13}, "C (\u2030)")))

  if (labelID == TRUE){
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label=ID), hjust=0, vjust=0, show.legend = FALSE)
  }

  if (Sites == 1){
    if (source_level == "Øm Kloster"){
      conscolor <- c("Øm Kloster" = "#00BFC4")
    } else if (source_level == "St. Mikkel"){
      conscolor <- c("St. Mikkel" = "#D89000")
    } else if (source_level == "Ribe") {
      conscolor = c("Ribe" = "#39B600")
    } else if (source_level == "Tirup"){
      conscolor = c("Tirup" = "#FF62BC")
    }
  } else {
      conscolor = c("Øm Kloster" = "#00BFC4", "St. Mikkel" = "#D89000",
                   "Ribe" = "#39B600", "Tirup" = "#FF62BC")
    }

    p <- p +
      ggplot2::scale_color_manual(values =
                                    c("C3 Plants" = "#F8766D",
                                      "Freshwater Fish" = "#A3A500",
                                      "Marine Protein" = "#00A7FF", "Terrestrial Protein" = "#E76BF3",
                                      conscolor))

  if (timer == "Stat") {
    shapes <- c("Peasant" = "square",
                "Elite" = "triangle",
                "Monk" = "plus",
                "Source" = "circle")
  } else if (timer == "Period"){
    shapes <- c("Early" = "square",
                "Middle" = "triangle",
                "Late" = "plus",
                "Source" = "circle")
  } else if (timer == "Site"){
    shapes <- c("Consumer" = "square",
                "Source" = "circle")
  }

  p <- p + ggplot2::scale_shape_manual(values = c(shapes),
                                       name = ggplot2::waiver(),
                                       ggplot2::guide_legend(order = 1))

  #Create titles to save graphs
  title_plot <- paste0(title, ".png")

  out <- list(p, ggplot2::ggsave(title_plot, p,
                                 width = 2100,
                                 height = 2100,
                                 units = "px"))
  return(out)

}



#' Main Boxplot Function
#'
#' @param mixdata Consumer data
#'
#' @return Plot
#' @export
#'
#' @examples
#' # Add later
main_box <- function(mixdata){
  #Create boxplots
  d15N_box <- ggplot2::ggplot(mixdata %>%
                                dplyr::rename(Group = Site),
                              ggplot2::aes(x=Group, y=d15N)) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::xlab("Location")

  #Create boxplots
  d13C_box <- ggplot2::ggplot(mixdata %>%
                                dplyr::rename(Group = Site),
                              ggplot2::aes(x=Group, y=d13C)) +
    ggplot2::geom_boxplot() +
    ggplot2::ylab(expression(paste(delta^{13}, "N (\u2030)"))) +
    ggplot2::xlab("Location")

  #Create titles to save graphs
  out <- list(ggplot2::ggsave("d15N_box.png", d15N_box,
                              width = 2100,
                              height = 2100,
                              units = "px"),
              ggplot2::ggsave("d13C_box.png", d13C_box,
                              width = 2100,
                              height = 2100,
                              units = "px"))
  return(out)
}

