plot_modulation_indices_wang <- function(modulation_indices,
                                         theme = ggplot2::theme_bw()) {
  if (!loadNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(modulation_indices),
    length(modulation_indices) == 47
  )
  df <- data.frame(
    x = seq_along(modulation_indices),
    y = modulation_indices
  )
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Modulation index") +
    theme
  print(p)
}

plot_phase_impact_factors_wang <- function(phase_impact_factors,
                                           theme = ggplot2::theme_bw()) {
  if (!loadNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(phase_impact_factors),
    length(phase_impact_factors) == 47
  )
  df <- data.frame(
    x = seq_along(phase_impact_factors),
    y = phase_impact_factors
  )
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Phase impact factor") +
    theme
  print(p)
}

plot_specific_roughnesses_wang <- function(specific_roughnesses,
                                           theme = ggplot2::theme_bw()) {
  if (!loadNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(specific_roughnesses),
    length(specific_roughnesses) == 47
  )
  df <- data.frame(
    x = seq_along(specific_roughnesses),
    y = specific_roughnesses
  )
  p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Specific roughness") +
    theme
  print(p)
}
