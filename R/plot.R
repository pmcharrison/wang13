plot_modulation_indices_wang <- function(modulation_indices,
                                         theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(modulation_indices),
    length(modulation_indices) == 47
  )
  df <- data.frame(
    x = seq_along(modulation_indices),
    y = modulation_indices
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Modulation index") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_phase_impact_factors_wang <- function(phase_impact_factors,
                                           theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(phase_impact_factors),
    length(phase_impact_factors) == 47
  )
  df <- data.frame(
    x = seq_along(phase_impact_factors),
    y = phase_impact_factors
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Phase impact factor") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_specific_roughnesses_wang <- function(specific_roughnesses,
                                           theme = cowplot::theme_cowplot()) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  assertthat::assert_that(
    is.numeric(specific_roughnesses),
    length(specific_roughnesses) == 47
  )
  df <- data.frame(
    x = seq_along(specific_roughnesses),
    y = specific_roughnesses
  )
  ggplot2::ggplot(df, ggplot2::aes_string(x = "x", y = "y")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Channel number") +
    ggplot2::scale_y_continuous("Specific roughness") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_waveform <- function(
  x,
  sample_rate = 44000,
  range_sec = c(0, 0.2),
  theme = cowplot::theme_cowplot()
) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  df <- data.frame(
    t = seq(from = 0, by = 1 / sample_rate, length.out = length(x)),
    x = x
  ) %>%
    (function(df) df[df$t >= range_sec[1] & df$t <= range_sec[2], ])
  ggplot2::ggplot(df, ggplot2::aes_string(x = "t", y = "x")) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_x_continuous("Time (sec)") +
    ggplot2::scale_y_continuous("Instantaneous amplitude") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}

plot_filtered_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_after_ear_filtering$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(
    analysis$spectrum_after_ear_filtering$level_dB,
    60
  )
  plot_sparse_spectrum(frequency, amplitude, range_Hz = NULL)
}

plot_sparse_spectrum <- function(
  frequency,
  amplitude,
  resolution_Hz = 1,
  range_Hz = NULL,
  theme = cowplot::theme_cowplot()
) {
  if (!requireNamespace("ggplot2"))
    stop("ggplot2 must be installed before continuing")
  if (is.null(range_Hz)) range_Hz <- c(0, max(frequency))
  df <- data.frame(freq = round(frequency),
                   amp = amplitude) %>%
    (function(df) df[df$freq >= range_Hz[1] &
                       df$freq <= range_Hz[2], ]) %>%
    rbind(
      data.frame(freq = seq(from = range_Hz[1],
                            to = range_Hz[2],
                            by = resolution_Hz),
                 amp = 0)
    ) %>%
    (function(df) {
      df[order(df$freq), ]
    })
  ggplot2::ggplot(df, ggplot2::aes_string(x = "freq", y = "amp")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Frequency (Hz)") +
    ggplot2::scale_y_continuous("Amplitude") +
    theme +
    ggplot2::theme(aspect.ratio = 1)
}
