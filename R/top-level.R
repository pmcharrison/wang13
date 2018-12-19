#' Wang et al.'s (2013) roughness model
#'
#' Gets the roughness of a sonority according to the model of Wang et al. (2013).
#' @param x Object to analyse,
#' which will be coerced to an object of class
#' \code{\link[hrep]{fr_sparse_spectrum}}.
#' Various input types are possible:
#' * Numeric vectors will be treated as vectors of MIDI note numbers,
#' which will be expanded into their implied harmonics.
#' * A two-element list can be used to define a harmonic spectrum.
#' The first element should be a vector of frequencies in Hz,
#' the second a vector of amplitudes.
#' * The function also accepts classes from the \code{hrep} package,
#' such as produced by \code{\link[hrep]{pi_chord}()} and
#' \code{\link[hrep]{fr_sparse_spectrum}()}.
#' @param detail (Logical scalar) Whether to return detailed output information.
#' @param include_phase_impact_factors (Logical scalar)
#' Whether to include phase impact factors in roughness computation.
#' Set to \code{TRUE} to recover the original specifications of Wang et al. (2013).
#' However, disabling this feature (by leaving the parameter at \code{FALSE})
#' seems to result in better estimation of perceptual consonance.
#' @param unit_amplitude_in_dB (Numeric scalar)
#' Determines the decibel level of a partial with amplitude 1.
#' When the input is a musical chord,
#' this will correspond to the decibel level of the fundamental frequencies
#' of each chord tone.
#' @param msg Function to be called to give progress updates.
#' This function should accept three arguments:
#' \code{n}, an integer identifying the current position in the pipeline,
#' \code{N}, an integer identifying the length of the pipeline,
#' and \code{msg}, a string providing a longer-format description
#' of the current position in the pipeline.
#' @param ... Additional parameters to pass to
#' \code{\link[hrep]{fr_sparse_spectrum}}.
#' @return If \code{detail == FALSE}, a numeric vector of roughnesses,
#' otherwise a list containing detailed algorithm output.
#' @references
#' \insertRef{Wang2013}{wang13}
#' @note
#' This implementation is designed for sparse input spectra, that is,
#' spectra containing only a few (< 100) components.
#' @md
#' @rdname roughness_wang
#' @export
roughness_wang <- function(
  x,
  detail = FALSE,
  include_phase_impact_factors = FALSE,
  unit_amplitude_in_dB = 60,
  msg = function(n, N, msg)
    if (interactive())
      message(n, "/", N, ": ", msg),
  ...
) {
  UseMethod("roughness_wang")
}

#' @rdname roughness_wang
#' @export
roughness_wang.default <- function(
  x,
  detail = FALSE,
  include_phase_impact_factors = FALSE,
  unit_amplitude_in_dB = 60,
  msg = function(n, N, msg)
    if (interactive())
      message(n, "/", N, ": ", msg),
  ...
) {
  x <- hrep::fr_sparse_spectrum(x, ...)
  do.call(roughness_wang, as.list(environment()))
}

#' @rdname roughness_wang
#' @export
roughness_wang.fr_sparse_spectrum <- function(
  x,
  detail = FALSE,
  include_phase_impact_factors = FALSE,
  unit_amplitude_in_dB = 60,
  msg = function(n, N, msg)
    if (interactive())
      message(n, "/", N, ": ", msg)
) {
  frequency_Hz <- hrep::freq(x)
  level_dB <- hrep::amplitude_to_dB(hrep::amp(x), unit_amplitude_in_dB)

  msg(1, 6, "Ear transmission...")
  level_dB_filtered <- level_dB - ear_transmission(frequency_Hz)

  msg(2, 6, "Channel excitation levels...")
  channel_sound_excitation_levels <- get_channel_sound_excitation_levels(
    frequency_Hz = frequency_Hz,
    level_dB_filtered = level_dB_filtered
  )

  # <channel_wave_forms> is a list of numeric vectors corresponding to the
  # y values of the waveforms for each channel for time in [0s, 1s].
  # The units of y is amplitude ratios relative to the reference sound amplitude.
  msg(3, 6, "Channel waveforms...")
  channel_wave_forms <- purrr::map(.x = channel_sound_excitation_levels,
                                   .f = get_channel_wave_form,
                                   frequency_Hz)

  # These are waveforms corresponding to the signal envelopes
  msg(4, 6, "Channel envelopes...")
  channel_envelopes <- purrr::map(.x = channel_wave_forms,
                                  .f = get_channel_envelope)

  # The channel envelopes are filtered to account for the different roughness
  # contributions of different modulation frequencies
  msg(5, 6, "Filtering channel envelopes...")
  filtered_channel_envelopes <- purrr::map2(.x = seq_along(channel_envelopes),
                                            .y = channel_envelopes,
                                            .f = filter_channel_envelope)

  msg(6, 6, "Computing roughness...")
  modulation_indices <- purrr::map2_dbl(.x = filtered_channel_envelopes,
                                        .y = channel_wave_forms,
                                        .f = get_modulation_index)

  phase_impact_factors <- purrr::map_dbl(.x = seq_len(47),
                                         .f = get_phase_impact_factor,
                                         filtered_channel_envelopes)

  specific_roughnesses <- purrr::pmap_dbl(.l = list(get_channel_weight(seq_len(47)),
                                                    phase_impact_factors,
                                                    modulation_indices),
                                          .f = get_specific_roughness,
                                          include_phase_impact_factors)

  total_roughness <- 0.25 * sum(specific_roughnesses)

  if (detail)
    compile_detail(as.list(environment())) else
      total_roughness
}
