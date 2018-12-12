#' Spectral roughness (Wang)
#'
#' Gets the roughness of a spectrum according to the model of Wang et al. (2013).
#' @param frequency_Hz Numeric vector of frequencies (Hz)
#' @param level_dB Numeric vector of sound levels (dB)
#' @return Numeric vector of roughnesses
#' \insertRef{Wang2013}{incon}
#' @export
roughness_wang <- function(
  frequency_Hz,
  level_dB,
  detail = FALSE,
  include_phase_impact_factors = FALSE
) {
  assertthat::assert_that(length(frequency_Hz) == length(level_dB))

  level_dB_filtered <- level_dB - ear_transmission(frequency_Hz)

  channel_sound_excitation_levels <- get_channel_sound_excitation_levels(
    frequency_Hz = frequency_Hz,
    level_dB_filtered = level_dB_filtered
  )
  # <channel_wave_forms> is a list of numeric vectors corresponding to the
  # y values of the waveforms for each channel for time in [0s, 1s].
  # The units of y is amplitude ratios relative to the reference sound amplitude.
  channel_wave_forms <- purrr::map(.x = channel_sound_excitation_levels,
                                   .f = get_channel_wave_form,
                                   frequency_Hz)

  # These are waveforms corresponding to the signal envelopes
  channel_envelopes <- purrr::map(.x = channel_wave_forms,
                                  .f = get_channel_envelope)

  # The channel envelopes are filtered to account for the different roughness
  # contributions of different modulation frequencies
  filtered_channel_envelopes <- purrr::map2(.x = seq_along(channel_envelopes),
                                            .y = channel_envelopes,
                                            .f = filter_channel_envelope)

  modulation_indices <- purrr::map2(.x = filtered_channel_envelopes,
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

