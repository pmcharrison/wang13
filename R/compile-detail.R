compile_detail <- function(x) {
  x$spectrum_input <- data.frame(frequency_Hz = x$frequency_Hz,
                                 level_dB = x$level_dB)
  x$spectrum_after_ear_filtering <- data.frame(x$frequency_Hz,
                                               level_dB = x$level_dB_filtered)
  x[c(
    "spectrum_input",
    "spectrum_after_ear_filtering",
    "channel_wave_forms",
    "channel_envelopes",
    "filtered_channel_envelopes",
    "modulation_indices",
    "phase_impact_factors",
    "specific_roughnesses",
    "total_roughness"
  )]
}
