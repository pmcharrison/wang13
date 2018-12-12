get_channel_sound_excitation_levels <- function(frequency_Hz,
                                                level_dB_filtered) {
  frequency_bark <- convert_Hz_to_bark(frequency_Hz)
  lapply(
    seq_len(47),
    function(channel_num) {
      get_channel_sound_excitation_level(
        freq_Hz = frequency_Hz,
        freq_bark = frequency_bark,
        sound_intensity_level = level_dB_filtered,
        channel_num = channel_num)
    }
  )
}

# @param freq_Hz Frequency in Hz (numeric vector)
# @param freq_bark Corresponding frequency in barks (numeric vector)
# @param sound_intensity_level Sound intensity level (numeric vector, matches with freq_Hz)
# @param channel_num Critical band filter number (should be a scalar integer between 1 and 47)
# @return Numeric vector giving the excitation level for the respective channel for each frequency listed in \code{freq_Hz}
get_channel_sound_excitation_level <- function(freq_Hz,
                                               freq_bark,
                                               sound_intensity_level,
                                               channel_num) {
  assertthat::assert_that(
    is.numeric(freq_bark), is.numeric(sound_intensity_level),
    length(freq_bark) == length(sound_intensity_level),
    assertthat::is.scalar(channel_num), is.numeric(channel_num)
  )
  channel_centre_bark <- 0.5 * channel_num
  df <- data.frame(
    freq_Hz = freq_Hz,
    freq_bark = freq_bark,
    sound_intensity_level = sound_intensity_level,
    region = NA
  )
  # Assign components to regions
  df$region[0 <= df$freq_bark & freq_bark < channel_centre_bark - 0.5] <- "upper_slope"
  df$region[channel_centre_bark - 0.5 <= freq_bark &
              freq_bark <= channel_centre_bark + 0.5] <- "centre"
  df$region[channel_centre_bark + 0.5 < freq_bark &
              freq_bark <= 47] <- "lower_slope"
  # Calculate SELs for components within the critical band
  df$sound_excitation_level[df$region == "centre"] <-
    df[df$region == "centre", ] %>%
    (function(df) df$sound_intensity_level)
  # ... and for components below the critical band...
  df$sound_excitation_level[df$region == "upper_slope"] <-
    df[df$region == "upper_slope", ] %>%
    (function(df) {
      df$sound_intensity_level +
        ((channel_centre_bark + 0.5) - df$freq_bark) *
        (- 24 - (230 / df$freq_Hz) + 0.2 * df$sound_intensity_level)
    })
  # ... and for components above the critical band
  df$sound_excitation_level[df$region == "lower_slope"] <-
    df[df$region == "lower_slope", ] %>%
    (function(df) {
      df$sound_intensity_level +
        ((channel_centre_bark - 0.5) - df$freq_bark) * 27
    })
  df$sound_excitation_level
}

get_channel_wave_form <- function(excitation_levels,
                                  frequency_Hz) {
  # Excitation levels are in decibels, so we need to convert back
  # to a linear scale. The units for this linear scale don't matter,
  # because later on we normalise by the RMS of the waveform.
  assertthat::assert_that(
    all(frequency_Hz < 44000),
    all(frequency_Hz >= 0.5)
  )
  # This bit is inefficient for dense spectra
  # (i.e. when frequency_Hz is long)
  amplitudes <- 10 ^ (excitation_levels / 20)
  x <- rep(0, times = 44000)
  for (i in seq_along(frequency_Hz)) {
    tmp_freq <- round(frequency_Hz[i])
    tmp_amp <- amplitudes[i]
    x[tmp_freq] <- hrep::sum_amplitudes(
      x[tmp_freq],
      tmp_amp,
      coherent = FALSE
    )
  }
  Re(fft(x, inverse = TRUE) / length(x))
}

get_channel_envelope <- function(x) {
  hht::HilbertEnvelope(hht::HilbertTransform(x)) %>%
    (function(x) x - mean(x)) # center it
}

filter_channel_envelope <- function(i, channel_envelope) {
  ft <- fft(channel_envelope)
  ft.coef <- Mod(ft)
  ft.freq <- seq(from = 0, to = 44000 - 1)
  ft.freq <- ifelse(ft.freq >= 22000, # Nyquist frequency
                    44000 - ft.freq,
                    ft.freq)
  ft.coef_new <- ft.coef * envelope_weight(freq_Hz = abs(ft.freq),
                                           channel_num = i)
  waveform <- Re(fft(ft.coef_new, inverse = TRUE) / length(ft.coef_new))
  waveform <- waveform - mean(waveform)
  waveform
}

get_modulation_index <- function(filtered_channel_envelope,
                                 channel_wave_form) {
  sqrt(mean(filtered_channel_envelope ^ 2)) /
    sqrt(mean(channel_wave_form ^ 2))
}

get_phase_impact_factor <- function(i, filtered_channel_envelopes) {
  if (i == 1) {
    cor(
      filtered_channel_envelopes[[1]],
      filtered_channel_envelopes[[2]]
    ) ^ 2
  } else if (i == 47) {
    cor(
      filtered_channel_envelopes[[46]],
      filtered_channel_envelopes[[47]]
    ) ^ 2
  } else {
    cor(
      filtered_channel_envelopes[[i - 1]],
      filtered_channel_envelopes[[i]]
    ) *
      cor(
        filtered_channel_envelopes[[i]],
        filtered_channel_envelopes[[i + 1]]
      )
  }
}

get_channel_weight <- function(channel_num) {
  assertthat::assert_that(
    is.numeric(channel_num),
    all(round(channel_num) == channel_num),
    all(channel_num > 0),
    all(channel_num < 48)
  )
  approx(
    x = c(1,    5,    10,   17,   19,   24, 27,  47),
    y = c(0.41, 0.82, 0.83, 1.12, 1.12, 1,  0.8, 0.58),
    xout = channel_num
  )$y
}

# Get ear transmission coefficients
#
# Gets ear transmission coefficients according to Figure 3 of Wang et al. (2013).
# This is a linear approximation to the graph provided in the paper
# (the paper does not provide an equation for the curve, unfortunately).
# @param freq Numeric vector of frequencies
# @return Numeric vector of ear transmission coefficients.
# These coefficients describe a filter that can be applied to incoming spectra
# to simulate the filtering of the ear.
# \insertRef{Wang2013}{incon}
ear_transmission <- function(freq) {
  log_freq <- log(freq, base = 10)
  ifelse(
    log_freq < 3.1,
    0,
    ifelse(
      log_freq > 4.25,
      30 + (log_freq - 4.25) * 100,
      approx(
        x = c(3.1, 3.4, 3.5, 4, 4.25),
        y = c(0, -5, -5, 5, 30),
        method = "linear", rule = 2,
        xout = log_freq
      )$y
    )
  )
}

convert_Hz_to_bark <- function(freq_Hz) {
  freq_kHz <- freq_Hz / 1000
  ifelse(
    freq_kHz <= 1.5,
    11.82 * atan(1.21 * freq_kHz),
    5 * log(freq_kHz / 1.5) + 12.61
  )
}

get_critical_bandwidth <- function(freq_Hz) {
  ifelse(freq_Hz < 500, 100, freq_Hz / 5)
}

# Figure 5 of Wang et al.
envelope_weight <- function(freq_Hz, channel_num) {
  assertthat::assert_that(
    is.numeric(freq_Hz),
    is.numeric(channel_num), assertthat::is.scalar(channel_num),
    channel_num > 0, channel_num < 48,
    round(channel_num) == channel_num
  )
  if (channel_num < 5) {
    approx2(
      x = c(0, 20,  30, 150,   250, 350),
      y = c(0, 0.8, 1,  0.475, 0.2, 0.02),
      new_x = freq_Hz
    )
  } else if (channel_num < 16) {
    approx2(
      x = c(0, 30, 55, 150, 400),
      y = c(0, 0.8, 1, 0.675, 0.1),
      new_x = freq_Hz
    )
  } else if (channel_num < 21) {
    approx2(
      x = c(0, 50,   77, 165,  250,  400),
      y = c(0, 0.85, 1,  0.82, 0.48, 0.225),
      new_x = freq_Hz
    )
  } else if (channel_num < 42) {
    approx2(
      x = c(0, 50,  77, 100,  250,  400),
      y = c(0, 0.9, 1,  0.95, 0.48, 0.225),
      new_x = freq_Hz
    )
  } else {
    approx2(
      x = c(0, 50,   70, 85,    140,  400),
      y = c(0, 0.95, 1,  0.955, 0.69, 0.225),
      new_x = freq_Hz
    )
  }
}

get_specific_roughness <- function(channel_weight,
                                   phase_impact_factor,
                                   modulation_index,
                                   include_phase_impact_factors) {
  if (include_phase_impact_factors) {
    (channel_weight * phase_impact_factor * modulation_index) ^ 2
  } else {
    (channel_weight * modulation_index) ^ 2
  }
}
