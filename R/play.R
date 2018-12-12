play_channel_wave_forms <- function(channel_wave_forms,
                                    channel_num,
                                    scale_to_other_channels = TRUE,
                                    ...) {
  if (!requireNamespace("tuneR"))
    stop("tuneR must be installed before continuing")
  peak <- if (scale_to_other_channels) {
    channel_wave_forms %>%
      vapply(function(x) max(abs(x)), numeric(1)) %>%
      max
  } else {
    max(abs(channel_wave_forms[[channel_num]]))
  }
  play_wave_form(x = channel_wave_forms[[channel_num]],
                 peak = peak,
                 ...)
}


play_wave_form <- function(x,
                           sample_rate = 44e3,
                           fade_samples = 1e3,
                           bit = 16,
                           peak = max(abs(x))) {
  if (!requireNamespace("tuneR"))
    stop("tuneR must be installed before continuing")

  if (length(x) > 2 * fade_samples) {
    ind_1 <- seq(from = 1, length.out = fade_samples)
    ind_2 <- seq(to = length(x), length.out = fade_samples)
    x[ind_1] <- x[ind_1] * seq(from = 0, to = 1, length.out = fade_samples)
    x[ind_2] <- x[ind_2] * seq(from = 1, to = 0, length.out = fade_samples)
  }

  u <- x %>%
    magrittr::divide_by(peak * 1.5) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  tuneR::play(tuneR::Wave(u, samp.rate = sample_rate, bit = bit), "play")
}


play_sparse_spectrum <- function(
  frequency,
  amplitude,
  seconds = 1,
  sample_rate = 44e3,
  ...
) {
  sparse_spectrum_to_waveform(frequency, amplitude, seconds, sample_rate) %>%
  play_wave_form(sample_rate = sample_rate, ...)
}

# Note: this could be done more efficiently with ifft
sparse_spectrum_to_waveform <- function(
  frequency,
  amplitude,
  seconds,
  sample_rate
) {
  x <- seq(from = 0, to = seconds, length.out = sample_rate * seconds)
  y <- mapply(
    function(freq, amplitude) {
      sin(2 * pi * freq * x) * amplitude
    },
    frequency,
    amplitude
  ) %>% rowSums
}
