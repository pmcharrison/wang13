play_channel_wave_forms <- function(channel_wave_forms, channel_num,
                                    scale_to_other_channels = TRUE,
                                    sample_rate = 44e3, bit = 16) {
  if (!loadNamespace("tuneR"))
    stop("tuneR must be installed before continuing")
  peak <- if (scale_to_other_channels) {
    channel_wave_forms %>%
      vapply(function(x) max(abs(x)), numeric(1)) %>%
      max
  } else {
    max(abs(channel_wave_forms[[channel_num]]))
  }
  u <- channel_wave_forms[[channel_num]] %>%
    magrittr::divide_by(peak) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  tuneR::play(tuneR::Wave(u, samp.rate = sample_rate, bit = bit), "play")
}

#' @export
play_wave_form <- function(x, sample_rate = 44e3, bit = 16) {
  if (!loadNamespace("tuneR"))
    stop("tuneR must be installed before continuing")
  peak <- max(abs(x))
  u <- x %>%
    magrittr::divide_by(peak * 1.1) %>%
    magrittr::multiply_by(2 ^ (bit - 1) - 1) %>%
    round
  tuneR::play(tuneR::Wave(u, samp.rate = sample_rate, bit = bit), "play")
}
