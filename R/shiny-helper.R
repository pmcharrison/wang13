format_chord <- function(x) {
  paste(x, collapse = " ")
}

# draw_chord <- function(x, sharps = TRUE, dpi = 200) {
#   files <- list(
#     lily = tempfile(fileext = ".ly"),
#     png = tempfile(fileext = ".png")
#   )
#   seq <- lilypondR::make_chord_sequence(list(x), sharps = sharps)
#   lilypondR::write_lilypond(seq, path = files$lily)
#   lilypondR::convert_ly_to_png(ly_path = files$lily,
#                                png_path_sans_ext = gsub("\\.png", "", files$png),
#                                dpi = 200)
#   files$png
# }

enter_new_chord <- function(text, state, num_harmonics) {
  tryCatch({
    set_chord(hrep::pc_chord(text),
              state,
              num_harmonics)
  }, error = function(e){
    message("Call: ", capture.output(e$call))
    message("Message: ", e$message)
    shinyjs::alert("Invalid chord")
  })
}

set_chord <- function(chord, state, num_harmonics) {
  state$chord <- chord
  state$chord_img_src <- get_chord_url(chord) %>% print
}

plot_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_input$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(analysis$spectrum_input$level_dB, 60)
  plot_sparse_spectrum(frequency, amplitude, range_Hz = NULL)
}

play_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_input$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(analysis$spectrum_input$level_dB, 60)
  play_sparse_spectrum(frequency, amplitude)
}

play_filtered_input_spectrum <- function(analysis) {
  frequency <- analysis$spectrum_after_ear_filtering$frequency_Hz
  amplitude <- hrep::dB_to_amplitude(
    analysis$spectrum_after_ear_filtering$level_dB, 60
  )
  play_sparse_spectrum(frequency, amplitude)
}

get_chord_url <- function(chord, type = "png") {
  assertthat::assert_that(
    assertthat::is.scalar(type),
    is.character(type),
    type %in% c("png", "mp3", "midi")
  )
  pitch <- as.integer(hrep::pi_chord(chord))
  label <- paste(pitch, collapse = "_")
  src <- "http://research.pmcharrison.com/studies/HarmonyDissonance/chords/piano/%s/%s.%s" %>%
    sprintf(label, label, type)
  src
}

analyse_chord <- function(x,
                          include_phase_impact_factors,
                          fundamental_dB) {
  spectrum <- hrep::fr_sparse_spectrum(hrep::pi_chord(x))
  roughness_wang(
    frequency_Hz = hrep::freq(spectrum),
    level_dB = hrep::amplitude_to_dB(hrep::amp(spectrum), fundamental_dB),
    include_phase_impact_factors = include_phase_impact_factors,
    detail = TRUE
  )
}
