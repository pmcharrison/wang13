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
              state)
  }, error = function(e){
    message("Call: ", capture.output(e$call))
    message("Message: ", e$message)
    shinyjs::alert("Invalid chord")
  })
}

set_chord <- function(chord, state) {
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
                          fundamental_dB,
                          num_harmonics) {
  spectrum <- hrep::fr_sparse_spectrum(hrep::pi_chord(x),
                                       num_harmonics = num_harmonics)

  shiny::withProgress(
    roughness_wang(
      frequency_Hz = hrep::freq(spectrum),
      level_dB = hrep::amplitude_to_dB(hrep::amp(spectrum), fundamental_dB),
      include_phase_impact_factors = include_phase_impact_factors,
      detail = TRUE,
      msg = function(n, N, msg) shiny::setProgress(n, msg)
    ),
    min = 0, max = 7
  )
}

shiny_ui_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shinyjs::useShinyjs(),
    shinydashboard::sidebarMenu(
      lapply(c(
        "Input spectrum",
        "Filtered spectrum",
        "Channel waveforms",
        "Channel envelopes",
        "Filtered channel envelopes",
        "Modulation indices",
        "Phase impact factors",
        "Specific roughnesses"
      ), function(title) {
        shinydashboard::menuItem(title,
                                 tabName = gsub(" ", "_", tolower(title)),
                                 icon = NULL)
      }),
      shiny::uiOutput("total_roughness")
    )
  )
}

shiny_ui_body <- function(opt) {
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(6, shiny_ui_tabs()),
      shiny::column(6, shiny_ui_input(opt))
    )
  )
}

shiny_ui_tabs <- function() {
  shinydashboard::tabItems(
    shiny_ui_tab_1(),
    shiny_ui_tab_2(),
    shiny_ui_tab_3(),
    shiny_ui_tab_4(),
    shiny_ui_tab_5(),
    shiny_ui_tab_6(),
    shiny_ui_tab_7(),
    shiny_ui_tab_8()
  )
}

shiny_ui_tab_1 <- function() {
  shinydashboard::tabItem(
    tabName = "input_spectrum",
    shinydashboard::box(
      title = "Input spectrum",
      status = "primary",
      width = 12,
      shiny::tags$p("This represents the original sound wave."),
      shiny::div(shiny::plotOutput("plot_input_spectrum"),
                 style = "max-width: 100%"),
      shiny::actionButton("play_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_2 <- function() {
  shinydashboard::tabItem(
    tabName = "filtered_spectrum",
    shinydashboard::box(
      title = "Filtered spectrum",
      status = "primary",
      width = 12,
      shiny::tags$p("The sound wave is filtered by the mechanisms of the ear."),
      shiny::plotOutput("plot_filtered_input_spectrum"),
      shiny::actionButton("play_filtered_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_3 <- function() {
  shinydashboard::tabItem(
    tabName = "channel_waveforms",
    shinydashboard::box(
      title = "Channel waveforms",
      status = "primary",
      width = 12,
      shiny::tags$p("Different critical bands (a.k.a. channels)",
                    "are excited in different ways."),
      shiny::plotOutput("plot_channel_wave_form"),
      shiny::sliderInput("channel_wave_forms_channel_num",
                         "Channel number",
                         min = 1, max = 47, value = 25, step = 1),
      shiny::actionButton("play_channel_wave_form", "Play")
    ))
}

shiny_ui_tab_4 <- function() {
  shinydashboard::tabItem(
    tabName = "channel_envelopes",
    shinydashboard::box(
      title = "Channel envelopes",
      status = "primary",
      width = 12,
      shiny::tags$p("We extract the envelopes of the excitation patterns",
                    "for each channel."),
      shiny::plotOutput("plot_channel_envelope"),
      shiny::sliderInput("channel_envelopes_channel_num", "Channel number",
                         min = 1, max = 47, value = 25, step = 1)
    ))
}

shiny_ui_tab_5 <- function() {
  shinydashboard::tabItem(
    tabName = "filtered_channel_envelopes",
    shinydashboard::box(
      title = "Filtered channel envelopes",
      status = "primary",
      width = 12,
      shiny::tags$p("These amplitude modulations are filtered to prioritise",
                    "the modulation frequencies that contribute most towards roughness."),
      shiny::plotOutput("plot_filtered_channel_envelope"),
      shiny::sliderInput("filtered_channel_envelopes_channel_num", "Channel number",
                         min = 1, max = 47, value = 25, step = 1)
    ))
}

shiny_ui_tab_6 <- function() {
  shinydashboard::tabItem(
    tabName = "modulation_indices",
    shinydashboard::box(
      title = "Modulation indices",
      status = "primary",
      width = 12,
      shiny::tags$p("The modulation index of each channel constitutes",
                    "the RMS amplitude modulation as a fraction of the",
                    "RMS amplitude of the total waveform."),
      shiny::plotOutput("plot_modulation_indices")
    ))
}

shiny_ui_tab_7 <- function() {
  shinydashboard::tabItem(
    tabName = "phase_impact_factors",
    shinydashboard::box(
      title = "Phase impact factors",
      status = "primary",
      width = 12,
      shiny::tags$p("Phase impact factors describe the amount of correlation",
                    "between the envelope of the current critical band and the",
                    "envelopes of adjacent critical bands. Higher correlation",
                    "is thought to yield greater roughness."),
      shiny::plotOutput("plot_phase_impact_factors")
    ))
}

shiny_ui_tab_8 <- function() {
  shinydashboard::tabItem(
    tabName = "specific_roughnesses",
    shinydashboard::box(
      title = "Specific roughnesses",
      status = "primary",
      width = 12,
      shiny::tags$p("The roughness contribution of each critical band is",
                    "estimated as a function of modulation index,",
                    "phase impact factor, and pitch height of the critical band.",
                    "These are then summed to give the total roughness value."),
      shiny::plotOutput("plot_specific_roughnesses")
    ))
}


shiny_ui_input <- function(opt) {
  shinydashboard::box(
    shiny::textInput("chord", label = "Chord", placeholder = "e.g. 60 64 67"),
    shiny::actionButton("enter_new_chord", "Enter"),
    shiny::uiOutput("current_chord_text"),
    shiny::uiOutput("current_chord_image", height = "auto"),
    shiny::checkboxInput("include_phase_impact_factors",
                         "Include phase impact factors",
                         value = opt$default_include_phase_impact_factors),
    shiny::sliderInput("num_harmonics", "Number of harmonics",
                       value = opt$default_num_harmonics,
                       min = 1, max = 15, step = 1),
    style = "text-align: center"
  )
}
