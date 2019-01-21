format_chord <- function(x) {
  paste(x, collapse = " ")
}

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
  state$chord_img_src <- get_chord_url(chord)
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
  spectrum <- hrep::sparse_fr_spectrum(hrep::pi_chord(x),
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
    shiny::h4("Wang et al. (2013)",
              style = "padding-left: 20px"),
    shinydashboard::sidebarMenu(
      lapply(c(
        "About",
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
      shiny::column(6, shiny_ui_tabs(opt)),
      shiny::column(6, shiny_ui_input(opt))
    )
  )
}

shiny_ui_tabs <- function(opt) {
  shinydashboard::tabItems(
    shiny_ui_tab_0(),
    shiny_ui_tab_1(opt),
    shiny_ui_tab_2(opt),
    shiny_ui_tab_3(opt),
    shiny_ui_tab_4(),
    shiny_ui_tab_5(),
    shiny_ui_tab_6(),
    shiny_ui_tab_7(),
    shiny_ui_tab_8()
  )
}

shiny_ui_tab_0 <- function() {
  shinydashboard::tabItem(
    tabName = "about",
    shinydashboard::box(
      # title = "Input spectrum",
      title = "About",
      status = "primary",
      width = 12,
      shiny::tags$p("This interactive app analyses the roughness of musical chords",
                    "using Wang et al.'s (2013) algorithm.",
                    "Use the tabs on the left-hand side of the screen",
                    "to navigate through the successive stages of the model."),
      shiny::tags$p("Wang, Y. S., Shen, G. Q., Guo, H., Tang, X. L., & Hamade, T. (2013).",
                    "Roughness modelling based on human auditory perception for",
                    "sound quality evaluation of vehicle interior noise.",
                    shiny::tags$em("Journal of Sound and Vibration"),
                    "332(16), 3893-3904.",
                    shiny::tags$a(href = "https://doi.org/10.1016/j.jsv.2013.02.030")
                    )
    )
  )
}

shiny_ui_tab_1 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "input_spectrum",
    shinydashboard::box(
      # title = "Input spectrum",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The input to the model is an acoustic spectrum."),
      shiny::div(shiny::plotOutput("plot_input_spectrum"),
                 style = "max-width: 100%"),
      if (opt$audio) shiny::actionButton("play_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_2 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "filtered_spectrum",
    shinydashboard::box(
      # title = "Filtered spectrum",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The spectrum is filtered by the outer and middle ear."),
      shiny::plotOutput("plot_filtered_input_spectrum"),
      if (opt$audio) shiny::actionButton("play_filtered_input_spectrum", "Play")
    )
  )
}

shiny_ui_tab_3 <- function(opt) {
  shinydashboard::tabItem(
    tabName = "channel_waveforms",
    shinydashboard::box(
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Different cochlear channels selectively filter",
                    "for different frequency ranges."),
      shiny::plotOutput("plot_channel_wave_form"),
      shiny::sliderInput("channel_wave_forms_channel_num",
                         "Channel number",
                         min = 1, max = 47, value = 25, step = 1),
      if (opt$audio) shiny::fluidRow(
        shiny::column(3, shiny::actionButton("play_channel_wave_form", "Play",
                                             style = "text-align: center")),
        shiny::column(9, shiny::checkboxInput("normalise_volume_across_channels",
                                              "Normalise volume across channels?",
                                              value = TRUE))
      )
    ))
}

shiny_ui_tab_4 <- function() {
  shinydashboard::tabItem(
    tabName = "channel_envelopes",
    shinydashboard::box(
      # title = "Channel envelopes",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Waveform envelopes are extracted",
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
      # title = "Filtered channel envelopes",
      title = NULL,
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
      # title = "Modulation indices",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("The modulation index captures the magnitude of",
                    "amplitude modulation for each channel."),
      shiny::plotOutput("plot_modulation_indices")
    ))
}

shiny_ui_tab_7 <- function() {
  shinydashboard::tabItem(
    tabName = "phase_impact_factors",
    shinydashboard::box(
      # title = "Phase impact factors",
      title = NULL,
      status = "primary",
      width = 12,
      shiny::tags$p("Phase impact factors capture the correlation between",
                    "the envelopes of adjacent channels.",
                    "According to Wang et al. (2013), higher correlations",
                    "yield greater roughness."),
      shiny::plotOutput("plot_phase_impact_factors")
    ))
}

shiny_ui_tab_8 <- function() {
  shinydashboard::tabItem(
    tabName = "specific_roughnesses",
    shinydashboard::box(
      # title = "Specific roughnesses",
      title = NULL,
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
    shiny::p("Enter a pitch-class set to analyse.",
             "The first pitch class will be taken as the bass note."),
    shiny::textInput("chord", label = NULL, placeholder = "e.g. 4 0 7"),
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
