#' Wang et al. (2013) interactive demo
#'
#' Launches an interactive demo of Wang et al.'s (2013) roughness model.
#' This function requires a few additional packages to be installed;
#' you will be notified if any of these packages are missing
#' once you run \code{demo_wang()}.
#' @param audio (Scalar logical) Whether to enable playback controls
#' (currently the playback controls don't work when the app is hosted
#' on a remote server).
#' @note The demo takes the form of an Shiny app
#' (\url{https://shiny.rstudio.com/}).
#' @references
#' \insertRef{Wang2013}{wang13}
#' @export
demo_wang <- function(audio = TRUE) {
  pkg_suggest <- c("cowplot", "shiny", "shinyjs", "shinydashboard")
  purrr::map_lgl(pkg_suggest, requireNamespace) %>%
    {
      if (any(!.)) {
        stop("the following packages need to be installed before continuing: ",
             paste(pkg_suggest[!.], collapse = ", "))
      }
    }

  opt <- list(
    default_chord = hrep::pc_chord(c(4, 0, 7)),
    default_num_harmonics = 11,
    default_include_phase_impact_factors = FALSE,
    fundamental_dB = 60,
    audio = audio
  )

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Wang et al. (2013)",
                                    disable = FALSE),
    shiny_ui_sidebar(),
    shiny_ui_body(opt)
  )

  server <- function(input, output) {
    state <- shiny::reactiveValues(
      chord = opt$default_chord,
      analysis = NULL
    )
    output$current_chord_text <- shiny::renderUI(
      shiny::tags$p("Current chord: ",
                    shiny::tags$strong(as.character(state$chord)))
    )
    output$current_chord_image <- shiny::renderUI(
      shiny::img(src = get_chord_url(state$chord),
                 contentType = 'image/png',
                 alt = "Current chord",
                 style = paste("max-width: 150px;",
                               "border-style: solid;",
                               "border-width: 5px;",
                               "border-color: white"))
    )
    shiny::observeEvent(
      input$enter_new_chord,
      enter_new_chord(
        input$chord,
        state)
    )
    shiny::observe({
      state$analysis <- analyse_chord(x = state$chord,
                                      input$include_phase_impact_factors,
                                      opt$fundamental_dB,
                                      input$num_harmonics)
    })
    shiny::observeEvent(input$play_input_spectrum, {
      play_input_spectrum(state$analysis)
    })
    shiny::observeEvent(input$play_filtered_input_spectrum, {
      play_filtered_input_spectrum(state$analysis)
    })
    shiny::observeEvent(input$play_channel_wave_form, {
      play_channel_wave_forms(
        channel_wave_forms = state$analysis$channel_wave_forms,
        channel_num = input$channel_wave_forms_channel_num,
        scale_to_other_channels = input$normalise_volume_across_channels
      )
    })

    output$plot_input_spectrum <- shiny::renderPlot(
      plot_input_spectrum(state$analysis))

    output$plot_filtered_input_spectrum <- shiny::renderPlot(
      plot_filtered_input_spectrum(state$analysis))

    output$plot_channel_wave_form <- shiny::renderPlot(
      plot_waveform(
        state$analysis$channel_wave_forms[[input$channel_wave_forms_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_channel_envelope <- shiny::renderPlot(
      plot_waveform(
        state$analysis$channel_envelopes[[input$channel_envelopes_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_filtered_channel_envelope <- shiny::renderPlot(
      plot_waveform(
        state$analysis$filtered_channel_envelopes[[input$filtered_channel_envelopes_channel_num]],
        range_sec = c(0, 0.05)
      ))
    output$plot_modulation_indices <- shiny::renderPlot({
      plot_modulation_indices_wang(state$analysis$modulation_indices)
    })

    output$plot_phase_impact_factors <- shiny::renderPlot(
      plot_phase_impact_factors_wang(state$analysis$phase_impact_factors))

    output$plot_specific_roughnesses <- shiny::renderPlot(
      plot_specific_roughnesses_wang(state$analysis$specific_roughnesses))

    output$total_roughness <- shiny::renderUI({
      x <- round(state$analysis$total_roughness, digits = 5)
      shiny::showNotification(shiny::p("Roughness:", shiny::strong(x)),
                              type = "message",
                              duration = 120)
      shiny::p("Output:",
               shiny::strong(x),
               style = "padding: 15px; font-size: 12pt")
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
