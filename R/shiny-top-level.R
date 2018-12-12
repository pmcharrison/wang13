wang_app <- function() {
  pkg_suggest <- c("cowplot", "shiny", "shinyjs", "shinydashboard")
  purrr::map_lgl(pkg_suggest, requireNamespace) %>%
    {
      if (any(!.)) {
        stop("the following packages need to be installed before continuing: ",
             paste(pkg_suggest[!.], collapse = ", "))
      }
    }

  opt <- list(
    default_chord = hrep::pc_chord(c(0, 4, 7)),
    default_num_harmonics = 11,
    default_include_phase_impact_factors = FALSE,
    fundamental_dB = 60
  )

  ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Wang et al. (2013)"),
    shinydashboard::dashboardSidebar(
      shinyjs::useShinyjs(),
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Input spectrum",
                                 tabName = "input_spectrum",
                                 icon = icon("dashboard")),
        shinydashboard::menuItem("Filtered spectrum",
                                 icon = icon("th"),
                                 tabName = "filtered_spectrum",
                                 badgeLabel = "new", badgeColor = "green"),
        shiny::uiOutput("total_roughness")
      )
    ),
    shinydashboard::dashboardBody(
      shiny::fluidRow(
        shiny::column(
          6,
          shinydashboard::tabItems(
            shinydashboard::tabItem(
              tabName = "input_spectrum",
              shinydashboard::box(
                title = "Input spectrum", status = "primary",
                width = 12,
                shiny::tags$p("This represents the original sound wave."),
                shiny::plotOutput("plot_input_spectrum"),
                shiny::actionButton("play_input_spectrum", "Play")
              )
            ),
            shinydashboard::tabItem(
              tabName = "filtered_spectrum",
              shiny::h2("Filtered spectrum"),
              shiny::tags$p("The sound wave is filtered by the mechanisms of the ear."),
              shiny::plotOutput("plot_filtered_input_spectrum"),
              shiny::actionButton("play_filtered_input_spectrum", "Play")
            )
          )),
        shiny::column(
          6,
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
        )
      )
    )


    # shiny::fluidRow(shinydashboard::box(
    #   title = "Input spectrum", status = "primary",
    #   shiny::tags$p("This represents the original sound wave."),
    #   shiny::plotOutput("plot_input_spectrum"),
    #   shiny::actionButton("play_input_spectrum", "Play")
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Filtered spectrum", status = "primary",
    #   shiny::tags$p("The sound wave is filtered by the mechanisms of the ear."),
    #   shiny::plotOutput("plot_filtered_input_spectrum"),
    #   shiny::actionButton("play_filtered_input_spectrum", "Play")
    # )),

    # shiny::fluidRow(shinydashboard::box(
    #   title = "Channel waveforms", status = "primary",
    #   shiny::tags$p("Different critical bands (a.k.a. channels) are excited in different ways."),
    #   shiny::plotOutput("plot_channel_wave_form"),
    #   shiny::sliderInput(inputId = "channel_wave_forms_channel_num", "Channel number",
    #                      min = 1, max = 47, value = 25, step = 1),
    #   shiny::actionButton("play_channel_wave_form", "Play")
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Channel envelopes", status = "primary",
    #   shiny::tags$p("We extract the envelopes of the excitation patterns for each channel."),
    #   shiny::plotOutput("plot_channel_envelope"),
    #   shiny::sliderInput(inputId = "channel_envelopes_channel_num", "Channel number",
    #                      min = 1, max = 47, value = 25, step = 1)
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Filtered channel envelopes", status = "primary",
    #   shiny::tags$p("These amplitude modulations are filtered to prioritise the modulation frequencies that contribute most towards roughness."),
    #   shiny::plotOutput("plot_filtered_channel_envelope"),
    #   shiny::sliderInput(inputId = "filtered_channel_envelopes_channel_num", "Channel number",
    #               min = 1, max = 47, value = 25, step = 1)
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Modulation indices", status = "primary",
    #   shiny::tags$p("The modulation index of each channel constitutes the RMS amplitude modulation as a fraction of the RMS amplitude of the total waveform."),
    #   shiny::plotOutput("plot_modulation_indices")
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Phase impact factors", status = "primary",
    #   shiny::tags$p("Phase impact factors describe the amount of correlation between the envelope of the current critical band and the envelopes of adjacent critical bands. Higher correlation is thought to yield greater roughness."),
    #   shiny::plotOutput("plot_phase_impact_factors")
    # )),
    # shiny::fluidRow(shinydashboard::box(
    #   title = "Specific roughnesses", status = "primary",
    #   shiny::tags$p("The roughness contribution of each critical band is estimated as a function of modulation index, phase impact factor, and pitch height of the critical band. These are then summed to give the total roughness value."),
    #   shiny::plotOutput("plot_specific_roughnesses")
    # ))
  )


  # Define server logic required to draw a histogram
  server <- function(input, output) {
    state <- shiny::reactiveValues(
      chord = opt$default_chord,
      chord_img_src = get_chord_url(opt$default_chord),
      analysis = analyse_chord(x = opt$default_chord,
                               opt$default_include_phase_impact_factors,
                               opt$fundamental_dB)
    )
    output$current_chord_text <- shiny::renderUI(
      shiny::tags$p("Current chord: ",
                    shiny::tags$strong(as.character(state$chord)))
    )
    output$current_chord_image <- shiny::renderUI(
      shiny::img(src = state$chord_img_src,
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
        state,
        num_harmonics = input$num_harmonics)
    )
    shiny::observe({
      state$analysis <- analyse_chord(x = state$chord,
                                      input$include_phase_impact_factors,
                                      opt$fundamental_dB)
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
        scale_to_other_channels = FALSE
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

    output$total_roughness <- shiny::renderUI(
      shiny::p("Output:",
               shiny::strong(state$analysis$total_roughness %>%
                               round(digits = 5)),
               style = "padding: 15px; font-size: 12pt"))
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
