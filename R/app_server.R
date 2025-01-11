#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE. HEREEEEEEEEEEE
#' @import shiny
#' @import shinydashboard
#' @import bs4Dash
#' @import shinymanager
#' @importFrom httr POST content_type_json add_headers content
#' @import dplyr
#' @import shinyjs
#' @import stringr
#' @import tidytext
#' @import tokenizers
#' @import ggplot2
#' @import promises
#' @import future
#' @noRd
app_server <- function(input, output, session) {

  # Get translations once at server initialization
  translations <- get_translations()
  # get api
  api_pwd <- Sys.getenv("API_KEY")

  # Authentication
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(
      db = "anotherworkhubusers.sqlite",
      passphrase = Sys.getenv("shinymanagerauth")
    ),
    timeout = 600
  )

  # Language handling
  language_input <- reactive({ input$language })

  # Dynamic title
  output$dynamic_title <- renderUI({
    if (input$language == "ENG") {
      shiny::h1("Welcome!")
    } else {
      shiny::h1("Bienvenue !")
    }
  })

  # Logo
  output$app_logo <- renderUI({
    tags$img(
      src = "www/awh_logo.png",
      height = "auto",
      width = "100%"
    )
  })

  # Sidebar menu
  output$sidebar_menu <- renderUI({
    req(input$language)
    shinydashboard::sidebarMenu(
      id = "main_tabs",
      fixed = TRUE,
      if (input$language == "ENG") {
        shinydashboard::menuItem(
          "Simulate an interview",
          tabName = "tab_interview_simulator",
          icon = icon("id-card")
        )
      } else {
        shinydashboard::menuItem(
          "Simuler un entretien",
          tabName = "tab_interview_simulator",
          icon = icon("id-card")
        )
      },
      if (input$language == "ENG") {
        shinydashboard::menuItem(
          "Improve my pitch (written/spoken)",
          tabName = "tab_pitch_improver",
          icon = icon("sliders")
        )
      } else {
        shinydashboard::menuItem(
          "Améliorer mon pitch (écrit/parlé)",
          tabName = "tab_pitch_improver",
          icon = icon("sliders")
        )
      }
    )
  })

  # Module UI outputs
  output$interview_content <- renderUI({
    mod_section_interview_simulator_ui("section_interview_simulator_1")
  })

  output$pitch_content <- renderUI({
    mod_section_pitch_improver_ui("section_pitch_improver_1")
  })

  # Initialize modules
  mod_section_interview_simulator_server(
    "section_interview_simulator_1",
    api_pwd = api_pwd,
    language_input = language_input
  )

  mod_section_pitch_improver_server("section_pitch_improver_1", api_pwd, language_input, translations)



  # Button click handlers
  observeEvent(input$btn_interview, {
    updateTabItems(session, "main_tabs", "tab_interview_simulator")
    shinyjs::show("interview_section")
    shinyjs::hide("pitch_section")
  })

  observeEvent(input$btn_pitch, {
    updateTabItems(session, "main_tabs", "tab_pitch_improver")
    shinyjs::hide("interview_section")
    shinyjs::show("pitch_section")
  })
}
