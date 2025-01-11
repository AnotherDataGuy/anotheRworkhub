#' Interview Simulator Module
#'
#' @description A Shiny module for conducting simulated interviews
#' @param id The module ID
#' @param api_pwd API password
#' @param language_input Reactive expression for language selection
#'
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import httr
#' @import jsonlite
#'
#' @noRd

# UI Function ---------------------------------------------------------------

mod_section_interview_simulator_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = ns("tab_interview_simulator"),
    style = "margin: 2vw;",
    shinyjs::useShinyjs(),

    tags$head(
      tags$style(HTML("
        .box.box-solid.box-primary {
          border: none;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        .main-header {
          margin-bottom: 20px;
        }
        .form-group {
          margin-bottom: 20px;
        }
        .nav-tabs-custom {
          box-shadow: none;
        }
        .tab-content {
          padding: 20px 0;
        }
        .info-box {
          min-height: 75px;
          border-radius: 4px;
        }
        .info-box-icon {
          height: 75px;
          line-height: 75px;
          border-top-left-radius: 4px;
          border-bottom-left-radius: 4px;
        }
      "))
    ),

    # Main Header
    uiOutput(ns("main_header")),


    # Basic Information Section
    uiOutput(ns("basic_information")),

    # Interview Details Tabs
    uiOutput(ns("interview_details")),

    # Start Interview Button
    uiOutput(ns("start_interview")),


    uiOutput(ns("restart_interview")),

    # Chat Interface
    uiOutput(ns("user_text_and_messages"))
  )
}

#' section_interview_simulator Server Functions
#'
#' @noRd
mod_section_interview_simulator_server <- function(id, api_pwd, language_input) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    rv <- reactiveValues(
      thread_id = NULL,
      api_calls = 0,
      step = 1,
      config = list(
        api_key = api_pwd,
        assistant_id = "asst_uwNiLhjFqVlSD7UjD6bLcv6B",
        timeout_seconds = 60
      )
    )

    # Accessor Function
    translations <- list(
      start_interview = list(
        ENG = "Start Interview",
        FR = "Démarrer l'entretien"
      ),
      restart_interview = list(     # Add this
        ENG = "Restart Interview",
        FR = "Redémarrer l'entretien"
      ),
      refresh = list(
        ENG = "Refresh",
        FR = "Actualiser"
      ),
      labels = list(
        candidate_name = list(ENG = "Candidate Name", FR = "Nom du candidat"),
        position = list(ENG = "Position", FR = "Poste"),
        settings_title = list(ENG = "Interview Settings", FR = "Paramètres d'entretien"),
        basic_settings_title = list(ENG = "Basic Information", FR = "Informations de base"),
        interview_format_title = list(ENG = "Interview Format", FR = "Format d'entretien"),
        company_info_title = list(ENG = "Company Information", FR = "Information sur l'entreprise"),
        job_details_title = list(ENG = "Job Details", FR = "Détails du poste"),
        format_label = list(ENG = "Interview Format", FR = "Format d'entretien"),
        criteria_label = list(ENG = "Assessment Criteria", FR = "Critères d'évaluation"),
        time_label = list(ENG = "Time Constraints", FR = "Contraintes de temps"),
        cultural_fit_label = list(ENG = "Cultural Fit Focus", FR = "Focus sur l'adéquation culturelle"),
        follow_up_label = list(ENG = "Follow-up Process", FR = "Processus de suivi"),
        culture_label = list(ENG = "Company Culture", FR = "Culture d'entreprise"),
        values_label = list(ENG = "Company Values", FR = "Valeurs de l'entreprise"),
        challenges_label = list(ENG = "Business Challenges", FR = "Défis commerciaux"),
        growth_label = list(ENG = "Growth Phase", FR = "Phase de croissance"),
        competitors_label = list(ENG = "Main Competitors", FR = "Principaux concurrents"),
        reputation_label = list(ENG = "Company Reputation", FR = "Réputation de l'entreprise"),
        environment_label = list(ENG = "Work Environment", FR = "Environnement de travail"),
        responsibilities_label = list(ENG = "Key Responsibilities", FR = "Responsabilités principales"),
        skills_label = list(ENG = "Required Skills", FR = "Compétences requises"),
        team_label = list(ENG = "Team Structure", FR = "Structure de l'équipe"),
        job_challenges_label = list(ENG = "Key Challenges", FR = "Défis principaux"),
        performance_label = list(ENG = "Performance Expectations", FR = "Attentes de performance"),
        career_label = list(ENG = "Career Development", FR = "Développement de carrière")
      ),
      placeholders = list(
        name = list(ENG = "Enter candidate's name...", FR = "Entrez le nom du candidat..."),
        job_title = list(ENG = "Enter job title...", FR = "Entrez l'intitulé du poste..."),
        context = list(ENG = "Enter context or background...", FR = "Entrez le contexte ou l'arrière-plan...")
      ),
      choices = list(
        list(
          company_size = list(
            ENG = c("", "Very small (1-10)", "Small (11-50)", "Medium (51-250)", "Large (251+)"),
            FR = c("", "Très petite (1-10)", "Petite (11-50)", "Moyenne (51-250)", "Grande (251+)")
          ),
          interview_format = list(
            ENG = c("", "First interview", "Follow-up interview", "Case Study", "Technical"),
            FR = c("", "Premier interview", "Entretien de suivi", "Étude de cas", "Technique")
          ),
          company_sector = list(
            ENG = c(
              "Agriculture, Forestry, and Fishing",
              "Mining and Quarrying",
              "Manufacturing",
              "Electricity, Gas, Steam, and Air Conditioning Supply",
              "Water Supply, Sewerage, Waste Management, and Remediation Activities",
              "Construction",
              "Wholesale and Retail Trade; Repair of Motor Vehicles and Motorcycles",
              "Transportation and Storage",
              "Accommodation and Food Service Activities",
              "Information and Communication",
              "Financial and Insurance Activities",
              "Real Estate Activities",
              "Professional, Scientific, and Technical Activities",
              "Administrative and Support Service Activities",
              "Public Administration and Defence; Compulsory Social Security",
              "Education",
              "Human Health and Social Work Activities",
              "Arts, Entertainment, and Recreation",
              "Other Service Activities"
            ),
            FR = c(
              "Agriculture, Sylviculture et Pêche",
              "Extraction Minière et Carrières",
              "Industrie Manufacturière",
              "Production et Distribution d'Électricité, de Gaz, de Vapeur et de Climatisation",
              "Captage, Traitement et Distribution d'Eau; Assainissement, Gestion des Déchets",
              "Construction",
              "Commerce de Gros et de Détail; Réparation de Véhicules Automobiles et de Motocycles",
              "Transport et Entreposage",
              "Hébergement et Services de Restauration",
              "Information et Communication",
              "Activités Financières et d'Assurance",
              "Activités Immobilières",
              "Activités Professionnelles, Scientifiques et Techniques",
              "Activités de Services Administratifs et de Soutien",
              "Administration Publique et Défense; Sécurité Sociale Obligatoire",
              "Éducation",
              "Activités de Santé Humaine et d'Action Sociale",
              "Arts, Spectacles et Activités Récréatives",
              "Autres Activités de Services"
            )
          )
        ),
        interview_format = list(
          ENG = c("", "First interview", "Follow-up interview", "Case Study", "Technical"),
          FR = c("", "Premier interview", "Entretien de suivi", "Étude de cas", "Technique")
        ),
        assessment_criteria = list(
          ENG = c("", "Technical Skills", "Soft Skills", "Cultural Fit", "Leadership", "Problem Solving"),
          FR = c("", "Compétences techniques", "Compétences relationnelles", "Adéquation culturelle", "Leadership", "Résolution de problèmes")
        ),
        time_constraints = list(
          ENG = c("", "30 minutes", "45 minutes", "60 minutes", "90 minutes"),
          FR = c("", "30 minutes", "45 minutes", "60 minutes", "90 minutes")
        ),
        cultural_fit_focus = list(
          ENG = c("", "High", "Moderate", "Low"),
          FR = c("", "Élevé", "Modéré", "Faible")
        ),
        follow_up_process = list(
          ENG = c("", "Same Day", "Within Week", "Multiple Rounds"),
          FR = c("", "Même jour", "Dans la semaine", "Plusieurs tours")
        ),
        company_culture = list(
          ENG = c("", "Formal", "Casual", "Startup", "Corporate"),
          FR = c("", "Formel", "Décontracté", "Startup", "Entreprise")
        ),
        growth_phase = list(
          ENG = c("", "Startup", "Growth", "Mature", "Transformation"),
          FR = c("", "Démarrage", "Croissance", "Mature", "Transformation")
        ),
        work_environment = list(
          ENG = c("", "Remote", "Hybrid", "Office-based", "Flexible"),
          FR = c("", "Télétravail", "Hybride", "Présentiel", "Flexible")
        )
      )
    )

    output$restart_interview <- renderUI({
      div(
        class = "text-center",
        style = "margin: 20px 0;",
        actionButton(
          ns("restart_interview"),
          translations$restart_interview[[language_input()]],
          icon = icon("sync"),
          class = "btn-lg btn-warning",
          style = "padding: 10px 30px; font-size: 18px;"
        )
      )
    })

    observeEvent(input$restart_interview, {
      # Reset reactive values
      rv$thread_id <- NULL
      rv$api_calls <- 0
      rv$step <- 1

      # Show start button, hide input area
      shinyjs::show("start_interview")
      shinyjs::hide("input_area")

      # Clear chat UI
      output$chat_ui <- renderUI({
        NULL
      })
    })


    # Accessor function for translations
    get_translation <- function(type, key, language) {
      translations[[type]][[key]][[language]]
    }




    # Helper functions specific to thread-based operations
    helpers <- list(
      getTimestamp = \() format(Sys.time(), "%Y-%m-%d %H:%M:%S"),

      applyCooldown = \() {
        shinyjs::disable(ns("send_message"))
        shinyjs::delay(10000, shinyjs::enable(ns("send_message")))
      },

      validateInputs = function() {
        req(input$name, input$job_title, input$company_sector)
        if (nchar(input$name) < 2 || nchar(input$job_title) < 2) {
          showNotification(
            if(language_input() == "ENG")
              "Please fill in all required fields properly"
            else
              "Veuillez remplir tous les champs requis correctement",
            type = "warning"
          )
          return(FALSE)
        }
        return(TRUE)
      },

      formatInitialContext = function() {
        # Helper function to check if a value exists, is not empty, and is not the default empty selection
        has_value <- function(x) {
          !is.null(x) && !is.na(x) && x != "" && nchar(trimws(x)) > 0
        }

        # Format single value with label
        format_value <- function(value, label_key) {
          label <- get_translation("labels", label_key, language_input())
          return(sprintf("%s: %s", label, value))
        }

        # Collect all filled values
        filled_values <- character(0)

        # Basic Information - each field separately to ensure capture
        if (has_value(input$name)) {
          filled_values <- c(filled_values, format_value(input$name, "candidate_name"))
        }
        if (has_value(input$job_title)) {
          filled_values <- c(filled_values, format_value(input$job_title, "position"))
        }
        if (has_value(input$company_sector)) {
          filled_values <- c(filled_values, sprintf("Secteur d'activité: %s", input$company_sector))
        }

        # Interview Format section
        if (has_value(input$interview_format)) {
          filled_values <- c(filled_values, format_value(input$interview_format, "format_label"))
        }
        if (has_value(input$assessment_criteria)) {
          filled_values <- c(filled_values, format_value(input$assessment_criteria, "criteria_label"))
        }
        if (has_value(input$time_constraints)) {
          filled_values <- c(filled_values, format_value(input$time_constraints, "time_label"))
        }
        if (has_value(input$cultural_fit_focus)) {
          filled_values <- c(filled_values, format_value(input$cultural_fit_focus, "cultural_fit_label"))
        }
        if (has_value(input$follow_up_process)) {
          filled_values <- c(filled_values, format_value(input$follow_up_process, "follow_up_label"))
        }

        # Company Information section
        if (has_value(input$company_culture)) {
          filled_values <- c(filled_values, format_value(input$company_culture, "culture_label"))
        }
        if (has_value(input$company_values)) {
          filled_values <- c(filled_values, format_value(input$company_values, "values_label"))
        }
        if (has_value(input$company_challenges)) {
          filled_values <- c(filled_values, format_value(input$company_challenges, "challenges_label"))
        }
        if (has_value(input$growth_phase)) {
          filled_values <- c(filled_values, format_value(input$growth_phase, "growth_label"))
        }
        if (has_value(input$company_reputation)) {
          filled_values <- c(filled_values, format_value(input$company_reputation, "reputation_label"))
        }
        if (has_value(input$work_environment)) {
          filled_values <- c(filled_values, format_value(input$work_environment, "environment_label"))
        }

        # Job Details section
        if (has_value(input$responsibilities)) {
          filled_values <- c(filled_values, format_value(input$responsibilities, "responsibilities_label"))
        }
        if (has_value(input$skills)) {
          filled_values <- c(filled_values, format_value(input$skills, "skills_label"))
        }
        if (has_value(input$team)) {
          filled_values <- c(filled_values, format_value(input$team, "team_label"))
        }
        if (has_value(input$job_challenges)) {
          filled_values <- c(filled_values, format_value(input$job_challenges, "job_challenges_label"))
        }
        if (has_value(input$performance)) {
          filled_values <- c(filled_values, format_value(input$performance, "performance_label"))
        }
        if (has_value(input$career)) {
          filled_values <- c(filled_values, format_value(input$career, "career_label"))
        }

        # Combine all values
        if (length(filled_values) > 0) {
          return(paste(filled_values, collapse = "\n"))
        }
        return("")
      }
    )


    output$main_header <- renderUI({
      fluidRow(
        column(
          width = 12,
          div(
            class = "main-header",
            h2(
              icon("user-tie"),
              if (language_input() == "ENG") {
                "Interview Simulator"
              } else {
                "Simulateur d'entretien"
              },
              style = "margin-top: 0;"
            )
          )
        )
      )
    })


    output$name_input <- renderUI({
      textInput(ns("name"), get_translation("labels", "candidate_name", language_input()))
    })

    output$job_title_input <- renderUI({
      textInput(ns("job_title"), get_translation("labels", "position", language_input()))
    })

    output$company_sector_ui_set <- renderUI({
      selectInput(
        ns("company_sector"),
        "Secteur d'activité",
        choices = translations$choices[[1]]$company_sector[[language_input()]],
        width = "100%"
      )
    })



    # Basic Information Box
    output$basic_information <- renderUI({
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            title = tagList(icon("info-circle"), get_translation("labels", "basic_settings_title", language_input())),
            status = "primary",
            solidHeader = TRUE,

            fluidRow(
              column(
                width = 4,
                div(class = "form-group",
                    tags$label(icon("user"), get_translation("labels", "candidate_name", language_input())),
                    textInput(ns("name"), NULL,
                              placeholder = get_translation("placeholders", "name", language_input()))
                )
              ),
              column(
                width = 4,
                div(class = "form-group",
                    tags$label(icon("briefcase"), get_translation("labels", "position", language_input())),
                    textInput(ns("job_title"), NULL,
                              placeholder = get_translation("placeholders", "job_title", language_input()))
                )
              ),
              column(
                width = 4,
                div(class = "form-group",
                    tags$label(icon("building"), get_translation("labels", "company_sector", language_input())),
                    uiOutput(ns("company_sector_ui_set"))
                )
                )
            )
          )
        )
      )
    })


    # UI Outputs
    output$interview_format_ui_set <- renderUI({
      get_translation("labels", "interview_format_title", language_input())
    })

    output$company_info_ui_set <- renderUI({
      get_translation("labels", "company_info_title", language_input())
    })

    output$job_details_ui_set <- renderUI({
      get_translation("labels", "job_details_title", language_input())
    })


    output$start_interview <- renderUI({
      fluidRow(
        column(
          width = 12,
          div(
            class = "text-center",
            style = "margin: 20px 0;",
            actionButton(
              ns("start_interview"),
              translations$start_interview[[language_input()]],  # Direct access to the translation
              icon = icon("play"),
              class = "btn-lg btn-primary",
              style = "padding: 10px 30px; font-size: 18px;"
            )
          )
        )
      )
    })




    output$interview_details <- renderUI({
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            status = "primary",
            solidHeader = FALSE,

            tabsetPanel(
              id = ns("settings_tabs"),

              # Interview Format Tab
              tabPanel(
                title = tagList(
                  icon("comments"),
                  get_translation("labels", "format_label", language_input())
                ),
                div(
                  style = "padding: 20px 0;",
                  fluidRow(
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("file-alt"), get_translation("labels", "format_label", language_input())),
                          selectInput(ns("interview_format"), NULL,
                                      choices = translations$choices[[1]]$interview_format[[language_input()]])
                      ),
                      div(class = "form-group",
                          tags$label(icon("tasks"), get_translation("labels", "criteria_label", language_input())),
                          selectInput(ns("assessment_criteria"), NULL,
                                      choices = translations$choices$assessment_criteria[[language_input()]])
                      ),
                      div(class = "form-group",
                          tags$label(icon("clock"), get_translation("labels", "time_label", language_input())),
                          selectInput(ns("time_constraints"), NULL,
                                      choices = translations$choices$time_constraints[[language_input()]])
                      )
                    ),
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("users"), get_translation("labels", "cultural_fit_label", language_input())),
                          selectInput(ns("cultural_fit_focus"), NULL,
                                      choices = translations$choices$cultural_fit_focus[[language_input()]])
                      ),
                      div(class = "form-group",
                          tags$label(icon("sync"), get_translation("labels", "follow_up_label", language_input())),
                          selectInput(ns("follow_up_process"), NULL,
                                      choices = translations$choices$follow_up_process[[language_input()]])
                      )
                    )
                  )
                )
              ),

              # Company Info Tab
              tabPanel(
                title = tagList(
                  icon("building"),
                  get_translation("labels", "company_info_title", language_input())
                ),
                div(
                  style = "padding: 20px 0;",
                  fluidRow(
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("landmark"), get_translation("labels", "culture_label", language_input())),
                          selectInput(ns("company_culture"), NULL,
                                      choices = translations$choices$company_culture[[language_input()]])
                      ),
                      div(class = "form-group",
                          tags$label(icon("heart"), get_translation("labels", "values_label", language_input())),
                          textInput(ns("company_values"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("chart-line"), get_translation("labels", "growth_label", language_input())),
                          selectInput(ns("growth_phase"), NULL,
                                      choices = translations$choices$growth_phase[[language_input()]])
                      )
                    ),
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("exclamation-circle"), get_translation("labels", "challenges_label", language_input())),
                          textInput(ns("company_challenges"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("star"), get_translation("labels", "reputation_label", language_input())),
                          textInput(ns("company_reputation"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("home"), get_translation("labels", "environment_label", language_input())),
                          selectInput(ns("work_environment"), NULL,
                                      choices = translations$choices$work_environment[[language_input()]])
                      )
                    )
                  )
                )
              ),

              # Job Details Tab
              tabPanel(
                title = tagList(
                  icon("briefcase"),
                  get_translation("labels", "job_details_title", language_input())
                ),
                div(
                  style = "padding: 20px 0;",
                  fluidRow(
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("tasks"), get_translation("labels", "responsibilities_label", language_input())),
                          textInput(ns("responsibilities"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("check-circle"), get_translation("labels", "skills_label", language_input())),
                          textInput(ns("skills"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("users"), get_translation("labels", "team_label", language_input())),
                          textInput(ns("team"), NULL)
                      )
                    ),
                    column(
                      width = 6,
                      div(class = "form-group",
                          tags$label(icon("exclamation-triangle"), get_translation("labels", "job_challenges_label", language_input())),
                          textInput(ns("job_challenges"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("chart-bar"), get_translation("labels", "performance_label", language_input())),
                          textInput(ns("performance"), NULL)
                      ),
                      div(class = "form-group",
                          tags$label(icon("road"), get_translation("labels", "career_label", language_input())),
                          textInput(ns("career"), NULL)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    })



    # Control Buttons
    # output$control_buttons <- renderUI({
    #   actionButton(ns("start_interview"), "Démarrer l'entretien", class = "btn-primary")
    # })

    # Chat Interface
    output$user_text_and_messages <- renderUI({
      tagList(
        div(class = "chat-container", id = ns("chat_container"),
            uiOutput(ns("chat_ui"))
        ),
        shinyjs::hidden(
          div(id = ns("input_area"), class = "input-area",
              textAreaInput(ns("user_input"), NULL,
                            placeholder = if(language_input() == "ENG")
                              "Type your message here..."
                            else
                              "Tapez votre message ici..."),
              actionButton(ns("send_message"),
                           if(language_input() == "ENG") "Send" else "Envoyer",
                           class = "btn btn-primary send-button")
          )
        )
      )
    })

    # Update Chat UI
    updateChatUI <- function() {
      tryCatch({
        url <- sprintf("https://api.openai.com/v1/threads/%s/messages", rv$thread_id)
        response <- GET(
          url = url,
          add_headers(
            Authorization = paste("Bearer", rv$config$api_key),
            "OpenAI-Beta" = "assistants=v2"
          )
        )

        if (http_error(response)) {
          stop(sprintf("HTTP error: %s", http_status(response)$message))
        }

        content <- fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)

        output$chat_ui <- renderUI({
          tagList(
            lapply(rev(content$data), function(msg) {
              div(
                class = paste0("chat-bubble ",
                               if(msg$role == "user") "user-bubble" else "gpt-bubble"),
                msg$content[[1]]$text$value
              )
            })
          )
        })

        runjs(sprintf("
          var container = document.getElementById('%s');
          if(container) {
            container.scrollTop = container.scrollHeight;
          }
        ", ns("chat_container")))

      }, error = function(e) {
        showNotification(
          if(language_input() == "ENG")
            paste("Error updating chat:", e$message)
          else
            paste("Erreur de mise à jour du chat:", e$message),
          type = "error"
        )
      })
    }

    # Start Interview Event Handler
    observeEvent(input$start_interview, {
      req(input$name, input$job_title, input$company_sector)

      withProgress(message = if(language_input() == "ENG")
        'Starting conversation...'
        else
          'Démarrage de la conversation...', {

            # Initialize thread - pass rv$config
            rv$thread_id <- init_thread(rv$config)

            # Update UI visibility
            shinyjs::hide("start_interview")
            shinyjs::show("input_area")

            # Format and send initial context
            initial_context <- helpers$formatInitialContext()

            tryCatch({
              # Process initial message - pass rv$config
              result <- process_message(rv$thread_id, initial_context, rv$config)

              if(!is.null(result)) {
                rv$api_calls <- rv$api_calls + 1
                updateChatUI()
              }
            }, error = function(e) {
              showNotification(
                if(language_input() == "ENG")
                  paste("Error:", e$message)
                else
                  paste("Erreur:", e$message),
                type = "error"
              )
            })
          })
    })

    # Message Sending Event Handler
    observeEvent(input$send_message, {
      req(input$user_input, rv$thread_id)

      if (nchar(input$user_input) < 2) {
        showNotification(
          if(language_input() == "ENG")
            "Message is too short!"
          else
            "Le message est trop court !",
          type = "warning"
        )
        return()
      }

      # Apply cooldown
      helpers$applyCooldown()

      message_text <- input$user_input
      updateTextAreaInput(session, "user_input", value = "")

      withProgress(message = if(language_input() == "ENG") 'Processing...' else 'Traitement...', {
        tryCatch({
          # Process message - pass rv$config
          response <- process_message(rv$thread_id, message_text, rv$config)

          if (!is.null(response)) {
            rv$api_calls <- rv$api_calls + 1
            updateChatUI()
          }
        }, error = function(e) {
          showNotification(
            sprintf(if(language_input() == "ENG") "Error: %s" else "Erreur : %s", e$message),
            type = "error"
          )
        })
      })
    })




    # Interview Format Inputs
    output$interview_format_inputs <- renderUI({
      tagList(
        selectInput(
          ns("interview_format"),
          label = get_translation("labels", "format_label", language_input()),
          choices = translations$choices$interview_format[[language_input()]]
        ),
        selectInput(
          ns("assessment_criteria"),
          label = get_translation("labels", "criteria_label", language_input()),
          choices = translations$choices$assessment_criteria[[language_input()]]
        ),
        selectInput(
          ns("time_constraints"),
          label = get_translation("labels", "time_label", language_input()),
          choices = translations$choices$time_constraints[[language_input()]]
        ),
        selectInput(
          ns("cultural_fit_focus"),
          label = get_translation("labels", "cultural_fit_label", language_input()),
          choices = translations$choices$cultural_fit_focus[[language_input()]]
        ),
        selectInput(
          ns("follow_up_process"),
          label = get_translation("labels", "follow_up_label", language_input()),
          choices = translations$choices$follow_up_process[[language_input()]]
        )
      )
    })

    # Company Info Inputs
    output$company_info_inputs <- renderUI({
      tagList(
        selectInput(
          ns("company_culture"),
          label = get_translation("labels", "culture_label", language_input()),
          choices = translations$choices$company_culture[[language_input()]]
        ),
        textInput(
          ns("company_values"),
          label = get_translation("labels", "values_label", language_input())
        ),
        textInput(
          ns("company_challenges"),
          label = get_translation("labels", "challenges_label", language_input())
        ),
        selectInput(
          ns("growth_phase"),
          label = get_translation("labels", "growth_label", language_input()),
          choices = translations$choices$growth_phase[[language_input()]]
        ),
        textInput(
          ns("company_reputation"),
          label = get_translation("labels", "reputation_label", language_input())
        ),
        selectInput(
          ns("work_environment"),
          label = get_translation("labels", "environment_label", language_input()),
          choices = translations$choices$work_environment[[language_input()]]
        )
      )
    })

    # Job Details Inputs
    output$job_details_inputs <- renderUI({
      tagList(
        textInput(
          ns("responsibilities"),
          label = get_translation("labels", "responsibilities_label", language_input())
        ),
        textInput(
          ns("skills"),
          label = get_translation("labels", "skills_label", language_input())
        ),
        textInput(
          ns("team"),
          label = get_translation("labels", "team_label", language_input())
        ),
        textInput(
          ns("job_challenges"),
          label = get_translation("labels", "job_challenges_label", language_input())
        ),
        textInput(
          ns("performance"),
          label = get_translation("labels", "performance_label", language_input())
        ),
        textInput(
          ns("career"),
          label = get_translation("labels", "career_label", language_input())
        )
      )
    })











  })
}





## To be copied in the UI
# mod_section_interview_simulator_ui("section_interview_simulator_1")

## To be copied in the server
# mod_section_interview_simulator_server("section_interview_simulator_1")
