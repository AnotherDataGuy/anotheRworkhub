#' section_pitch_improver UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom promises then
#' @importFrom future future
#' @importFrom stringr str_remove_all str_squish
#' @importFrom tokenizers tokenize_sentences tokenize_words
#' @importFrom dplyr count filter
#' @importFrom tidytext unnest_tokens
mod_section_pitch_improver_ui <- function(id) {
  ns <- NS(id)

  shinydashboard::tabItem(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
      ),
    tabName = "tab_pitch_improver",

    # Main container
    div(
      class = "pitch-improver-container",
      style = "padding: 2rem;",

      # Header Section
      fluidRow(
        column(
          width = 12,
          div(
            class = "settings-panel",
            uiOutput(ns("app_description"))
          )
        )
      ),

      # Main Content Area - Two Column Layout
      fluidRow(
        # Left Column - Input Forms
        column(
          width = 4,
          # Required Fields Panel
          div(
            class = "required-fields-panel",
            # Required Fields Header with dynamic content
            uiOutput(ns("required_fields_header")),

            # Communication Context
            div(
              class = "form-group",
              uiOutput(ns("context_label_ui")),
              div(
                class = "context-select-wrapper",
                uiOutput(ns("communication_context_ui"))
              )
            ),

            # Recipient
            div(
              class = "form-group",
              uiOutput(ns("recipient_label_ui")),
              uiOutput(ns("recipient_of_the_pitch_ui"))
            ),

            # Hierarchical Status
            div(
              class = "form-group",
              uiOutput(ns("hierarchical_label_ui")),
              uiOutput(ns("hierarchical_status_ui"))
            )
          ),

          # Divider
          div(
            style = "height: 1px; background: #e9ecef; margin: 2rem -2rem;"
          ),

          # Optional Fields Panel
          div(
            class = "settings-panel",
            style = "margin-top: 1.5rem;",
            # Optional Fields Header
            div(
              class = "form-group",
              uiOutput(ns("optional_fields_header"))
            ),
            # Optional Fields Content
            shinyjs::hidden(
              div(
                id = ns("optional_fields"),
                # Background
                div(
                  class = "form-group",
                  uiOutput(ns("background_label_ui")),
                  uiOutput(ns("recipients_background_ui"))
                ),
                # Activity
                div(
                  class = "form-group",
                  uiOutput(ns("activity_label_ui")),
                  uiOutput(ns("recipients_activity_ui"))
                ),
                # Expertise
                div(
                  class = "form-group",
                  uiOutput(ns("expertise_label_ui")),
                  uiOutput(ns("recipients_expertise_ui"))
                )
              )
            )
          )
        ),

        # Right Column - Analysis Area
        column(
          width = 8,
          # Text Input and Stats
          div(
            class = "settings-panel",
            # Language Selection
            div(
              class = "form-group",
              uiOutput(ns("gpt_language_label_ui")),
              uiOutput(ns("gpt_language_ui"))
            ),
            # Text Area
            div(
              class = "form-group",
              uiOutput(ns("pitch_improver_user_text_area"))
            ),
            # Statistics Display
            div(
              class = "indicators",
              uiOutput(ns("main_indicators_output"))
            ),
            # Reading Speed Control
            div(
              width = 12,
              class = "form-group",
              style = "margin-top: 1.5rem;",
              uiOutput(ns("reading_speed_slider"))
            )
          ),

          # Analysis Results Panel
          div(
            class = "settings-panel",
            style = "margin-top: 1.5rem;",
            # Recap and Analysis Button
            div(
              class = "form-group",
              uiOutput(ns("recap_prompt")),
              div(
                class = "analyze-button",
                style = "text-align: center; margin-top: 1.5rem;",
                uiOutput(ns("gpt_button_pitch_improver")),
                uiOutput(ns("informative_message"))
              )
            ),
            # Analysis Results
            div(
              id = ns("gpt_pitch_analysis"),
              class = "analysis-results",
              style = "margin-top: 1.5rem;",
              uiOutput(ns("orthography_and_grammar")),
              uiOutput(ns("structure_and_coherence")),
              uiOutput(ns("potential_questions")),
              uiOutput(ns("sentiment"))
            )
          )
        )
      )
    )
  )
}

#' section_pitch_improver Server Functions
#'
#' @noRd
mod_section_pitch_improver_server <- function(id, api_pwd, language_input, translations) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    api_key <- api_pwd


    english_choices_map <- translations$english_choices_map
    french_choices_map <- translations$french_choices_map
    hierarchical_status_labels_en <- translations$hierarchical_status_labels_en
    hierarchical_status_labels_fr <- translations$hierarchical_status_labels_fr
    expect_choices_en <- translations$expect_choices_en
    expect_choices_fr <- translations$expect_choices_fr

    # Required Fields Header
    output$required_fields_header <- renderUI({
      div(
        class = "required-fields-header",
        tags$i(class = "fa fa-asterisk"),
        span(if(language_input() == "ENG") "Required Fields" else "Champs requis")
      )
    })

    # Context Label
    output$context_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-handshake-o"),
        span(if(language_input() == "ENG") "What's the context of the exchange?" else "Quel est le contexte de l'échange ?")
      )
    })

    # Recipient Label
    output$recipient_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-user"),
        span(if(language_input() == "ENG") "Who's your pitch addressed to?" else "À qui est adressé votre pitch ?")
      )
    })

    # Hierarchical Label
    output$hierarchical_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-sitemap"),
        span(if(language_input() == "ENG") "Hierarchical status of the Recipient" else "Statut hiérarchique du destinataire")
      )
    })

    # Optional Fields Header
    output$optional_fields_header <- renderUI({
      actionLink(
        ns("toggle_optional"),
        span(
          tags$i(class = "fa fa-plus-circle", style = "color: #3498db; margin-right: 8px;"),
          tags$span(
            style = "color: #3498db; font-weight: 500;",
            if(language_input() == "ENG") "Additional Details" else "Détails supplémentaires"
          )
        ),
        style = "text-decoration: none;"
      )
    })

    # Background Label
    output$background_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-id-card"),
        span(if(language_input() == "ENG") "Background of your recipient/ personality" else "Contexte/ parcours/ personnalité de votre interlocuteur/ destinataire")
      )
    })

    # Activity Label
    output$activity_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-building"),
        span(if(language_input() == "ENG") "Sector of activity of your recipient" else "Secteur d'activité de votre interlocuteur/ destinataire")
      )
    })

    # Expertise Label
    output$expertise_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-graduation-cap"),
        span(if(language_input() == "ENG") "Expertise of your recipient" else "Expertise de votre interlocuteur/ destinataire")
      )
    })

    # GPT Language Label
    output$gpt_language_label_ui <- renderUI({
      tags$label(
        class = "input-label",
        tags$i(class = "fa fa-language"),
        span(if(language_input() == "ENG") "Select language for GPT responses:" else "Sélectionner la langue pour les réponses GPT :")
      )
    })

    output$expectations_grid <- renderUI({
      div(class = "expectations-grid",
          # First Level
          div(class = "radio-card",
              tags$input(type = "radio", id = ns("level_1"), name = ns("expectation_level"), value = if(language_input() == "ENG") "High" else "Elevé"),
              tags$label(`for` = ns("level_1"),
                         span(class = "expectation-label",
                              if(language_input() == "ENG") "High" else "Élevé"),
                         span(class = "expectation-description",
                              if(language_input() == "ENG") "Standard excellence" else "Excellence standard"),
                         div(class = "expectation-progress",
                             div(class = "expectation-progress-bar"))
              )
          ),

          # Second Level
          div(class = "radio-card",
              tags$input(type = "radio", id = ns("level_2"), name = ns("expectation_level"), value = if(language_input() == "ENG") "Very High" else "Très élevé"),
              tags$label(`for` = ns("level_2"),
                         span(class = "expectation-label",
                              if(language_input() == "ENG") "Very High" else "Très élevé"),
                         span(class = "expectation-description",
                              if(language_input() == "ENG") "Superior quality" else "Qualité supérieure"),
                         div(class = "expectation-progress",
                             div(class = "expectation-progress-bar"))
              )
          ),

          # Third Level
          div(class = "radio-card",
              tags$input(type = "radio", id = ns("level_3"), name = ns("expectation_level"), value = if(language_input() == "ENG") "Exceptional" else "Exceptionnellement élevé"),
              tags$label(`for` = ns("level_3"),
                         span(class = "expectation-label",
                              if(language_input() == "ENG") "Exceptional" else "Exceptionnel"),
                         span(class = "expectation-description",
                              if(language_input() == "ENG") "Outstanding performance" else "Performance exceptionnelle"),
                         div(class = "expectation-progress",
                             div(class = "expectation-progress-bar"))
              )
          )
      )
    })



    # Create a reactive value to track input validity
    input_is_valid <- reactive({
      !is.null(input$communication_context) &&
        !is.null(input$recipient_of_the_pitch) &&
        !is.null(input$hierarchical_status) &&
        nchar(input$text_input_pitch_improver) > 100
    })

    output$informative_message <- renderUI({
      if (!input_is_valid()) {
        lang <- language_input()
        message <- if (lang == "ENG") {
          "Please fill in all required fields and enter a pitch with more than 100 characters."
        } else {
          "Veuillez remplir tous les champs obligatoires et entrer un pitch de plus de 100 caractères."
        }
        div(
          class = "informative-message",
          style = "color: red; font-size: 14px; margin-top: 10px;",
          message
        )
      }
    })



    # Reactive value to store the selected GPT response language
    gpt_language_input <- reactive({
      req(input$gpt_language_messages)
      input$gpt_language_messages
    })

    # Dynamic GPT Language Selection UI
    output$gpt_language_ui <- renderUI({
      lang <- language_input()

      choices_lang <- if (lang == "ENG") {
        c("French" = "FR", "English" = "ENG")
      } else {
        c("Français" = "FR", "Anglais" = "ENG")
      }

      selected_lang <- if (lang == "ENG") "ENG" else "FR"

      selectInput(
        ns("gpt_language_messages"),
        label = if (lang == "ENG") "Select language for GPT responses:" else "Sélectionner la langue pour les réponses GPT :",
        choices = choices_lang,
        width = "100%",
        selected = selected_lang
      )
    })

    # App description
    output$app_description <- renderUI({
      lang <- language_input()

      if (lang == "ENG") {
        HTML('<h2 style="font-size: 16px;">Preparing your application for your future position? This application helps you improve your professional pitches in various communication contexts. You can tailor the analysis according to specific criteria such as your recipient, the level of exigency, and more. The application operates using generative artificial intelligence. By nature, responses will vary each time a report is generated. The application does not in any way replace the advice of an expert.</h2>')
      } else if (lang == "FR") {
        HTML('<h2 style="font-size: 16px;">Vous préparez votre candidature pour votre futur poste ? Cette application vous permet d\'améliorer vos pitchs professionnels dans différents contextes d\'échange. Vous pouvez calibrer l\'analyse selon des critères spécifiques tels que votre destinataire, le niveau d\'exigence, etc. L\'application fonctionne à l\'aide d\'intelligences artificielles de type génératives. Par nature, les réponses seront différentes à chaque fois qu\'un rapport est généré. L\'application ne remplace en aucun cas les avis d\'un expert.</h2>')
      } else {
        HTML('<h2 style="font-size: 18px;">Language Not Supported</h2>')
      }
    })

    # User text area
    output$pitch_improver_user_text_area <- renderUI({
      label_text <- if (language_input() == "ENG") {
        "Pitch to analyze:"
      } else {
        "Pitch à analyser :"
      }

      textAreaInput(
        inputId = ns("text_input_pitch_improver"),
        label = label_text,
        rows = 9,
        width = "100%"
      )
    })

    # GPT button
    output$gpt_button_pitch_improver <- renderUI({
      button_label <- if (gpt_language_input() == "ENG") {
        "Start Analysis 🚀"
      } else {
        "Démarrer l'analyse 🚀"
      }

      disabled <- !input_is_valid()

      actionButton(
        inputId = ns("gpt_update_pitch_improver_button"),
        label = button_label,
        disabled = disabled
      )
    })

    # Reading speed slider
    output$reading_speed_slider <- renderUI({
      req(language_input())
      label <- if (language_input() == "ENG") {
        "Adjust Reading Speed (Words Per Minute):"
      } else {
        "Ajuster la vitesse de lecture (mots par minute) pour calibrer l'estimation de la durée du pitch à l'oral :"
      }

      sliderInput(ns("reading_speed"), label, value = 150, min = 90, max = 180, width = "100%")
    })

    # Pitch recipient
    output$recipient_of_the_pitch_ui <- renderUI({
      div(
        class = "pitch-recipient-input-wrapper",
        textInput(
          inputId = ns("recipient_of_the_pitch"),
          label = NULL,
          placeholder = if(language_input() == "ENG") "Enter recipient name..." else "Entrez le nom du destinataire..."
        ) %>% tagAppendAttributes(class = "pitch-recipient-input")
      )
    })

    # Communication context
    output$communication_context_ui <- renderUI({
      div(
        class = "context-select-wrapper",
        selectInput(
          inputId = ns("communication_context"),
          label = NULL,
          width = "100%",
          choices = if(language_input() == "ENG") {
            list(
              "Application" = c(
                "Spontaneous application" = "spontaneous_application",
                "Reply to an offer" = "offer_reply"
              ),
              "Interviews" = c(
                "Phone Screening" = "phone_screening",
                "One-on-One Interview" = "one_on_one_interview"
              ),
              "Meetings" = c(
                "Formal Meeting" = "first_formal_meeting",
                "Informal Meeting" = "first_informal_meeting"
              ),
              "Networking" = c(
                "Networking Event" = "networking_event",
                "Follow-up After Networking Event" = "followup_after_networking"
              ),
              "Job Offers" = c(
                "Job Offer Acceptance" = "job_offer_acceptance",
                "Job Offer Clarification" = "job_offer_clarification",
                "Rejecting a Job Offer" = "rejecting_job_offer"
              ),
              "Post-Interview" = c(
                "Asking for Feedback" = "asking_for_feedback",
                "Follow-up After Interview" = "followup_after_interview"
              )
            )
          } else {
            list(
              "Candidature" = c(
                "Candidature spontanée" = "spontaneous_application",
                "Réponse à une offre" = "offer_reply"
              ),
              "Entretiens" = c(
                "Entretien téléphonique" = "phone_screening",
                "Entretien individuel" = "one_on_one_interview"
              ),
              "Réunions" = c(
                "Réunion formelle" = "first_formal_meeting",
                "Réunion informelle" = "first_informal_meeting"
              ),
              "Réseautage" = c(
                "Événement de réseautage" = "networking_event",
                "Suivi après un événement de réseautage" = "followup_after_networking"
              ),
              "Offres d'emploi" = c(
                "Acceptation d'offre d'emploi" = "job_offer_acceptance",
                "Clarification d'offre d'emploi" = "job_offer_clarification",
                "Refus d'offre d'emploi" = "rejecting_job_offer"
              ),
              "Post-Entretien" = c(
                "Demande de retour d'information" = "asking_for_feedback",
                "Suivi après l'entretien" = "followup_after_interview"
              )
            )
          },
          selected = "spontaneous_application"
        ) %>% tagAppendAttributes(class = "context-select")
      )
    })



    # Recipient's background
    output$recipients_background_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          if (language_input() == "ENG") "Background of your recipient/ personality" else "Contexte/ parcours/ personnalité de votre interlocuteur/ destinataire"
        ),
        textInput(ns("recipients_background"), label = NULL)
      )
    })

    # Recipient's activity sector
    output$recipients_activity_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          if (language_input() == "ENG") "Sector of activity of your recipient" else "Secteur d'activité de votre interlocuteur/ destinataire"
        ),
        textInput(ns("recipients_activity"), label = NULL)
      )
    })

    # Recipient's expertise
    output$recipients_expertise_ui <- renderUI({
      div(
        class = "form-group",
        tags$label(
          class = "input-label",
          if (language_input() == "ENG") "Expertise of your recipient" else "Expertise de votre interlocuteur/ destinataire"
        ),
        textInput(ns("recipients_expertise"), label = NULL)
      )
    })





    # Hierarchical status
    # Initialize hierarchical_status with a default value
    rv <- reactiveValues(hierarchical_status = "entry_level")

    observe({
      req(input$hierarchical_status)
      rv$hierarchical_status <- input$hierarchical_status
    })

    # Hierarchical status
    output$hierarchical_status_ui <- renderUI({
      selected_status <- input$hierarchical_status %||% "entry_level"

      choices <- if (language_input() == "ENG") {
        list(
          entry_level = list(icon = "user", label = "Entry Level"),
          manager = list(icon = "user-tie", label = "Manager"),
          senior_manager = list(icon = "users", label = "Senior Manager"),
          director = list(icon = "user-graduate", label = "Director"),
          ceo = list(icon = "crown", label = "CEO")
        )
      } else {
        list(
          entry_level = list(icon = "user", label = "Débutant"),
          manager = list(icon = "user-tie", label = "Manager"),
          senior_manager = list(icon = "users", label = "Manager Senior"),
          director = list(icon = "user-graduate", label = "Directeur"),
          ceo = list(icon = "crown", label = "PDG")
        )
      }

      tagList(
        tags$div(
          class = "form-group",
          uiOutput(ns("hierarchical_label_ui")),
          tags$div(
            class = "avatar-grid",
            lapply(names(choices), function(value) {
              tags$div(
                class = if(value == selected_status) "avatar-item selected" else "avatar-item",
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", ns("hierarchical_status"), value),
                tags$div(
                  class = "avatar-circle",
                  tags$i(class = paste0("fas fa-", choices[[value]]$icon))
                ),
                tags$div(
                  class = "avatar-label",
                  choices[[value]]$label
                )
              )
            })
          )
        )
      )
    })

    # Expectation level
    output$expectations_level_ui <- renderUI({
      label <- if (language_input() == "ENG") "Level of expectations" else "Niveau d'attentes"
      choices <- if (language_input() == "ENG") {
        c("High" = "High", "Very high" = "Very high", "Exceptionally High Expectations" = "Exceptionally High")
      } else {
        c("Elevé" = "Elevé", "Très élevé" = "Très élevé", "Exceptionnellement élevé" = "Exceptionnellement élevé")
      }
      radioButtons(inputId = ns("expectations_level"), label = label, choices = choices, selected = "Elevé")
    })

    # Main indicators
    output$main_indicators_output <- renderUI({
      req(language_input())
      donnees <- reactive_summary_data()
      lang <- language_input()

      labels <- if(lang == "ENG") {
        c("Total characters", "Total words", "Total sentences", "Total paragraphs", "Pitch length (s)")
      } else {
        c("Nombre de caractères", "Nombre de mots", "Nombre de phrases", "Nombre de paragraphes", "Durée du pitch (s)")
      }

      html_content <- sprintf('
  <div class="stats-container">
    <div class="stat-item">
      <div class="stat-value">%s</div>
      <div class="stat-label">%s</div>
    </div>
    <div class="stat-item">
      <div class="stat-value">%s</div>
      <div class="stat-label">%s</div>
    </div>
    <div class="stat-item">
      <div class="stat-value">%s</div>
      <div class="stat-label">%s</div>
    </div>
    <div class="stat-item">
      <div class="stat-value">%s</div>
      <div class="stat-label">%s</div>
    </div>
    <div class="stat-item">
      <div class="stat-value">%s</div>
      <div class="stat-label">%s</div>
    </div>
  </div>',
                              donnees[[1]], labels[1],
                              donnees[[2]], labels[2],
                              donnees[[3]], labels[3],
                              donnees[[4]], labels[4],
                              donnees[[5]], labels[5]
      )

      HTML(html_content)
    })




    # Create a reactive expression to calculate summary
    reactive_summary_data <- reactive({
      req(input$reading_speed, language_input())
      calculate_summary(input$text_input_pitch_improver, input$reading_speed, language=language_input())
    })


    observeEvent(input$toggle_optional, {
      shinyjs::toggle(id = "optional_fields", anim = TRUE)
    })



    output$main_indicators_output <- renderUI({
      req(language_input())
      donnees <- reactive_summary_data()
      lang <- language_input()

      # Définir les étiquettes selon la langue
      labels <- if(lang == "ENG") {
        c("Total characters", "Total words", "Total sentences", "Total paragraphs", "Pitch length (s)")
      } else {
        c("Nombre de caractères", "Nombre de mots", "Nombre de phrases", "Nombre de paragraphes", "Durée du pitch (s)")
      }

      # Construire le HTML
      html_content <- paste0(
        '<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css">',
        '<div class="stats-container">',
        paste0(
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[1]], '</span>',
          '<span class="stat-label">', labels[1], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[2]], '</span>',
          '<span class="stat-label">', labels[2], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[3]], '</span>',
          '<span class="stat-label">', labels[3], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[4]], '</span>',
          '<span class="stat-label">', labels[4], '</span>',
          '</div>',
          '<div class="stat-item">',
          '<div class="stat-background"></div>',
          '<span class="stat-value">', donnees[[5]], '</span>',
          '<span class="stat-label">', labels[5], '</span>',
          '</div>',
          '</div>'
        ),
        collapse = ""
      )

      HTML(html_content)
    })







    # Define a reactive expression for constructing the prompt summary
    prompt_summary_for_user <- reactive({
      lang <- language_input()
      hierarchical_status_labels <- if (lang == "ENG") translations$hierarchical_status_labels_en else translations$hierarchical_status_labels_fr
      expect_choices_labels <- if (lang == "ENG") translations$expect_choices_en else translations$expect_choices_fr

      sentences <- c(
        construct_sentence(
          input$communication_context,
          "<b>The context of the interaction</b>: ",
          "<br><b>Le contexte de l'interaction</b> : ",
          lang,
          FALSE,
          translations
        ),
        construct_sentence(
          input$recipient_of_the_pitch,
          "<br><b>Pitch recipient</b>: ",
          "<br><b>Le pitch s'adresse à</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_background,
          "<br><b>Personality or background of the recipient of the pitch</b>: ",
          "<br><b>Personnalité ou parcours du destinataire</b> :",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_activity,
          "<br><b>Activity sector of the recipient</b>: ",
          "<br><b>Secteur d'activité du destinataire</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence(
          input$recipients_expertise,
          "<br><b>Expertise of the recipient</b>: ",
          "<br><b>Expertise du destinataire</b> : ",
          lang,
          TRUE,
          translations
        ),
        construct_sentence_niveau(
          input$hierarchical_status,
          "<br><b>Hierarchical status of the recipient</b>: ",
          "<br><b>Statut hiérarchique du destinataire</b> : ",
          lang,
          FALSE,
          hierarchical_status_labels
        ),
        construct_general_sentence(
          input$expectations_level,
          "<br><b>Level of expectations</b>: ",
          "<br><b>Niveau d'exigence</b> : ",
          lang,
          expect_choices_labels
        )
      )
      sentences_with_content <- sentences[sentences != ""]
      final_message_for_api <- paste(c(sentences_with_content, "<br><b>Pitch</b>: ", input$text_input_pitch_improver), collapse = " ")
      final_message_for_api
    })



    # Recap prompt
    output$recap_prompt <- renderUI({
      if (!is.null(prompt_summary_for_user()) && nchar(prompt_summary_for_user()) > 0) {
        tagList(
          HTML(prompt_summary_for_user()),
          tags$script('showTextSlowly();')
        )
      } else {
        HTML("<p>No prompt summary available yet.</p>")
      }
    })


    responses <- reactiveValues(
      orthography_and_grammar = NULL,
      structure_and_coherence = NULL,
      potential_questions = NULL,
      sentiment = NULL
    )



    observeEvent(input$gpt_update_pitch_improver_button, {
      req(input_is_valid())
      user_input <- prompt_summary_for_user()
      lang <- gpt_language_input()
      ui_lang <- language_input()

      # Define admin prompts
      admin_prompt_orthography_and_grammar <- reactive({
        if(lang == "ENG") {
          "Analyze only the grammar and orthography of the user's pitch and give insights only on what needs to be changed or improved. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse uniquement la grammaire et l'orthographe du pitch de l'utilisateur et fais un retour uniquement sur les aspects à modifier ou à améliorer. Aucun autre aspect. Ton neutre. Utilise des bullets points, des sauts de ligne ou du HTML pour améliorer la lisibilité. Limiter la réponse à 400 caractères. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_structure_and_coherence <- reactive({
        if(lang == "ENG") {
          "Analyze only the structure of the user's pitch. The pitch must be logically organized, without contradictions or ambiguities. No other aspects. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse la structure, la clarté, la cohérence et le sens du pitch fourni par l'utilisateur. Aucun autre aspect. Ton neutre. Utilisez le HTML ou des sauts de ligne pour la lisibilité. Limite de 400 caractères. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_potential_questions <- reactive({
        if(lang == "ENG") {
          "Generate questions the recipient might have after receiving the user's pitch. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 500 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Génère des questions que le destinataire pourrait avoir après avoir reçu le pitch de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour améliorer la lisibilité. Limiter la réponse à 500 caractères. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      admin_prompt_sentiment_response <- reactive({
        if(lang == "ENG") {
          "Analyze the emotional valence of the user's text. Nothing else. Maintain a neutral tone. Format the text to include line breaks or bold text to improve readability. Limit response to 400 characters. ANSWER ONLY IN ENGLISH."
        } else {
          "Analyse la valence émotionnelle du texte de l'utilisateur. Aucune autre chose. Ton neutre. Formate le texte pour inclure des sauts de ligne ou de la police en gras pour améliorer la lisibilité. Limiter la réponse à 400 caractères. REPONDS UNIQUEMENT EN FRANCAIS."
        }
      })

      # Orthography and Grammar Analysis
      showModal(modalDialog(
        if (ui_lang == "ENG") "Analyzing orthography and grammar..." else "Analyse de l'orthographe et de la grammaire en cours...",
        easyClose = FALSE
      ))

      future::future({
        fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_orthography_and_grammar(), "gpt-4o-mini")
      }) %>%
        promises::then(function(result) {
          responses$orthography_and_grammar <- result
          removeModal()

          # Structure Analysis
          showModal(modalDialog(
            if (ui_lang == "ENG") "Analyzing structure..." else "Analyse de la structure en cours...",
            easyClose = FALSE
          ))

          future::future({
            fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_structure_and_coherence(), "gpt-4o-mini")
          }) %>%
            promises::then(function(result) {
              responses$structure_and_coherence <- result
              removeModal()

              # Potential Questions Analysis
              showModal(modalDialog(
                if (ui_lang == "ENG") "Generating potential questions..." else "Génération des questions éventuelles...",
                easyClose = FALSE
              ))

              future::future({
                fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_potential_questions(), "gpt-4o-mini")
              }) %>%
                promises::then(function(result) {
                  responses$potential_questions <- result
                  removeModal()

                  # Sentiment Analysis
                  showModal(modalDialog(
                    if (ui_lang == "ENG") "Analyzing emotional valence..." else "Analyse de la valence émotionnelle en cours...",
                    easyClose = FALSE
                  ))

                  future::future({
                    fct_interact_with_gpt_api_only_text(api_key, user_input, admin_prompt_sentiment_response(), "gpt-4o-mini")
                  }) %>%
                    promises::then(function(result) {
                      responses$sentiment <- result
                      removeModal()
                    })
                })
            })
        })
    })

    ####################

    output$orthography_and_grammar <- renderUI({
      req(responses$orthography_and_grammar)
      header <- if(gpt_language_input() == "ENG") {
        "Orthography and Grammar"
      } else {
        "Orthographe et grammaire"
      }
      column(width=12,
             h3(header),
             renderMarkdown(responses$orthography_and_grammar)
      )
    })

    output$structure_and_coherence <- renderUI({
      req(responses$structure_and_coherence)
      header <- if(gpt_language_input() == "ENG") {
        "Structure and Coherence"
      } else {
        "Structure et cohérence"
      }
      column(width=12,
             h3(header),
             renderMarkdown(responses$structure_and_coherence)
      )
    })

    output$potential_questions <- renderUI({
      req(responses$potential_questions)
      header <- if(gpt_language_input() == "ENG") {
        "Potential Questions"
      } else {
        "Questions éventuelles"
      }
      column(width=12,
             h3(header),
             renderMarkdown(responses$potential_questions)
      )
    })

    output$sentiment <- renderUI({
      req(responses$sentiment)
      header <- if(gpt_language_input() == "ENG") {
        "Emotional Valence"
      } else {
        "Valence émotionnelle"
      }
      column(width=12,
             h3(header),
             renderMarkdown(responses$sentiment)
      )
    })



  })
}




## To be copied in the UI
# mod_section_pitch_improver_ui("section_pitch_improver_1")

## To be copied in the server
# mod_section_pitch_improver_server("section_pitch_improver_1")
