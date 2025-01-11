#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bs4Dash::dashboardPage(
      dark = TRUE,
      help = NULL,
      header = bs4Dash::dashboardHeader(uiOutput("user_profile")),
      sidebar = bs4Dash::dashboardSidebar(disable = TRUE),
      controlbar = NULL,
      footer = NULL,
      body = bs4Dash::dashboardBody(
        tags$div(
          style = "margin-bottom: 80px;",  # Keep the spacing
          uiOutput("auth_output"),
          fluidRow(
            column(width = 9),
            column(width = 3,
                   style = "text-align: right;padding: 3px 32px 0 0px;",
                   selectInput("language", "Langue de l'interface/ UI's Language:",
                               choices = c("Français" = "FR", "English" = "ENG"),
                               selected = "Français")
            )
          ),
          # Content sections
          div(id = "interview_section", uiOutput("interview_content")),
          div(id = "pitch_section", style = "display: none;", uiOutput("pitch_content"))
        ),
        # Fixed bottom navigation
        tags$div(
          id = "bottom-sidebar",
          style = "
      position: fixed;
      bottom: 2vh;
      left: 50%;
      transform: translateX(-50%);
      width: 96%;
      max-width: 800px;
      background: rgba(25, 28, 36, 0.85);
      backdrop-filter: blur(8px);
      padding: 15px;
      border-radius: 12px;
      box-shadow: 0 -8px 32px 0 rgba(31, 38, 135, 0.37);
      z-index: 1031;
    ",
          div(
            class = "bottom-sidebar-menu",
            style = "display: flex; justify-content: center; gap: 12px;",
            actionButton(
              "btn_interview",
              label = tags$div(
                style = "display: flex; align-items: center; gap: 8px;",
                icon("id-card"),
                tags$span("Simuler un entretien")
              ),
              class = "nav-link active"
            ),
            actionButton(
              "btn_pitch",
              label = tags$div(
                style = "display: flex; align-items: center; gap: 8px;",
                icon("sliders"),
                tags$span("Améliorer mon pitch")
              ),
              class = "nav-link"
            )
          ),
          tags$button(
            id = "toggle-bottom-sidebar",
            icon("chevron-down"),
            style = "
        position: absolute;
        top: -18px;
        right: 15px;
        background: rgba(255, 193, 7, 0.9);
        border: none;
        width: 36px;
        height: 36px;
        border-radius: 50%;
        cursor: pointer;
      "
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    tags$link(rel = "stylesheet", href = "www/app_ui.css"),
    tags$link(rel = "stylesheet", href = "www/interview_simulator.css"),
    tags$link(rel = "stylesheet", href = "www/pitch_improver.css"),
    tags$script(src = "main_indicators.js"),
    # Update the JavaScript for minimize functionality
    tags$script("
      $(document).ready(function() {
        // Show interview section by default
        $('#interview_section').show();
        $('#pitch_section').hide();

        // Handle interview button click
        $('#btn_interview').on('click', function() {
          $('#interview_section').show();
          $('#pitch_section').hide();
          $('.nav-link').removeClass('active');
          $(this).addClass('active');
        });

        // Handle pitch button click
        $('#btn_pitch').on('click', function() {
          $('#interview_section').hide();
          $('#pitch_section').show();
          $('.nav-link').removeClass('active');
          $(this).addClass('active');
        });

        // Handle minimize button click - Updated for new transform
        $('#toggle-bottom-sidebar').on('click', function() {
          const sidebar = $('#bottom-sidebar');
          if (sidebar.hasClass('minimized')) {
            sidebar.removeClass('minimized');
            // Reset to original transform
            sidebar.css('transform', 'translateX(-50%)');
          } else {
            sidebar.addClass('minimized');
            // Combine the transforms
            sidebar.css('transform', 'translate(-50%, calc(100% - 40px))');
          }
          $(this).find('i').toggleClass('fa-chevron-down fa-chevron-up');
        });
      });
    "),
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "anotheRworkhub")
  )
}
