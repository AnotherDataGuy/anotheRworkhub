#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(

      ui = shinymanager::secure_app(
        tags_top = tags$div(
          tags$img(
            src="https://images.unsplash.com/photo-1577563908411-5077b6dc7624",
            # height = "150px",
            width = "150px"
          ),
          # tags$img(src = "www/awh_logo.png", height = "auto", width = "100%"),
          h4("Welcome to AnotherWorkhub!"),
          p("For more details about this app and access password please visit its associated ",
            a(href = "https://github.com/AnotherDataGuy/anotheRworkhub", "GitHub repository.", target = "_blank"),
            style = "text-align: center; font-size: 14px; color: gray;"
          )
        ),
        app_ui
        ),
      # ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

