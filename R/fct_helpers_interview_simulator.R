#' Interview Simulator Helper Functions
#'
#' @description A collection of helper functions for the interview simulator module.
#' These functions handle API communication with OpenAI, message processing, and thread management.
#'
#' @importFrom httr GET POST add_headers http_error http_status
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom stats difftime
#'
#' @section API Functions:
#' - send_message: Sends a message to the OpenAI API
#' - execute_assistant: Triggers the assistant to process the message
#' - monitor_run_status: Checks the status of an assistant run
#' - fetch_response: Retrieves messages from a thread
#'
#' @section Thread Management:
#' - init_thread: Creates a new conversation thread
#' - monitor_until_complete: Monitors a thread until completion or timeout
#'
#' @section Message Processing:
#' - process_message: Handles the complete message processing flow
#'
#' @section Utility Functions:
#' - get_timestamp: Returns current timestamp
#'
#' @return The functions return various types depending on their purpose.
#'
#' @noRd

#' Send Message to OpenAI API
#'
#' @param thread_id Character string of the thread ID
#' @param message Character string of the message to send
#' @param config List containing API configuration including api_key
#' @return List containing the API response
#' @importFrom httr POST add_headers http_error http_status
#' @importFrom jsonlite toJSON fromJSON
#' @noRd
send_message <- function(thread_id, message, config) {

  tryCatch({
    url <- sprintf("https://api.openai.com/v1/threads/%s/messages", thread_id)
    response <- httr::POST(
      url = url,
      httr::add_headers(
        Authorization = paste("Bearer", config$api_key),
        "Content-Type" = "application/json",
        "OpenAI-Beta" = "assistants=v2"
      ),
      body = jsonlite::toJSON(list(
        role = "user",
        content = message
      ), auto_unbox = TRUE),
      encode = "raw"
    )

    if (httr::http_error(response)) {
      stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content <- jsonlite::fromJSON(rawToChar(response$content))
    if (!is.null(content$error)) {
      stop(content$error$message)
    }
    return(content)
  }, error = function(e) {
    stop(sprintf("Failed to add message: %s", e$message))
  })
}

#' Execute Assistant
#'
#' @param thread_id Character string of the thread ID
#' @param config List containing API configuration including api_key and assistant_id
#' @return Character string of the run ID
#' @importFrom httr POST add_headers http_error http_status
#' @importFrom jsonlite toJSON fromJSON
#' @noRd
execute_assistant <- function(thread_id, config) {

  tryCatch({
    url <- sprintf("https://api.openai.com/v1/threads/%s/runs", thread_id)
    response <- httr::POST(
      url = url,
      httr::add_headers(
        Authorization = paste("Bearer", config$api_key),
        "Content-Type" = "application/json",
        "OpenAI-Beta" = "assistants=v2"
      ),
      body = jsonlite::toJSON(list(
        assistant_id = config$assistant_id
      ), auto_unbox = TRUE),
      encode = "raw"
    )

    if (httr::http_error(response)) {
      stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content <- jsonlite::fromJSON(rawToChar(response$content))
    if (!is.null(content$error)) {
      stop(content$error$message)
    }
    return(content$id)
  }, error = function(e) {
    stop(sprintf("Failed to execute assistant: %s", e$message))
  })
}

#' Monitor Run Status
#'
#' @param thread_id Character string of the thread ID
#' @param run_id Character string of the run ID
#' @param config List containing API configuration including api_key
#' @return Character string of the current status
#' @importFrom httr GET add_headers http_error http_status
#' @importFrom jsonlite fromJSON
#' @noRd
monitor_run_status <- function(thread_id, run_id, config) {
  tryCatch({
    url <- sprintf("https://api.openai.com/v1/threads/%s/runs/%s", thread_id, run_id)
    response <- httr::GET(
      url = url,
      httr::add_headers(
        Authorization = paste("Bearer", config$api_key),
        "OpenAI-Beta" = "assistants=v2"
      )
    )

    if (httr::http_error(response)) {
      stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content <- jsonlite::fromJSON(rawToChar(response$content))
    return(content$status)
  }, error = function(e) {
    stop(sprintf("Failed to check run status: %s", e$message))
  })
}

#' Process Complete Message Flow
#'
#' @param thread_id Character string of the thread ID
#' @param message Character string of the message to process
#' @param config List containing API configuration including api_key, assistant_id, and timeout_seconds
#' @return Character string of the assistant's response
#' @noRd
process_message <- function(thread_id, message, config) {
  tryCatch({
    # Send message with config
    message_result <- send_message(thread_id, message, config)

    # Execute assistant with config
    run_id <- execute_assistant(thread_id, config)

    # Monitor completion with config
    status <- monitor_until_complete(thread_id, run_id, config)

    if(status != "completed") {
      stop("Processing incomplete or timed out")
    }

    # Return the response with config
    return(fetch_response(thread_id, config))

  }, error = function(e) {
    stop(sprintf("Error in process_message: %s", e$message))
  })
}

#' Fetch Response from Thread
#'
#' @param thread_id Character string of the thread ID
#' @param config List containing API configuration including api_key
#' @return Character string of the assistant's response
#' @importFrom httr GET add_headers http_error http_status
#' @importFrom jsonlite fromJSON
#' @noRd
fetch_response <- function(thread_id, config) {

  tryCatch({
    url <- sprintf("https://api.openai.com/v1/threads/%s/messages", thread_id)
    response <- httr::GET(
      url = url,
      httr::add_headers(
        Authorization = paste("Bearer", config$api_key),
        "OpenAI-Beta" = "assistants=v2"
      )
    )

    if (httr::http_error(response)) {
      stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = FALSE)
    if (!is.null(content$data) && length(content$data) > 0) {
      assistant_idx <- vapply(content$data, function(x) x$role == "assistant", logical(1))
      assistant_messages <- content$data[assistant_idx]
      if (length(assistant_messages) > 0) {
        return(assistant_messages[[1]]$content[[1]]$text$value)
      }
    }
    return(NULL)
  }, error = function(e) {
    stop(sprintf("Failed to get response: %s", e$message))
  })
}

#' Get Current Timestamp
#'
#' @return Character string of current timestamp in format YYYY-MM-DD HH:MM:SS
#' @noRd
get_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

#' Initialize Thread
#'
#' @param config List containing API configuration including api_key
#' @return Character string of the new thread ID
#' @importFrom httr POST add_headers http_error http_status
#' @importFrom jsonlite toJSON fromJSON
#' @noRd
init_thread <- function(config) {

  tryCatch({
    url <- "https://api.openai.com/v1/threads"
    response <- httr::POST(
      url = url,
      httr::add_headers(
        Authorization = paste("Bearer", config$api_key),
        "Content-Type" = "application/json",
        "OpenAI-Beta" = "assistants=v2"
      ),
      body = jsonlite::toJSON(list(), auto_unbox = TRUE),
      encode = "raw"
    )

    if (httr::http_error(response)) {
      stop(sprintf("HTTP error: %s", httr::http_status(response)$message))
    }

    content <- jsonlite::fromJSON(rawToChar(response$content))
    if (!is.null(content$error)) {
      stop(content$error$message)
    }
    return(content$id)
  }, error = function(e) {
    stop(sprintf("Thread creation failed: %s", e$message))
  })
}

#' Monitor Thread Until Complete
#'
#' @param thread_id Character string of the thread ID
#' @param run_id Character string of the run ID
#' @param config List containing API configuration including api_key and timeout_seconds
#' @return Character string of the final status
#' @importFrom stats difftime
#' @noRd
monitor_until_complete <- function(thread_id, run_id, config) {
  start_time <- Sys.time()
  status <- "queued"

  repeat {
    Sys.sleep(1)
    status <- monitor_run_status(thread_id, run_id, config)

    if (status == "completed" ||
        difftime(Sys.time(), start_time, units = "secs") >= config$timeout_seconds) {
      break
    }
  }

  if (status != "completed") stop("Processing timed out")
  return(status)
}
