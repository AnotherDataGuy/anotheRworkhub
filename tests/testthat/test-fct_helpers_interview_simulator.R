test_that("process_message handles the complete message flow", {
  # Create sequential mock responses
  msg_response <- structure(list(
    status_code = 200,
    content = charToRaw('{"id": "msg_123"}')
  ), class = "response")

  run_response <- structure(list(
    status_code = 200,
    content = charToRaw('{"id": "run_123"}')
  ), class = "response")

  status_response <- structure(list(
    status_code = 200,
    content = charToRaw('{"status": "completed"}')
  ), class = "response")

  final_response <- structure(list(
    status_code = 200,
    content = charToRaw('{"data": [{"role": "assistant", "content": [{"text": {"value": "test response"}}]}]}')
  ), class = "response")

  # Setup mocks for POST and GET
  post_mock <- mock(msg_response, run_response)
  get_mock <- mock(status_response, final_response)

  with_mocked_bindings(
    POST = post_mock,
    GET = get_mock,
    http_error = function(...) FALSE,
    .package = "httr",
    {
      config <- list(
        api_key = "test-key",
        assistant_id = "asst_123",
        timeout_seconds = 1
      )

      result <- process_message("thread_123", "test message", config)
      expect_equal(result, "test response")

      # Verify our mocks were called
      expect_called(post_mock, 2)  # Once for message, once for run
      expect_called(get_mock, 2)   # Once for status, once for response
    }
  )
})
