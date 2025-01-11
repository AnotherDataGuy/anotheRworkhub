test_that("renderMarkdown handles different input formats correctly", {
  # Test empty inputs
  expect_equal(as.character(renderMarkdown("")), "")
  expect_equal(as.character(renderMarkdown(NULL)), "")

  # Test JSON input
  json_input <- '{"name": "John", "age": 30}'
  expect_type(renderMarkdown(json_input), "character")

  # Test HTML input
  html_input <- '<p>This is HTML</p>'
  expect_type(renderMarkdown(html_input), "character")

  # Test LaTeX input
  latex_input <- '$E=mc^2$'
  latex_result <- as.character(renderMarkdown(latex_input))
  expect_true(grepl('class="math"', latex_result))

  # Test plain text
  plain_input <- "Just plain text"
  expect_type(renderMarkdown(plain_input), "character")
})

test_that("calculate_summary handles text analysis correctly", {
  # Test English text
  text <- "This is a test. It has two sentences."
  result_eng <- calculate_summary(text, reading_speed = 150, language = "ENG")
  expect_s3_class(result_eng, "data.frame")
  expect_named(result_eng, c("Total.Words", "Total.Characters", "Total.Sentences",
                             "Total.Paragraphs", "Pitch.Length..seconds."))

  # Test French text
  result_fr <- calculate_summary(text, reading_speed = 150, language = "FR")
  expect_s3_class(result_fr, "data.frame")
  expect_named(result_fr, c("Nombre.total.de.mots", "Nombre.total.de.caractères",
                            "Nombre.total.de.phrases", "Nombre.total.de.paragraphes",
                            "Durée.du.pitch..secondes."))

  # Test empty input
  empty_result <- calculate_summary("", 150, "ENG")
  expect_equal(nrow(empty_result), 1)
  expect_equal(ncol(empty_result), 5)
  expect_equal(empty_result$Total.Words, 0)
  expect_equal(empty_result$Total.Characters, 0)
})

test_that("construct_sentence generates correct output", {
  translations <- list(
    english_choices_map = list(opt1 = "First Option"),
    french_choices_map = list(opt1 = "Première Option")
  )

  # Test basic English output
  expect_equal(
    construct_sentence("opt1", "The option is", "L'option est", "ENG", FALSE, translations),
    "The option is First Option."
  )

  # Test empty input
  expect_equal(construct_sentence("", "Text is", "Le texte est", "ENG"), "")
  expect_equal(construct_sentence(NULL, "Text is", "Le texte est", "ENG"), "")
})

test_that("construct_sentence_niveau handles different inputs", {
  labels <- list(level1 = "Beginner")

  # Test basic output
  expect_equal(
    construct_sentence_niveau("level1", "Level is", "Le niveau est", "ENG", FALSE, labels),
    "Level is Beginner."
  )

  # Test empty input
  expect_equal(
    construct_sentence_niveau("", "Level is", "Le niveau est", "ENG", FALSE, labels),
    ""
  )
})

test_that("generate_input_field creates input elements", {
  ns <- NS("test")

  # Test text input existence
  text_input <- generate_input_field(ns, "text_id", "Text Label")
  expect_s3_class(text_input, "shiny.tag")

  # Test select input existence
  select_input <- generate_input_field(
    ns,
    "select_id",
    "Select Label",
    "select",
    choices = c("A", "B")
  )
  expect_s3_class(select_input, "shiny.tag")
})
