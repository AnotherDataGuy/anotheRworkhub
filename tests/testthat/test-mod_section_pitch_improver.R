test_that("module ui works", {
  ui <- mod_section_pitch_improver_ui(id = "test")
  # Changed expectation to check for shiny.tag since we're returning a tabItem
  expect_true(inherits(ui, "shiny.tag"))

  # Check that formals have not been removed
  fmls <- formals(mod_section_pitch_improver_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module server works", {
  # Create mock translations
  mock_translations <- list(
    english_choices_map = c(
      "spontaneous_application" = "Spontaneous application"
    ),
    french_choices_map = c(
      "spontaneous_application" = "Candidature spontanée"
    ),
    hierarchical_status_labels_en = c(
      "entry_level" = "Entry Level / Junior"
    ),
    hierarchical_status_labels_fr = c(
      "entry_level" = "Niveau d'entrée / Débutant"
    ),
    expect_choices_en = c(
      "High" = "High"
    ),
    expect_choices_fr = c(
      "Elevé" = "Elevé"
    )
  )

  testServer(
    mod_section_pitch_improver_server,
    args = list(
      api_pwd = "dummy_key",
      language_input = reactive({ "ENG" }),
      translations = mock_translations
    ),
    {
      # Basic server function tests
      expect_true(inherits(ns, "function"))
      expect_true(grepl(id, ns("")))
      expect_true(grepl("test", ns("test")))
    }
  )
})
