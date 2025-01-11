#' Translation and Label Mappings
#'
#' @description Provides predefined mappings for translations and labels used in the app.
#'
#' @details
#' This function returns a list of mappings and labels:
#' - `english_choices_map`: Mappings for English choices.
#' - `french_choices_map`: Mappings for French choices.
#' - `hierarchical_status_labels_en`: Labels for hierarchical status in English.
#' - `hierarchical_status_labels_fr`: Labels for hierarchical status in French.
#' - `expect_choices_en`: Choices for expectations in English.
#' - `expect_choices_fr`: Choices for expectations in French.
#'
#' @return A list containing all the mappings.
#' @noRd
get_translations <- function() {
  list(
    # Choices Maps
    english_choices_map = c(
      "spontaneous_application" = "Spontaneous application",
      "offer_reply" = "Reply to an offer",
      "phone_screening" = "Phone Screening",
      "first_formal_meeting" = "First Formal Meeting",
      "first_informal_meeting" = "First Informal Meeting",
      "networking_event" = "Networking Event",
      "followup_after_networking" = "Follow-up After Networking Event",
      "linkedin_message" = "LinkedIn Message",
      "referral_introduction" = "Referral Introduction",
      "one_on_one_interview" = "One-on-One Interview",
      "job_offer_acceptance" = "Job Offer Acceptance",
      "job_offer_clarification" = "Job Offer Clarification",
      "rejecting_job_offer" = "Rejecting a Job Offer",
      "asking_for_feedback" = "Asking for Feedback",
      "followup_after_interview" = "Follow-up After Interview"
    ),

    french_choices_map = c(
      "spontaneous_application" = "Candidature spontanée",
      "offer_reply" = "Réponse à une offre",
      "phone_screening" = "Entretien téléphonique",
      "first_formal_meeting" = "Première réunion formelle",
      "first_informal_meeting" = "Première réunion informelle",
      "networking_event" = "Événement de réseautage",
      "followup_after_networking" = "Suivi après un événement de réseautage",
      "linkedin_message" = "Message LinkedIn",
      "referral_introduction" = "Introduction par référence",
      "one_on_one_interview" = "Entretien individuel",
      "job_offer_acceptance" = "Acceptation d'offre d'emploi",
      "job_offer_clarification" = "Clarification d'offre d'emploi",
      "rejecting_job_offer" = "Refus d'offre d'emploi",
      "asking_for_feedback" = "Demande de retour d'information",
      "followup_after_interview" = "Suivi après l'entretien"
    ),

    # Hierarchical Status Labels
    hierarchical_status_labels_en = c(
      "entry_level" = "Entry Level / Junior",
      "manager" = "Manager / Supervisor",
      "senior_manager" = "Senior Manager / Department Head",
      "director" = "Director / Vice President",
      "ceo" = "CEO / Executive"
    ),

    hierarchical_status_labels_fr = c(
      "entry_level" = "Niveau d'entrée / Débutant",
      "manager" = "Manager / Superviseur",
      "senior_manager" = "Manager Senior / Chef de département",
      "director" = "Directeur / Vice-Président",
      "ceo" = "PDG / Exécutif"
    ),

    # Expectation Choices
    expect_choices_en = c(
      "High" = "High",
      "Very high" = "Very high",
      "Exceptionally High Expectations" = "Exceptionally High Expectations"
    ),

    expect_choices_fr = c(
      "Elevé" = "Elevé",
      "Très élevé" = "Très élevé",
      "Exceptionnellement élevé" = "Exceptionnellement élevé"
    )
  )
}
