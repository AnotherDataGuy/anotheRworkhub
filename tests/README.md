Tests and Coverage
================
11 janvier, 2025 23:09:36

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

    ## - Not All Tests Passed
    ##   Coverage statistics are approximations of the non-failing tests.
    ##   Use with caution
    ## 
    ##  For further investigation check in testthat summary tables.

| Object                                                                                | Coverage (%) |
|:--------------------------------------------------------------------------------------|:------------:|
| anotheRworkhub                                                                        |    18.90     |
| [R/app_config.R](../R/app_config.R)                                                   |     0.00     |
| [R/app_server.R](../R/app_server.R)                                                   |     0.00     |
| [R/app_ui.R](../R/app_ui.R)                                                           |     0.00     |
| [R/fct_interact_with_gpt_api_only_text.R](../R/fct_interact_with_gpt_api_only_text.R) |     0.00     |
| [R/mod_section_interview_simulator.R](../R/mod_section_interview_simulator.R)         |     0.00     |
| [R/run_app.R](../R/run_app.R)                                                         |     0.00     |
| [R/utils_translations.R](../R/utils_translations.R)                                   |     0.00     |
| [R/mod_section_pitch_improver.R](../R/mod_section_pitch_improver.R)                   |    28.16     |
| [R/fct_helpers_pitch_improver.R](../R/fct_helpers_pitch_improver.R)                   |    69.44     |
| [R/fct_helpers_interview_simulator.R](../R/fct_helpers_interview_simulator.R)         |    72.66     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                                                              |   n | time | error | failed | skipped | warning |
|:--------------------------------------------------------------------------------------------------|----:|-----:|------:|-------:|--------:|--------:|
| [test-fct_helpers_interview_simulator.R](testthat/test-fct_helpers_interview_simulator.R)         |   0 | 0.03 |     1 |      0 |       0 |       0 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R)                   |  21 | 0.09 |     0 |      0 |       0 |       0 |
| [test-fct_interact_with_gpt_api_only_text.R](testthat/test-fct_interact_with_gpt_api_only_text.R) |   1 | 0.02 |     0 |      0 |       0 |       0 |
| [test-mod_section_interview_simulator.R](testthat/test-mod_section_interview_simulator.R)         |   1 | 0.01 |     0 |      0 |       0 |       0 |
| [test-mod_section_pitch_improver.R](testthat/test-mod_section_pitch_improver.R)                   |   5 | 0.15 |     0 |      0 |       0 |       0 |
| [test-utils_translations.R](testthat/test-utils_translations.R)                                   |   1 | 0.01 |     0 |      0 |       0 |       0 |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file                                                                                                 | context                             | test                                                     | status |   n | time |
|:-----------------------------------------------------------------------------------------------------|:------------------------------------|:---------------------------------------------------------|:-------|----:|-----:|
| [test-fct_helpers_interview_simulator.R](testthat/test-fct_helpers_interview_simulator.R#L24)        | fct_helpers_interview_simulator     | process_message handles the complete message flow        | ERROR  |   0 | 0.03 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R#L3)                   | fct_helpers_pitch_improver          | renderMarkdown handles different input formats correctly | PASS   |   6 | 0.03 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R#L28)                  | fct_helpers_pitch_improver          | calculate_summary handles text analysis correctly        | PASS   |   8 | 0.03 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R#L54_L57)              | fct_helpers_pitch_improver          | construct_sentence generates correct output              | PASS   |   3 | 0.02 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R#L68_L71)              | fct_helpers_pitch_improver          | construct_sentence_niveau handles different inputs       | PASS   |   2 | 0.01 |
| [test-fct_helpers_pitch_improver.R](testthat/test-fct_helpers_pitch_improver.R#L85)                  | fct_helpers_pitch_improver          | generate_input_field creates input elements              | PASS   |   2 | 0.00 |
| [test-fct_interact_with_gpt_api_only_text.R](testthat/test-fct_interact_with_gpt_api_only_text.R#L2) | fct_interact_with_gpt_api_only_text | multiplication works                                     | PASS   |   1 | 0.02 |
| [test-mod_section_interview_simulator.R](testthat/test-mod_section_interview_simulator.R#L2)         | mod_section_interview_simulator     | multiplication works                                     | PASS   |   1 | 0.01 |
| [test-mod_section_pitch_improver.R](testthat/test-mod_section_pitch_improver.R#L4)                   | mod_section_pitch_improver          | module ui works                                          | PASS   |   2 | 0.02 |
| [test-mod_section_pitch_improver.R](testthat/test-mod_section_pitch_improver.R#L45)                  | mod_section_pitch_improver          | module server works                                      | PASS   |   3 | 0.13 |
| [test-utils_translations.R](testthat/test-utils_translations.R#L2)                                   | utils_translations                  | multiplication works                                     | PASS   |   1 | 0.01 |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                             |
|:---------|:----------------------------------|
| Version  | R version 4.4.0 (2024-04-24 ucrt) |
| Platform | x86_64-w64-mingw32/x64            |
| Running  | Windows 11 x64 (build 22631)      |
| Language | French_France                     |
| Timezone | Europe/Paris                      |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.2   |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>
<!--- Final Status : error/failed --->
