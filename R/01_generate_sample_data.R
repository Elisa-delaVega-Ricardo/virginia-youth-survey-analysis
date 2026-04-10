# ============================================================================
# 01_generate_sample_data.R
# ============================================================================
# Generates realistic sample data mirroring the CDC Youth Risk Behavior
# Survey (YRBS) and Virginia Youth Survey (VYS) data structures.
#
# Data structure based on:
#   - CDC YRBS: https://www.cdc.gov/yrbs/data/index.html
#   - VDOE VYS: https://www.doe.virginia.gov/data-policy-funding/
#     data-reports/data-collection/virginia-youth-survey
#
# The YRBS uses a complex survey design with strata, PSUs, and weights.
# This sample data includes these elements so the analysis pipeline
# demonstrates proper use of the survey package for weighted inference.
#
# IMPORTANT: Replace with real data — see R/00_download_real_data.R
# ============================================================================


library(tidyverse)

set.seed(2024)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  GENERATING SAMPLE SURVEY DATA\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# ── Configuration ──────────────────────────────────────────────────────────

n_respondents <- 3200  # Approximate Virginia YRBS sample size

# Survey years (YRBS is administered biennially in odd years)
survey_years <- c(2015, 2017, 2019, 2021, 2023)

# ── Generate Student-Level Survey Responses ────────────────────────────────
# This mirrors the raw YRBS microdata structure

generate_yrbs_responses <- function(year, n) {
  
  # Demographics
  sex <- sample(c("Female", "Male"), n, replace = TRUE, prob = c(0.50, 0.50))
  grade <- sample(c("9th", "10th", "11th", "12th"), n, replace = TRUE,
                  prob = c(0.28, 0.26, 0.24, 0.22))
  race_ethnicity <- sample(
    c("White", "Black or African American", "Hispanic/Latino",
      "Asian", "Two or More Races"),
    n, replace = TRUE, prob = c(0.48, 0.22, 0.16, 0.08, 0.06)
  )
  
  # Base probabilities (approximate Virginia 2023 YRBS results)
  # Source: CDC YRBS Data Summary & Trends Report 2013-2023
  # https://www.cdc.gov/yrbs/dstr/index.html
  
  # Demographic risk modifiers
  female_mod   <- ifelse(sex == "Female", 1.0, 0.0)
  senior_mod   <- ifelse(grade %in% c("11th", "12th"), 1.0, 0.0)
  minority_mod <- ifelse(race_ethnicity %in%
                           c("Black or African American", "Hispanic/Latino"), 1.0, 0.0)
  
  # Year trends (mental health worsening over time, substance use declining)
  year_mental_trend  <- (year - 2015) * 0.015  # Increasing
  year_substance_trend <- (year - 2015) * -0.008  # Decreasing
  
  # COVID spike for 2021
  covid_spike <- ifelse(year == 2021, 0.08, 0)
  
  # ── MENTAL HEALTH INDICATORS ──────────────────────────────────────────
  
  # Felt sad or hopeless (2+ weeks, past 12 months)
  p_sad <- pmin(0.95, pmax(0.05,
    0.37 + female_mod * 0.18 + year_mental_trend + covid_spike +
      minority_mod * 0.03 + rnorm(n, 0, 0.02)
  ))
  felt_sad_hopeless <- rbinom(n, 1, p_sad)
  
  # Seriously considered attempting suicide (past 12 months)
  p_suicide <- pmin(0.60, pmax(0.03,
    0.17 + female_mod * 0.12 + year_mental_trend * 0.5 + covid_spike * 0.5 +
      rnorm(n, 0, 0.02)
  ))
  considered_suicide <- rbinom(n, 1, p_suicide)
  
  # Attempted suicide (past 12 months)
  p_attempt <- pmin(0.30, pmax(0.02,
    0.08 + female_mod * 0.05 + minority_mod * 0.02 + covid_spike * 0.3 +
      rnorm(n, 0, 0.01)
  ))
  attempted_suicide <- rbinom(n, 1, p_attempt)
  
  # ── BULLYING & VIOLENCE ───────────────────────────────────────────────
  
  # Bullied at school (past 12 months)
  p_bullied <- pmin(0.50, pmax(0.05,
    0.19 + female_mod * 0.05 + year_mental_trend * -0.3 + rnorm(n, 0, 0.02)
  ))
  bullied_at_school <- rbinom(n, 1, p_bullied)
  
  # Electronically bullied (past 12 months)
  p_cyberbullied <- pmin(0.45, pmax(0.04,
    0.16 + female_mod * 0.08 + year_mental_trend * 0.3 + rnorm(n, 0, 0.02)
  ))
  electronically_bullied <- rbinom(n, 1, p_cyberbullied)
  
  # Missed school due to safety concerns (past 30 days)
  p_unsafe <- pmin(0.30, pmax(0.03,
    0.09 + minority_mod * 0.04 + covid_spike * 0.5 + year_mental_trend * 0.3 +
      rnorm(n, 0, 0.015)
  ))
  missed_school_safety <- rbinom(n, 1, p_unsafe)
  
  # ── SUBSTANCE USE ─────────────────────────────────────────────────────
  
  # Currently drink alcohol (past 30 days)
  p_alcohol <- pmin(0.60, pmax(0.05,
    0.29 + senior_mod * 0.08 + year_substance_trend + rnorm(n, 0, 0.02)
  ))
  current_alcohol <- rbinom(n, 1, p_alcohol)
  
  # Currently use marijuana (past 30 days)
  p_marijuana <- pmin(0.40, pmax(0.03,
    0.16 + senior_mod * 0.06 + year_substance_trend * 0.5 + rnorm(n, 0, 0.02)
  ))
  current_marijuana <- rbinom(n, 1, p_marijuana)
  
  # Currently use e-cigarettes/vaping (past 30 days)
  p_vaping <- pmin(0.50, pmax(0.02,
    ifelse(year < 2019, 0.10, 0.18) + senior_mod * 0.04 +
      ifelse(year >= 2021, -0.04, 0) + rnorm(n, 0, 0.02)
  ))
  current_vaping <- rbinom(n, 1, p_vaping)
  
  # ── SCHOOL CONNECTEDNESS (Likert scale 1-5) ──────────────────────────
  
  # "I feel like I belong at this school"
  school_belonging <- sample(1:5, n, replace = TRUE,
    prob = c(0.08, 0.12, 0.25, 0.32, 0.23))
  
  # "Teachers at this school care about me"
  teacher_caring <- sample(1:5, n, replace = TRUE,
    prob = c(0.06, 0.10, 0.22, 0.35, 0.27))
  
  # "I feel safe at this school"
  school_safety_perception <- sample(1:5, n, replace = TRUE,
    prob = c(0.05, 0.09, 0.20, 0.36, 0.30))
  
  # "Adults at school listen to students"
  adults_listen <- sample(1:5, n, replace = TRUE,
    prob = c(0.10, 0.14, 0.28, 0.28, 0.20))
  
  # ── SURVEY DESIGN VARIABLES ──────────────────────────────────────────
  # YRBS uses stratified cluster sampling
  
  stratum <- sample(1:15, n, replace = TRUE)  # Sampling strata
  psu <- paste0(stratum, "-", sample(1:5, n, replace = TRUE))  # Primary sampling units
  weight <- runif(n, 0.5, 2.5)  # Survey weights (simplified)
  
  tibble(
    survey_year = year,
    respondent_id = paste0("VA-", year, "-", sprintf("%04d", 1:n)),
    stratum = stratum,
    psu = psu,
    weight = round(weight, 4),
    sex = sex,
    grade = grade,
    race_ethnicity = race_ethnicity,
    # Mental health
    felt_sad_hopeless = felt_sad_hopeless,
    considered_suicide = considered_suicide,
    attempted_suicide = attempted_suicide,
    # Bullying & violence
    bullied_at_school = bullied_at_school,
    electronically_bullied = electronically_bullied,
    missed_school_safety = missed_school_safety,
    # Substance use
    current_alcohol = current_alcohol,
    current_marijuana = current_marijuana,
    current_vaping = current_vaping,
    # School connectedness (Likert 1-5: Strongly Disagree to Strongly Agree)
    school_belonging = school_belonging,
    teacher_caring = teacher_caring,
    school_safety_perception = school_safety_perception,
    adults_listen = adults_listen
  )
}

# Generate data for all survey years
cat("Generating student-level survey responses...\n")
survey_data <- map_dfr(survey_years, ~generate_yrbs_responses(.x, n_respondents))

cat(sprintf("  ✓ %s total respondents across %d survey years\n",
            format(nrow(survey_data), big.mark = ","), length(survey_years)))

# ── Generate Aggregated Summary (VYS-style) ────────────────────────────────
# This mirrors how VDOE publishes VYS results: aggregated prevalence rates

cat("Generating aggregated prevalence tables...\n")

binary_vars <- c("felt_sad_hopeless", "considered_suicide", "attempted_suicide",
                 "bullied_at_school", "electronically_bullied",
                 "missed_school_safety", "current_alcohol",
                 "current_marijuana", "current_vaping")

# By year and sex
prevalence_by_sex <- survey_data %>%
  group_by(survey_year, sex) %>%
  summarise(
    n = n(),
    across(all_of(binary_vars), ~round(mean(.) * 100, 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = all_of(binary_vars),
               names_to = "indicator", values_to = "prevalence_pct") %>%
  mutate(demographic_category = "Sex", demographic_group = sex) %>%
  select(survey_year, demographic_category, demographic_group, n,
         indicator, prevalence_pct)

# By year and race/ethnicity
prevalence_by_race <- survey_data %>%
  group_by(survey_year, race_ethnicity) %>%
  summarise(
    n = n(),
    across(all_of(binary_vars), ~round(mean(.) * 100, 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = all_of(binary_vars),
               names_to = "indicator", values_to = "prevalence_pct") %>%
  mutate(demographic_category = "Race/Ethnicity",
         demographic_group = race_ethnicity) %>%
  select(survey_year, demographic_category, demographic_group, n,
         indicator, prevalence_pct)

# By year and grade
prevalence_by_grade <- survey_data %>%
  group_by(survey_year, grade) %>%
  summarise(
    n = n(),
    across(all_of(binary_vars), ~round(mean(.) * 100, 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = all_of(binary_vars),
               names_to = "indicator", values_to = "prevalence_pct") %>%
  mutate(demographic_category = "Grade", demographic_group = grade) %>%
  select(survey_year, demographic_category, demographic_group, n,
         indicator, prevalence_pct)

# Overall by year
prevalence_overall <- survey_data %>%
  group_by(survey_year) %>%
  summarise(
    n = n(),
    across(all_of(binary_vars), ~round(mean(.) * 100, 1)),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = all_of(binary_vars),
               names_to = "indicator", values_to = "prevalence_pct") %>%
  mutate(demographic_category = "Overall", demographic_group = "All Students") %>%
  select(survey_year, demographic_category, demographic_group, n,
         indicator, prevalence_pct)

# Combine all aggregated prevalence data
prevalence_summary <- bind_rows(
  prevalence_overall, prevalence_by_sex, prevalence_by_race, prevalence_by_grade
)

# ── Generate Likert Summary ────────────────────────────────────────────────

cat("Generating school connectedness Likert summaries...\n")

likert_vars <- c("school_belonging", "teacher_caring",
                 "school_safety_perception", "adults_listen")

likert_labels <- c("1" = "Strongly Disagree", "2" = "Disagree",
                   "3" = "Neutral", "4" = "Agree", "5" = "Strongly Agree")

likert_summary <- survey_data %>%
  group_by(survey_year) %>%
  summarise(
    n = n(),
    across(all_of(likert_vars), list(
      mean = ~round(mean(.), 2),
      sd   = ~round(sd(.), 2),
      pct_agree = ~round(mean(. >= 4) * 100, 1)  # % Agree or Strongly Agree
    )),
    .groups = "drop"
  )

# ── Save All Datasets ─────────────────────────────────────────────────────

cat("Saving datasets...\n\n")

write_csv(survey_data, "data/yrbs_student_responses.csv")
write_csv(prevalence_summary, "data/prevalence_summary.csv")
write_csv(likert_summary, "data/likert_connectedness_summary.csv")

cat("  ✓ yrbs_student_responses.csv:",
    format(nrow(survey_data), big.mark = ","), "student records\n")
cat("  ✓ prevalence_summary.csv:",
    format(nrow(prevalence_summary), big.mark = ","),
    "aggregated prevalence records\n")
cat("  ✓ likert_connectedness_summary.csv:",
    nrow(likert_summary), "year summaries\n")
cat("\n  Files saved to: data/\n")
cat("  💡 Replace with real CDC/VDOE data — see R/00_download_real_data.R\n")
