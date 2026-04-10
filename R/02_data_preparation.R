# ============================================================================
# 02_data_preparation.R
# ============================================================================
# Loads, validates, cleans, and prepares survey data for analysis.
# Configures the complex survey design using the 'survey' package.
#
# Input:  data/yrbs_student_responses.csv
#         data/prevalence_summary.csv
#         data/likert_connectedness_summary.csv
#
# Output: data/survey_design.rds    — svydesign object for weighted analysis
#         data/survey_clean.rds     — Cleaned student-level data
#         data/prevalence_clean.rds — Cleaned aggregated prevalence data
#         output/tables/data_quality_report.csv
# ============================================================================

library(tidyverse)
library(survey)
library(janitor)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 1: DATA PREPARATION & SURVEY DESIGN\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# ── 1.1 Load Raw Data ─────────────────────────────────────────────────────

cat("Loading datasets...\n")

students   <- read_csv("data/yrbs_student_responses.csv", show_col_types = FALSE)
prevalence <- read_csv("data/prevalence_summary.csv", show_col_types = FALSE)
likert     <- read_csv("data/likert_connectedness_summary.csv", show_col_types = FALSE)

cat(sprintf("  ✓ Student responses: %s records, %d variables\n",
            format(nrow(students), big.mark = ","), ncol(students)))
cat(sprintf("  ✓ Prevalence summary: %s records\n",
            format(nrow(prevalence), big.mark = ",")))
cat(sprintf("  ✓ Likert summary: %d year records\n", nrow(likert)))

# ── 1.2 Data Quality Checks ───────────────────────────────────────────────

cat("\nRunning data quality checks...\n")

quality <- tibble(
  check = c(
    "Total respondents",
    "Survey years present",
    "Missing values (binary indicators)",
    "Missing values (Likert items)",
    "Sex categories",
    "Race/ethnicity categories",
    "Grade levels",
    "Weight range",
    "Unique strata",
    "Unique PSUs"
  ),
  result = c(
    format(nrow(students), big.mark = ","),
    paste(sort(unique(students$survey_year)), collapse = ", "),
    as.character(sum(is.na(select(students, felt_sad_hopeless:current_vaping)))),
    as.character(sum(is.na(select(students, school_belonging:adults_listen)))),
    paste(sort(unique(students$sex)), collapse = ", "),
    paste(sort(unique(students$race_ethnicity)), collapse = ", "),
    paste(sort(unique(students$grade)), collapse = ", "),
    sprintf("%.3f – %.3f", min(students$weight), max(students$weight)),
    as.character(n_distinct(students$stratum)),
    as.character(n_distinct(students$psu))
  ),
  status = c("INFO", "INFO", 
             ifelse(sum(is.na(select(students, felt_sad_hopeless:current_vaping))) == 0, "PASS", "FLAG"),
             ifelse(sum(is.na(select(students, school_belonging:adults_listen))) == 0, "PASS", "FLAG"),
             "INFO", "INFO", "INFO", "INFO", "INFO", "INFO")
)

cat("\n  Data Quality Report:\n")
cat("  ", paste(rep("-", 65), collapse = ""), "\n")
for (i in 1:nrow(quality)) {
  icon <- case_when(quality$status[i] == "PASS" ~ "✓",
                    quality$status[i] == "FLAG" ~ "⚠", TRUE ~ "ℹ")
  cat(sprintf("  %s %-40s %s\n", icon, quality$check[i], quality$result[i]))
}

write_csv(quality, "output/tables/data_quality_report.csv")

# ── 1.3 Clean and Recode Variables ─────────────────────────────────────────

cat("\nCleaning and recoding variables...\n")

students_clean <- students %>%
  
  clean_names() %>%
  mutate(
    # Factor ordering for demographics
    sex = factor(sex, levels = c("Female", "Male")),
    grade = factor(grade, levels = c("9th", "10th", "11th", "12th")),
    race_ethnicity = factor(race_ethnicity),
    
    # Create Likert labels for school connectedness
    belonging_label = factor(school_belonging,
      levels = 1:5,
      labels = c("Strongly Disagree", "Disagree", "Neutral",
                 "Agree", "Strongly Agree")),
    caring_label = factor(teacher_caring,
      levels = 1:5,
      labels = c("Strongly Disagree", "Disagree", "Neutral",
                 "Agree", "Strongly Agree")),
    safety_label = factor(school_safety_perception,
      levels = 1:5,
      labels = c("Strongly Disagree", "Disagree", "Neutral",
                 "Agree", "Strongly Agree")),
    listening_label = factor(adults_listen,
      levels = 1:5,
      labels = c("Strongly Disagree", "Disagree", "Neutral",
                 "Agree", "Strongly Agree")),
    
    # Composite risk score (number of risk behaviors endorsed)
    mental_health_risk = felt_sad_hopeless + considered_suicide + attempted_suicide,
    behavioral_risk = current_alcohol + current_marijuana + current_vaping,
    total_risk_score = mental_health_risk + behavioral_risk +
      bullied_at_school + electronically_bullied + missed_school_safety,
    
    # School connectedness composite (mean of 4 Likert items)
    connectedness_score = (school_belonging + teacher_caring +
                             school_safety_perception + adults_listen) / 4,
    
    # Binary: high connectedness (mean >= 4 = Agree or higher)
    high_connectedness = as.integer(connectedness_score >= 4),
    
    # High risk flag (3+ risk behaviors)
    high_risk = as.integer(total_risk_score >= 3)
  )

# ── 1.4 Configure Complex Survey Design ───────────────────────────────────

cat("Configuring complex survey design (stratified cluster sampling)...\n")

# The YRBS uses a two-stage cluster sample design:
#   Stage 1: Schools selected with probability proportional to enrollment
#   Stage 2: Classes randomly selected within schools
# Survey weights account for nonresponse and oversampling

# Full dataset design
survey_design <- svydesign(
  id      = ~psu,           # Primary sampling unit (school-class)
  strata  = ~stratum,       # Sampling stratum
  weights = ~weight,        # Survey weight
  data    = students_clean,
  nest    = TRUE            # PSUs nested within strata
)

cat("  ✓ Survey design configured\n")
cat(sprintf("    Design: Stratified cluster sample\n"))
cat(sprintf("    Strata: %d | PSUs: %d | Observations: %s\n",
            length(unique(students_clean$stratum)),
            length(unique(students_clean$psu)),
            format(nrow(students_clean), big.mark = ",")))

# Per-year designs (for year-specific weighted estimates)
survey_designs_by_year <- list()
for (yr in unique(students_clean$survey_year)) {
  yr_data <- filter(students_clean, survey_year == yr)
  survey_designs_by_year[[as.character(yr)]] <- svydesign(
    id = ~psu, strata = ~stratum, weights = ~weight,
    data = yr_data, nest = TRUE
  )
}

# ── 1.5 Save Prepared Data ────────────────────────────────────────────────

cat("Saving analysis-ready datasets...\n")

saveRDS(students_clean, "data/survey_clean.rds")
saveRDS(survey_design, "data/survey_design.rds")
saveRDS(survey_designs_by_year, "data/survey_designs_by_year.rds")
saveRDS(prevalence, "data/prevalence_clean.rds")

cat("\n  ✓ survey_clean.rds           — Cleaned student data\n")
cat("  ✓ survey_design.rds          — svydesign object (full)\n")
cat("  ✓ survey_designs_by_year.rds — svydesign objects by year\n")
cat("  ✓ prevalence_clean.rds       — Aggregated prevalence\n")

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  ✅ DATA PREPARATION COMPLETE\n")
cat("══════════════════════════════════════════════════════════════════\n")
