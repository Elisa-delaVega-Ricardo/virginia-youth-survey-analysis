# ============================================================================
# 00_download_real_data.R
# ============================================================================
# Instructions for downloading real survey data from CDC and VDOE.
#
# DATA SOURCES:
#   1. CDC Youth Risk Behavior Survey (YRBS) — Virginia state data
#   2. Virginia Youth Survey (VYS) — Published by VDOE
#   3. Virginia School Climate Survey — Published by VDOE
# ============================================================================

cat("
╔══════════════════════════════════════════════════════════════════════╗
║  DOWNLOADING REAL SURVEY DATA                                       ║
║  Replace sample data with actual CDC YRBS and VDOE VYS data         ║
╚══════════════════════════════════════════════════════════════════════╝

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

DATASET 1: CDC Youth Risk Behavior Survey (YRBS) — Virginia Data
Source: Centers for Disease Control and Prevention

  OPTION A — YRBS Explorer (easiest for Virginia-specific data):
  1. Go to: https://yrbs-explorer.services.cdc.gov/
  2. Select 'State' → 'Virginia'
  3. Select topics of interest (e.g., Mental Health, Violence, Substance Use)
  4. Export results as CSV
  5. Save to: data/raw/

  OPTION B — Full YRBS dataset download:
  1. Go to: https://www.cdc.gov/yrbs/data/index.html
  2. Under 'National YRBSS Datasets and Documentation', download the
     most recent year (2023)
  3. The combined state dataset includes Virginia
  4. Available in Access (.mdb) or ASCII format
  5. Use the SAS/SPSS syntax files provided to convert ASCII data
  6. Filter for state = 'Virginia' (sitecode = 'VA')

  OPTION C — YRBS Data Summary & Trends:
  1. Go to: https://www.cdc.gov/yrbs/dstr/index.html
  2. Pre-created tables with 10-year trend data available
  3. Useful for trend analysis without raw data processing

  KEY VARIABLES IN THE YRBS DATASET:
  ─────────────────────────────────
  • sitecode     — State code ('VA' for Virginia)
  • sex          — Male / Female
  • race4        — White, Black, Hispanic, All Other
  • grade        — 9, 10, 11, 12
  • weight       — Survey weight for population estimates
  • stratum, PSU — For complex survey design (use with 'survey' package)
  • qn26         — Felt sad or hopeless (2+ weeks)
  • qn17         — Were bullied at school
  • qn46         — Currently use marijuana
  • qn41         — Currently drink alcohol
  • qn29         — Attempted suicide
  • qn89         — Missed school due to safety concerns

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

DATASET 2: Virginia Youth Survey (VYS)
Source: Virginia Department of Education

  1. Go to: https://www.doe.virginia.gov/data-policy-funding/
     data-reports/data-collection/virginia-youth-survey
  2. VYS is administered biennially to Virginia high school students
  3. Results are published in summary reports (PDF and Excel)
  4. Topics covered: substance use, bullying, mental health,
     school safety, school connectedness
  5. Save any downloadable files to: data/raw/

  NOTE: VYS results are often published as aggregated percentages
  by region, not raw student-level data. The sample data generator
  in this project mirrors this aggregated format.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

DATASET 3: Virginia School Climate Survey
Source: VDOE

  1. Go to: https://www.doe.virginia.gov/programs-services/
     school-operations-support-services/safety-crisis-management
  2. School climate survey data includes student perceptions of
     safety, engagement, and school environment
  3. Save to: data/raw/

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

After downloading, update file paths in R/02_data_preparation.R
to point to your real data files.
")

# ── Example code for reading real YRBS data (uncomment when ready) ─────
#
# library(tidyverse)
# library(survey)
# library(haven)  # for reading SAS/SPSS files
#
# # Reading the combined state YRBS dataset
# yrbs_raw <- read_sas("data/raw/sadc_2023.sas7bdat")
#
# # Filter for Virginia
# virginia_yrbs <- yrbs_raw %>%
#   filter(sitecode == "VA")
#
# # Set up complex survey design (required for valid inference)
# yrbs_design <- svydesign(
#   id      = ~PSU,
#   strata  = ~stratum,
#   weights = ~weight,
#   data    = virginia_yrbs,
#   nest    = TRUE
# )
#
# # Example: Weighted prevalence of persistent sadness
# svymean(~qn26, yrbs_design, na.rm = TRUE)
