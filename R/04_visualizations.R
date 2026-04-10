# ============================================================================
# 04_visualizations.R
# ============================================================================
# Publication-quality visualizations for the Virginia Youth Survey analysis.
# Includes Likert-scale plots, prevalence trends, demographic comparisons,
# and logistic regression odds ratio forest plots.
#
# Output: output/figures/ (10 PNG files, 300 DPI)
# ============================================================================

library(tidyverse)
library(scales)
library(broom)

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  PHASE 3: GENERATING VISUALIZATIONS\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

# ── Load Data ──────────────────────────────────────────────────────────────

df <- readRDS("data/survey_clean.rds")
latest <- filter(df, survey_year == 2023)

# ── Theme ──────────────────────────────────────────────────────────────────

VDOE_BLUE     <- "#003366"
ALERT_RED     <- "#CC3333"
SUCCESS_GREEN <- "#2E8B57"
ACCENT_ORANGE <- "#FF8C00"
PURPLE        <- "#6A5ACD"

theme_survey <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, color = VDOE_BLUE),
    plot.subtitle = element_text(size = 11, color = "gray40", margin = margin(b = 15)),
    plot.caption  = element_text(size = 9, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(15, 20, 10, 15)
  )

save_fig <- function(filename, width = 10, height = 7) {
  ggsave(paste0("output/figures/", filename),
         width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("  ✓ %s\n", filename))
}

indicator_labels <- c(
  "felt_sad_hopeless"      = "Persistent Sadness",
  "considered_suicide"     = "Considered Suicide",
  "attempted_suicide"      = "Attempted Suicide",
  "bullied_at_school"      = "Bullied at School",
  "electronically_bullied" = "Cyberbullied",
  "missed_school_safety"   = "Missed School\n(Safety)",
  "current_alcohol"        = "Current Alcohol",
  "current_marijuana"      = "Current Marijuana",
  "current_vaping"         = "Current Vaping"
)

indicators <- names(indicator_labels)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 1: Prevalence Overview (Horizontal Bar — 2023)
# ══════════════════════════════════════════════════════════════════════════

prev_2023 <- latest %>%
  summarise(across(all_of(indicators), ~round(mean(.) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "pct") %>%
  mutate(
    label = indicator_labels[indicator],
    category = case_when(
      indicator %in% c("felt_sad_hopeless", "considered_suicide",
                        "attempted_suicide") ~ "Mental Health",
      indicator %in% c("bullied_at_school", "electronically_bullied",
                        "missed_school_safety") ~ "Safety & Bullying",
      TRUE ~ "Substance Use"
    ),
    label = fct_reorder(label, pct)
  )

ggplot(prev_2023, aes(x = pct, y = label, fill = category)) +
  geom_col(alpha = 0.85, width = 0.65) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.15,
            fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c("Mental Health" = ALERT_RED,
                                "Safety & Bullying" = ACCENT_ORANGE,
                                "Substance Use" = VDOE_BLUE),
                    name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Virginia Youth Risk Behavior Prevalence (2023)",
       subtitle = "Percentage of high school students reporting each behavior or experience",
       x = "Prevalence (%)", y = NULL,
       caption = "Source: CDC Youth Risk Behavior Survey (YRBS) | Virginia data") +
  theme_survey + theme(panel.grid.major.y = element_blank())

save_fig("01_prevalence_overview_2023.png", width = 11, height = 7)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 2: Mental Health Trends (2015–2023 Line Chart)
# ══════════════════════════════════════════════════════════════════════════

mental_trends <- df %>%
  group_by(survey_year) %>%
  summarise(
    `Persistent Sadness`   = mean(felt_sad_hopeless) * 100,
    `Considered Suicide`   = mean(considered_suicide) * 100,
    `Attempted Suicide`    = mean(attempted_suicide) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-survey_year, names_to = "indicator", values_to = "pct")

ggplot(mental_trends, aes(x = survey_year, y = pct,
                           color = indicator, group = indicator)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("Persistent Sadness" = ALERT_RED,
                                 "Considered Suicide" = ACCENT_ORANGE,
                                 "Attempted Suicide" = PURPLE), name = NULL) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  annotate("rect", xmin = 2020, xmax = 2022, ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = ALERT_RED) +
  annotate("text", x = 2021, y = 5, label = "COVID-19",
           color = ALERT_RED, fontface = "bold", size = 3.5) +
  labs(title = "Youth Mental Health Trends in Virginia (2015–2023)",
       subtitle = "Persistent sadness has been rising steadily, with a spike during the pandemic.",
       x = "Survey Year", y = "Prevalence (%)",
       caption = "Source: CDC YRBS | Surveys administered biennially") +
  theme_survey + theme(legend.position = "right")

save_fig("02_mental_health_trends.png")

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 3: Gender Disparities (Grouped Bar)
# ══════════════════════════════════════════════════════════════════════════

gender_data <- latest %>%
  group_by(sex) %>%
  summarise(across(all_of(indicators), ~mean(.) * 100), .groups = "drop") %>%
  pivot_longer(-sex, names_to = "indicator", values_to = "pct") %>%
  mutate(label = indicator_labels[indicator])

ggplot(gender_data, aes(x = label, y = pct, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("Female" = ALERT_RED, "Male" = VDOE_BLUE),
                    name = NULL) +
  labs(title = "Youth Risk Behaviors by Sex (Virginia, 2023)",
       subtitle = "Female students report higher rates of mental health concerns and bullying.\nMale students show higher substance use in some categories.",
       x = NULL, y = "Prevalence (%)",
       caption = "Source: CDC YRBS | Chi-square tests confirm significant gender differences") +
  theme_survey + theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

save_fig("03_gender_disparities.png", width = 13, height = 7)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 4: Substance Use Trends (2015–2023)
# ══════════════════════════════════════════════════════════════════════════

substance_trends <- df %>%
  group_by(survey_year) %>%
  summarise(
    Alcohol   = mean(current_alcohol) * 100,
    Marijuana = mean(current_marijuana) * 100,
    `E-Cigarettes / Vaping` = mean(current_vaping) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-survey_year, names_to = "substance", values_to = "pct")

ggplot(substance_trends, aes(x = survey_year, y = pct,
                              color = substance, group = substance)) +
  geom_line(linewidth = 1.8) + geom_point(size = 4) +
  scale_color_manual(values = c("Alcohol" = VDOE_BLUE,
                                 "Marijuana" = SUCCESS_GREEN,
                                 "E-Cigarettes / Vaping" = ACCENT_ORANGE),
                     name = NULL) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Substance Use Trends Among Virginia Youth (2015–2023)",
       subtitle = "Alcohol use has declined steadily. Vaping spiked in 2019 before declining.",
       x = "Survey Year", y = "Current Use (%)",
       caption = "Source: CDC YRBS | 'Current use' = past 30 days") +
  theme_survey + theme(legend.position = "right")

save_fig("04_substance_use_trends.png")

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 5: School Connectedness Likert Distribution
# ══════════════════════════════════════════════════════════════════════════

likert_data <- latest %>%
  select(belonging_label, caring_label, safety_label, listening_label) %>%
  rename(
    `I feel like I belong\nat this school` = belonging_label,
    `Teachers care\nabout me`              = caring_label,
    `I feel safe\nat this school`          = safety_label,
    `Adults listen\nto students`           = listening_label
  ) %>%
  pivot_longer(everything(), names_to = "item", values_to = "response") %>%
  count(item, response) %>%
  group_by(item) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

likert_colors <- c("Strongly Disagree" = "#d73027", "Disagree" = "#fc8d59",
                   "Neutral" = "#ffffbf", "Agree" = "#91bfdb",
                   "Strongly Agree" = "#4575b4")

ggplot(likert_data, aes(x = pct, y = item, fill = response)) +
  geom_col(position = "stack", width = 0.65) +
  scale_fill_manual(values = likert_colors, name = NULL,
                    breaks = c("Strongly Disagree", "Disagree", "Neutral",
                               "Agree", "Strongly Agree")) +
  labs(title = "School Connectedness: Student Perceptions (Virginia, 2023)",
       subtitle = "Likert-scale responses from Virginia high school students",
       x = "Percentage of Respondents (%)", y = NULL,
       caption = "Source: Virginia Youth Survey / CDC YRBS | Scale: 1 (Strongly Disagree) to 5 (Strongly Agree)") +
  theme_survey + theme(panel.grid.major.y = element_blank())

save_fig("05_likert_school_connectedness.png", width = 12, height = 6)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 6: Connectedness as Protective Factor
# ══════════════════════════════════════════════════════════════════════════

protect_data <- latest %>%
  mutate(connectedness_group = ifelse(high_connectedness == 1,
                                      "High Connectedness", "Low Connectedness")) %>%
  group_by(connectedness_group) %>%
  summarise(across(all_of(indicators[1:6]), ~mean(.) * 100), .groups = "drop") %>%
  pivot_longer(-connectedness_group, names_to = "indicator", values_to = "pct") %>%
  mutate(label = indicator_labels[indicator])

ggplot(protect_data, aes(x = label, y = pct, fill = connectedness_group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("High Connectedness" = SUCCESS_GREEN,
                                "Low Connectedness" = ALERT_RED), name = NULL) +
  labs(title = "School Connectedness as a Protective Factor",
       subtitle = "Students who feel connected to school report lower rates of every risk indicator.",
       x = NULL, y = "Prevalence (%)",
       caption = "Source: CDC YRBS | High connectedness = mean Likert score ≥ 4 (Agree or higher)") +
  theme_survey + theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_fig("06_connectedness_protective_factor.png", width = 12, height = 7)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 7: Race/Ethnicity Comparison (Heatmap)
# ══════════════════════════════════════════════════════════════════════════

race_data <- latest %>%
  group_by(race_ethnicity) %>%
  summarise(across(all_of(indicators), ~round(mean(.) * 100, 1)),
            .groups = "drop") %>%
  pivot_longer(-race_ethnicity, names_to = "indicator", values_to = "pct") %>%
  mutate(label = indicator_labels[indicator])

ggplot(race_data, aes(x = label, y = race_ethnicity, fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(pct, "%")), size = 3.2, fontface = "bold") +
  scale_fill_gradient2(low = SUCCESS_GREEN, mid = "#FFFFCC", high = ALERT_RED,
                       midpoint = 25, name = "Prevalence (%)") +
  labs(title = "Youth Risk Behavior Prevalence by Race/Ethnicity (Virginia, 2023)",
       subtitle = "Darker red indicates higher prevalence of the risk behavior or experience",
       x = NULL, y = NULL,
       caption = "Source: CDC YRBS") +
  theme_survey +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
        panel.grid = element_blank())

save_fig("07_race_ethnicity_heatmap.png", width = 14, height = 7)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 8: Odds Ratio Forest Plot (Logistic Regression)
# ══════════════════════════════════════════════════════════════════════════

model_1 <- glm(felt_sad_hopeless ~ sex + grade + race_ethnicity +
                 bullied_at_school + connectedness_score,
               data = latest, family = binomial)

or_data <- tidy(model_1, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "sexMale" ~ "Male (vs Female)",
      term == "grade10th" ~ "10th Grade (vs 9th)",
      term == "grade11th" ~ "11th Grade (vs 9th)",
      term == "grade12th" ~ "12th Grade (vs 9th)",
      str_detect(term, "race") ~ str_replace(term, "race_ethnicity", ""),
      term == "bullied_at_school" ~ "Bullied at School",
      term == "connectedness_score" ~ "School Connectedness Score",
      TRUE ~ term
    ),
    significant = p.value < 0.05,
    term = fct_reorder(term, estimate)
  )

ggplot(or_data, aes(x = estimate, y = term, color = significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_color_manual(values = c("TRUE" = ALERT_RED, "FALSE" = "gray60"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "Not significant"),
                     name = NULL) +
  labs(title = "Predictors of Persistent Sadness/Hopelessness",
       subtitle = "Odds ratios from logistic regression. OR > 1 = increased risk; OR < 1 = protective.",
       x = "Odds Ratio (95% CI)", y = NULL,
       caption = "Source: CDC YRBS Virginia 2023 | Dashed line = OR of 1 (no effect)") +
  theme_survey

save_fig("08_odds_ratio_forest_plot.png", width = 11, height = 7)

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 9: Bullying Trends by Type
# ══════════════════════════════════════════════════════════════════════════

bully_trends <- df %>%
  group_by(survey_year) %>%
  summarise(
    `Bullied at School`     = mean(bullied_at_school) * 100,
    `Electronically Bullied` = mean(electronically_bullied) * 100,
    `Missed School (Safety)` = mean(missed_school_safety) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-survey_year, names_to = "type", values_to = "pct")

ggplot(bully_trends, aes(x = survey_year, y = pct,
                          color = type, group = type)) +
  geom_line(linewidth = 1.8) + geom_point(size = 4) +
  scale_color_manual(values = c("Bullied at School" = VDOE_BLUE,
                                 "Electronically Bullied" = ACCENT_ORANGE,
                                 "Missed School (Safety)" = ALERT_RED),
                     name = NULL) +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023)) +
  labs(title = "Bullying and School Safety Trends (Virginia, 2015–2023)",
       subtitle = "Cyberbullying has risen while in-person bullying has remained stable.",
       x = "Survey Year", y = "Prevalence (%)",
       caption = "Source: CDC YRBS") +
  theme_survey + theme(legend.position = "right")

save_fig("09_bullying_trends.png")

# ══════════════════════════════════════════════════════════════════════════
# FIGURE 10: Risk Score Distribution by Connectedness
# ══════════════════════════════════════════════════════════════════════════

ggplot(latest, aes(x = total_risk_score,
                    fill = factor(high_connectedness))) +
  geom_histogram(position = "identity", alpha = 0.6, binwidth = 1,
                 color = "white") +
  scale_fill_manual(values = c("0" = ALERT_RED, "1" = SUCCESS_GREEN),
                    labels = c("0" = "Low Connectedness",
                               "1" = "High Connectedness"),
                    name = NULL) +
  labs(title = "Total Risk Score Distribution by School Connectedness",
       subtitle = "Students with high school connectedness cluster toward lower risk scores.",
       x = "Total Risk Score (0–9: count of risk behaviors endorsed)",
       y = "Number of Students",
       caption = "Source: CDC YRBS Virginia 2023 | Risk score = sum of 9 binary indicators") +
  theme_survey

save_fig("10_risk_score_distribution.png")

# ══════════════════════════════════════════════════════════════════════════

cat("\n══════════════════════════════════════════════════════════════════\n")
cat("  ✅ ALL VISUALIZATIONS COMPLETE\n")
cat("  Saved to: output/figures/\n")
cat("══════════════════════════════════════════════════════════════════\n")
