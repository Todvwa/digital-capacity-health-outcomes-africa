# ================================================================
# CONSOLIDATED AFRICA DIGITAL HEALTH MODELS
# Bricks and Bytes Project
#
# Purpose:
#   1. Use log GDP per capita consistently across all models.
#   2. Produce a clean summary statistics table.
#   3. Combine baseline and distributed lag models in one table.
#   4. Produce the main interaction model table.
#   5. Produce appendix/robustness tables.
#   6. Produce three trend plots.
#
# Output folders:
#   tables/  = LaTeX regression and summary tables
#   figures/ = trend plots
# ================================================================

# -------------------------------
# 0) Packages
# -------------------------------
required_packages <- c(
  "tidyverse", "fixest", "knitr", "car", "scales"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(tidyverse)
library(fixest)
library(knitr)
library(car)
library(scales)

# -------------------------------
# 1) Project setup
# -------------------------------
# Set this to the folder where africa_panel_extended.csv is located.
# If the script is in the same folder as the data, this should work as-is.
# setwd("YOUR/FOLDER/PATH/HERE")

dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# -------------------------------
# 2) Load and prepare data
# -------------------------------
panel <- read_csv("africa_panel_extended.csv", show_col_types = FALSE)

# Main outcomes
outcomes <- c("u5_mortality", "maternal_mortality", "neonatal_mortality")
outcome_labels <- c(
  u5_mortality = "Under-5 Mortality",
  maternal_mortality = "Maternal Mortality",
  neonatal_mortality = "Neonatal Mortality"
)

# Basic variable check
needed_vars <- c(
  "country", "year", outcomes,
  "internet_users", "mobile_subs", "gdp_pc", "fertility",
  "water", "sanitation", "health_exp", "hiv_prev", "nurses", "ict_index"
)
missing_vars <- setdiff(needed_vars, names(panel))
if (length(missing_vars) > 0) {
  stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
}

# Create log GDP and lags once, then use this dataset everywhere.
df <- panel %>%
  filter(year >= 2000, year <= 2022) %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # Log GDP: only defined for positive GDP values
    log_gdp_pc = if_else(gdp_pc > 0, log(gdp_pc), NA_real_),

    # Digital lags
    internet_l1 = lag(internet_users, 1),
    internet_l2 = lag(internet_users, 2),
    internet_l3 = lag(internet_users, 3),

    mobile_l1 = lag(mobile_subs, 1),
    mobile_l2 = lag(mobile_subs, 2),
    mobile_l3 = lag(mobile_subs, 3),


    # Interaction terms
    internet_nurses_l2 = internet_l2 * nurses,
    interact0 = internet_users * nurses,
    interact_l1 = lag(interact0, 1),
    interact_l2 = lag(interact0, 2),
    interact_l3 = lag(interact0, 3)
  ) %>%
  ungroup()

# -------------------------------
# 3) Helper functions
# -------------------------------
clean_model_data <- function(data, vars) {
  data %>%
    select(all_of(vars)) %>%
    filter(complete.cases(.))
}

get_y_mean <- function(data, outcome) {
  mean(data[[outcome]], na.rm = TRUE)
}

export_etable <- function(models, headers, baseline_means, file, title, label, notes = NULL) {
  
  names(models) <- headers
  
  extra_lines <- list(
    "Baseline mean" = sprintf("%.2f", baseline_means),
    "Country FE" = rep("Yes", length(models)),
    "Year FE" = rep("Yes", length(models))
  )
  
  et <- do.call(
    etable,
    c(
      models,
      list(
        fitstat = ~ n,
        extralines = extra_lines,
        tex = TRUE,
        title = title,
        label = label,
        digits = 3,
        replace = TRUE
      )
    )
  )
  
  if (!is.null(notes)) {
    et <- gsub(
      "\\\\end\\{tabular\\}",
      paste0(
        "\\\\end{tabular}\n",
        "\\\\begin{flushleft}\n",
        "\\\\footnotesize Notes: ", notes, "\n",
        "\\\\end{flushleft}"
      ),
      et
    )
  }
  
  writeLines(et, file)
}
# -------------------------------
# 4) Summary statistics table
# -------------------------------
summary_vars <- c(
  "u5_mortality",
  "maternal_mortality",
  "neonatal_mortality",
  "internet_users",
  "mobile_subs",
  "ict_index",
  "log_gdp_pc",
  "fertility",
  "water",
  "sanitation",
  "health_exp"
)

summary_labels <- c(
  u5_mortality = "Under-5 Mortality (per 1,000)",
  maternal_mortality = "Maternal Mortality (per 100,000)",
  neonatal_mortality = "Neonatal Mortality (per 1,000)",
  internet_users = "Internet Users (\\% population)",
  mobile_subs = "Mobile Subscriptions (per 100)",
  ict_index = "ICT Index",
  log_gdp_pc = "Log GDP per capita",
  fertility = "Fertility Rate",
  water = "Access to Water (\\%)",
  sanitation = "Access to Sanitation (\\%)",
  health_exp = "Health Expenditure (\\% of GDP)"
)

summary_stats <- df %>%
  select(all_of(summary_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    N = sum(!is.na(value)),
    Mean = mean(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Variable = summary_labels[variable],
    across(c(Mean, SD, Min, Median, Max), ~ round(.x, 2))
  ) %>%
  select(Variable, N, Mean, SD, Min, Median, Max)

summary_latex <- kable(
  summary_stats,
  format = "latex",
  booktabs = TRUE,
  caption = "Summary Statistics",
  label = "tab:summary_stats",
  align = c("l", "c", "c", "c", "c", "c", "c")
)

writeLines(summary_latex, "tables/summary_statistics.tex")

# -------------------------------
# 5) Main model functions
# -------------------------------
run_baseline <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_users", "mobile_subs",
    "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_users + mobile_subs + log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_lag2_core <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "mobile_l2",
    "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_distributed_lag <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_users", "internet_l1", "internet_l2",
    "mobile_subs", "mobile_l1", "mobile_l2",
    "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_users + internet_l1 + internet_l2 + ",
      "mobile_subs + mobile_l1 + mobile_l2 + ",
      "log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_expanded_fe <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "mobile_l2",
    "log_gdp_pc", "fertility", "water", "sanitation",
    "hiv_prev", "health_exp"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + fertility + water + sanitation + hiv_prev + health_exp | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_interaction <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "nurses", "internet_nurses_l2",
    "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + nurses + internet_nurses_l2 + log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_lag_mechanism <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_users", "internet_l1", "internet_l2", "internet_l3",
    "nurses", "nurses_l1", "nurses_l2", "nurses_l3",
    "interact0", "interact_l1", "interact_l2", "interact_l3",
    "log_gdp_pc", "hiv_prev", "health_exp"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_users + internet_l1 + internet_l2 + internet_l3 + ",
      "nurses + nurses_l1 + nurses_l2 + nurses_l3 + ",
      "interact0 + interact_l1 + interact_l2 + interact_l3 + ",
      "log_gdp_pc + hiv_prev + health_exp | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

# -------------------------------
# 6) Estimate main models
# -------------------------------
baseline_results <- lapply(outcomes, run_baseline, data = df)
lag2_results <- lapply(outcomes, run_lag2_core, data = df)
distributed_results <- lapply(outcomes, run_distributed_lag, data = df)
expanded_results <- lapply(outcomes, run_expanded_fe, data = df)
interaction_results <- lapply(outcomes, run_interaction, data = df)
lag_mechanism_results <- lapply(outcomes, run_lag_mechanism, data = df)

names(baseline_results) <- outcomes
names(lag2_results) <- outcomes
names(distributed_results) <- outcomes
names(expanded_results) <- outcomes
names(interaction_results) <- outcomes
names(lag_mechanism_results) <- outcomes

baseline_models <- lapply(baseline_results, `[[`, "model")
lag2_models <- lapply(lag2_results, `[[`, "model")
distributed_lag_models <- lapply(distributed_results, `[[`, "model")
expanded_models <- lapply(expanded_results, `[[`, "model")
interaction_models <- lapply(interaction_results, `[[`, "model")
lag_mechanism_models <- lapply(lag_mechanism_results, `[[`, "model")

# -------------------------------
# 7) Main tables
# -------------------------------
# 7a. Combined Baseline + Distributed Lag table
# This is the table your supervisor requested.
combined_models <- list(
  baseline_models[["u5_mortality"]],
  distributed_lag_models[["u5_mortality"]],
  baseline_models[["maternal_mortality"]],
  distributed_lag_models[["maternal_mortality"]],
  baseline_models[["neonatal_mortality"]],
  distributed_lag_models[["neonatal_mortality"]]
)

combined_headers <- c(
  "U5: Baseline", "U5: Distributed Lag",
  "Maternal: Baseline", "Maternal: Distributed Lag",
  "Neonatal: Baseline", "Neonatal: Distributed Lag"
)

combined_means <- c(
  baseline_results[["u5_mortality"]]$mean_y,
  distributed_results[["u5_mortality"]]$mean_y,
  baseline_results[["maternal_mortality"]]$mean_y,
  distributed_results[["maternal_mortality"]]$mean_y,
  baseline_results[["neonatal_mortality"]]$mean_y,
  distributed_results[["neonatal_mortality"]]$mean_y
)

export_etable(
  models = combined_models,
  headers = combined_headers,
  baseline_means = combined_means,
  file = "tables/baseline_distributed_lag_table.tex",
  title = "Baseline and Distributed Lag Fixed Effects Models",
  label = "tab:baseline_distributed_lag"
)

# 7b. Main interaction table
interaction_means <- sapply(interaction_results, `[[`, "mean_y")
export_etable(
  models = interaction_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = interaction_means,
  file = "tables/main_interaction_table.tex",
  title = "Main Results: Internet Access, Nursing Capacity, and Health Outcomes",
  label = "tab:main_interaction"
)

export_etable(
  models = combined_models,
  headers = combined_headers,
  baseline_means = combined_means,
  file = "tables/baseline_distributed_lag_table.tex",
  title = "Baseline and Distributed Lag Fixed Effects Models",
  label = "tab:baseline_distributed_lag",
  notes = "Columns (1), (3), and (5) report the baseline fixed effects model in Equation~\\ref{eq:baseline}. Columns (2), (4), and (6) report the distributed lag model in Equation~\\ref{eq:lag}. All models include country and year fixed effects. Standard errors are clustered at the country level."
)

# 7c. Lag-2 core model table, useful if you want a compact robustness table
lag2_means <- sapply(lag2_results, `[[`, "mean_y")
export_etable(
  models = lag2_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = lag2_means,
  file = "tables/lag2_core_table.tex",
  title = "Lag-2 Core Fixed Effects Models",
  label = "tab:lag2_core"
)

export_etable(
  models = interaction_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = interaction_means,
  file = "tables/main_interaction_table.tex",
  title = "Main Results: Internet Access, Nursing Capacity, and Health Outcomes",
  label = "tab:main_interaction",
  notes = "This table reports the interaction model in Equation~\\ref{eq:interaction}. The interaction between lagged internet use and nurses tests whether digital capacity and workforce capacity operate as complements or substitutes. All models include country and year fixed effects. Standard errors are clustered at the country level."
)

# 7d. Expanded model table, appendix candidate
expanded_means <- sapply(expanded_results, `[[`, "mean_y")
export_etable(
  models = expanded_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = expanded_means,
  file = "tables/expanded_fe_table.tex",
  title = "Expanded Fixed Effects Models",
  label = "tab:expanded_fe"
)

# 7e. Extended lag mechanism table, appendix candidate
lag_mech_means <- sapply(lag_mechanism_results, `[[`, "mean_y")
export_etable(
  models = lag_mechanism_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = lag_mech_means,
  file = "tables/lag_mechanism_table.tex",
  title = "Extended Lag Mechanism Models",
  label = "tab:lag_mechanism"
)

# -------------------------------
# 8) Stepwise models, appendix
# -------------------------------
# Stepwise is kept in the appendix, as suggested by your supervisor.
stepwise_vars <- c(
  "internet_l2", "mobile_l2", "health_exp", "hiv_prev",
  "fertility", "water", "sanitation", "log_gdp_pc"
)

run_stepwise <- function(outcome, data) {
  model_vars <- c("country", "year", outcome, stepwise_vars)
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome, " ~ csw(", paste(stepwise_vars, collapse = ", "), ") | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

stepwise_results <- lapply(outcomes, run_stepwise, data = df)
names(stepwise_results) <- outcomes
stepwise_models <- lapply(stepwise_results, `[[`, "model")

# Note: csw() creates several columns per outcome, so this table can be wide.
do.call(
  etable,
  c(
    stepwise_models,
    list(
      headers = unname(outcome_labels[outcomes]),
      fitstat = ~ n,
      tex = TRUE,
      file = "tables/appendix_stepwise_table.tex",
      title = "Appendix: Stepwise Fixed Effects Models",
      label = "tab:appendix_stepwise",
      digits = 3,
      replace = TRUE
    )
  )
)

# -------------------------------
# 9) Fertility sensitivity models, appendix
# -------------------------------
run_no_fertility <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "mobile_l2", "log_gdp_pc", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

run_replace_fertility <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "mobile_l2", "log_gdp_pc",
    "hiv_prev", "health_exp", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + hiv_prev + health_exp + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

# Residualize fertility using log GDP and other controls.
fertility_sample <- df %>%
  select(country, year, fertility, log_gdp_pc, water, sanitation, hiv_prev, health_exp) %>%
  filter(complete.cases(.))

fertility_resid_model <- feols(
  fertility ~ log_gdp_pc + water + sanitation + hiv_prev + health_exp | country + year,
  data = fertility_sample
)

fertility_resid_data <- fertility_sample %>%
  mutate(fertility_resid = resid(fertility_resid_model)) %>%
  select(country, year, fertility_resid)

df_resid <- df %>%
  left_join(fertility_resid_data, by = c("country", "year"))

run_residual_fertility <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "internet_l2", "mobile_l2", "log_gdp_pc",
    "water", "sanitation", "hiv_prev", "health_exp", "fertility_resid"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + water + sanitation + hiv_prev + health_exp + fertility_resid | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

no_fertility_results <- lapply(outcomes, run_no_fertility, data = df)
replace_fertility_results <- lapply(outcomes, run_replace_fertility, data = df)
residual_fertility_results <- lapply(outcomes, run_residual_fertility, data = df_resid)

names(no_fertility_results) <- outcomes
names(replace_fertility_results) <- outcomes
names(residual_fertility_results) <- outcomes

no_fertility_models <- lapply(no_fertility_results, `[[`, "model")
replace_fertility_models <- lapply(replace_fertility_results, `[[`, "model")
residual_fertility_models <- lapply(residual_fertility_results, `[[`, "model")

export_etable(
  models = no_fertility_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = sapply(no_fertility_results, `[[`, "mean_y"),
  file = "tables/appendix_no_fertility_table.tex",
  title = "Appendix: Fixed Effects Models Excluding Fertility",
  label = "tab:appendix_no_fertility"
)

export_etable(
  models = replace_fertility_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = sapply(replace_fertility_results, `[[`, "mean_y"),
  file = "tables/appendix_replace_fertility_table.tex",
  title = "Appendix: Replacing Fertility with Alternative Health Controls",
  label = "tab:appendix_replace_fertility"
)

export_etable(
  models = residual_fertility_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = sapply(residual_fertility_results, `[[`, "mean_y"),
  file = "tables/appendix_residual_fertility_table.tex",
  title = "Appendix: Residual Fertility Models",
  label = "tab:appendix_residual_fertility"
)

# -------------------------------
# 10) Additional robustness models
# -------------------------------
# Standardized variables
z <- function(x) as.numeric(scale(x))

df_std <- df %>%
  mutate(
    internet_l2_z = z(internet_l2),
    mobile_l2_z = z(mobile_l2),
    log_gdp_pc_z = z(log_gdp_pc),
    fertility_z = z(fertility),
    water_z = z(water),
    sanitation_z = z(sanitation),
    health_exp_z = z(health_exp),
    hiv_prev_z = z(hiv_prev),
    u5_mortality_z = z(u5_mortality),
    maternal_mortality_z = z(maternal_mortality),
    neonatal_mortality_z = z(neonatal_mortality)
  )

run_standardized <- function(outcome, data) {
  outcome_z <- paste0(outcome, "_z")
  model_vars <- c(
    "country", "year", outcome_z,
    "internet_l2_z", "mobile_l2_z", "log_gdp_pc_z",
    "fertility_z", "water_z", "sanitation_z"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome_z,
      " ~ internet_l2_z + mobile_l2_z + log_gdp_pc_z + fertility_z + water_z + sanitation_z | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome_z), n = nrow(d))
}

standardized_results <- lapply(outcomes, run_standardized, data = df_std)
names(standardized_results) <- outcomes
standardized_models <- lapply(standardized_results, `[[`, "model")

export_etable(
  models = standardized_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = sapply(standardized_results, `[[`, "mean_y"),
  file = "tables/appendix_standardized_table.tex",
  title = "Appendix: Standardized Fixed Effects Models",
  label = "tab:appendix_standardized"
)

# Logged outcomes
# Add small guard: log only positive outcome values.
df_log_outcomes <- df %>%
  mutate(
    log_u5_mortality = if_else(u5_mortality > 0, log(u5_mortality), NA_real_),
    log_maternal_mortality = if_else(maternal_mortality > 0, log(maternal_mortality), NA_real_),
    log_neonatal_mortality = if_else(neonatal_mortality > 0, log(neonatal_mortality), NA_real_)
  )

log_outcome_map <- c(
  u5_mortality = "log_u5_mortality",
  maternal_mortality = "log_maternal_mortality",
  neonatal_mortality = "log_neonatal_mortality"
)

run_logged_outcome <- function(outcome, data) {
  log_outcome <- log_outcome_map[[outcome]]
  model_vars <- c(
    "country", "year", log_outcome,
    "internet_l2", "mobile_l2", "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      log_outcome,
      " ~ internet_l2 + mobile_l2 + log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, log_outcome), n = nrow(d))
}

logged_results <- lapply(outcomes, run_logged_outcome, data = df_log_outcomes)
names(logged_results) <- outcomes
logged_models <- lapply(logged_results, `[[`, "model")

export_etable(
  models = logged_models,
  headers = c("Log U5", "Log Maternal", "Log Neonatal"),
  baseline_means = sapply(logged_results, `[[`, "mean_y"),
  file = "tables/appendix_logged_outcomes_table.tex",
  title = "Appendix: Logged Outcome Fixed Effects Models",
  label = "tab:appendix_logged_outcomes"
)

# First-difference models using log GDP difference
fd_outcomes <- c("du5_mortality", "dmaternal_mortality", "dneonatal_mortality")

df_fd <- df %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    du5_mortality = u5_mortality - lag(u5_mortality),
    dmaternal_mortality = maternal_mortality - lag(maternal_mortality),
    dneonatal_mortality = neonatal_mortality - lag(neonatal_mortality),
    dinternet_l2 = internet_l2 - lag(internet_l2),
    dmobile_l2 = mobile_l2 - lag(mobile_l2),
    dlog_gdp_pc = log_gdp_pc - lag(log_gdp_pc),
    dfertility = fertility - lag(fertility),
    dwater = water - lag(water),
    dsanitation = sanitation - lag(sanitation)
  ) %>%
  ungroup()

run_first_difference <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "dinternet_l2", "dmobile_l2", "dlog_gdp_pc",
    "dfertility", "dwater", "dsanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ dinternet_l2 + dmobile_l2 + dlog_gdp_pc + dfertility + dwater + dsanitation | year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

fd_results <- lapply(fd_outcomes, run_first_difference, data = df_fd)
fd_models <- lapply(fd_results, `[[`, "model")

export_etable(
  models = fd_models,
  headers = c("Delta U5", "Delta Maternal", "Delta Neonatal"),
  baseline_means = sapply(fd_results, `[[`, "mean_y"),
  file = "tables/appendix_first_difference_table.tex",
  title = "Appendix: First-Difference Models",
  label = "tab:appendix_first_difference"
)

# Digital index model
# Uses log GDP as the economic control.
df_index <- df %>%
  mutate(
    internet_z = z(internet_l2),
    mobile_z = z(mobile_l2),
    digital_index = (internet_z + mobile_z) / 2
  )

run_digital_index <- function(outcome, data) {
  model_vars <- c(
    "country", "year", outcome,
    "digital_index", "log_gdp_pc", "fertility", "water", "sanitation"
  )
  d <- clean_model_data(data, model_vars)

  model <- feols(
    as.formula(paste0(
      outcome,
      " ~ digital_index + log_gdp_pc + fertility + water + sanitation | country + year"
    )),
    data = d,
    vcov = ~ country
  )

  list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
}

index_results <- lapply(outcomes, run_digital_index, data = df_index)
names(index_results) <- outcomes
index_models <- lapply(index_results, `[[`, "model")

export_etable(
  models = index_models,
  headers = unname(outcome_labels[outcomes]),
  baseline_means = sapply(index_results, `[[`, "mean_y"),
  file = "tables/appendix_digital_index_table.tex",
  title = "Appendix: Fixed Effects Models Using Digital Capacity Index",
  label = "tab:appendix_digital_index"
)

# -------------------------------
# 11) Joint lag tests and cumulative effects
# -------------------------------
lag_test_table <- bind_rows(lapply(outcomes, function(outcome) {
  m <- distributed_lag_models[[outcome]]
  b <- coef(m)

  internet_terms <- c("internet_users", "internet_l1", "internet_l2")
  mobile_terms <- c("mobile_subs", "mobile_l1", "mobile_l2")

  internet_joint <- fixest::wald(m, keep = "^internet_users$|^internet_l1$|^internet_l2$")
  mobile_joint <- fixest::wald(m, keep = "^mobile_subs$|^mobile_l1$|^mobile_l2$")

  tibble(
    Outcome = outcome_labels[[outcome]],
    N = nobs(m),
    `Internet Joint F` = unname(internet_joint$stat),
    `Internet Joint p` = unname(internet_joint$p),
    `Mobile Joint F` = unname(mobile_joint$stat),
    `Mobile Joint p` = unname(mobile_joint$p),
    `Cumulative Internet Effect` = sum(b[internet_terms], na.rm = TRUE),
    `Cumulative Mobile Effect` = sum(b[mobile_terms], na.rm = TRUE)
  )
})) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

lag_test_latex <- kable(
  lag_test_table,
  format = "latex",
  booktabs = TRUE,
  caption = "Joint Lag Tests and Cumulative Effects",
  label = "tab:joint_lag_tests"
)
writeLines(lag_test_latex, "tables/joint_lag_tests.tex")

# -------------------------------
# 12) Revised trend plots
# -------------------------------

# Plot 1: Mortality outcomes in actual values, not indexed.
outcome_trends_actual <- df %>%
  group_by(year) %>%
  summarise(
    `Maternal Mortality` = mean(maternal_mortality, na.rm = TRUE),
    `Neonatal Mortality` = mean(neonatal_mortality, na.rm = TRUE),
    `Under-5 Mortality` = mean(u5_mortality, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "Outcome", values_to = "Mean")

p1_actual <- ggplot(outcome_trends_actual, aes(x = year, y = Mean)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Outcome, scales = "free_y") +
  labs(
    title = "Trends in Health Outcomes, 2000–2022",
    subtitle = "Average mortality rates shown in original units",
    x = "Year",
    y = "Mortality rate"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  "figures/figure_outcome_trends_actual.png",
  p1_actual,
  width = 9,
  height = 5,
  dpi = 300
)


# Plot 2: Under-5 mortality and internet use with dual y-axis.
# Left axis = under-5 mortality; right axis = internet use.
u5_internet_dual <- df %>%
  group_by(year) %>%
  summarise(
    u5_mortality = mean(u5_mortality, na.rm = TRUE),
    internet_users = mean(internet_users, na.rm = TRUE),
    .groups = "drop"
  )

# Scaling factor allows internet use to be plotted on the mortality axis.
scale_factor_u5 <- max(u5_internet_dual$u5_mortality, na.rm = TRUE) /
  max(u5_internet_dual$internet_users, na.rm = TRUE)

p2_dual <- ggplot(u5_internet_dual, aes(x = year)) +
  geom_line(aes(y = u5_mortality, linetype = "Under-5 Mortality"), linewidth = 0.9) +
  geom_line(aes(y = internet_users * scale_factor_u5, linetype = "Internet Users"), linewidth = 0.9) +
  scale_y_continuous(
    name = "Under-5 mortality (deaths per 1,000 live births)",
    sec.axis = sec_axis(
      ~ . / scale_factor_u5,
      name = "Internet users (% of population)"
    )
  ) +
  labs(
    title = "Under-5 Mortality and Internet Use Over Time",
    subtitle = "Mortality shown on left axis; internet use shown on right axis",
    x = "Year",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  "figures/figure_u5_internet_dual_axis.png",
  p2_dual,
  width = 8,
  height = 5,
  dpi = 300
)


# Plot 3: Optional combined mortality and digital capacity dual-axis figure.
# Left axis = mortality; right axis = internet use.
mortality_digital_dual <- df %>%
  group_by(year) %>%
  summarise(
    maternal_mortality = mean(maternal_mortality, na.rm = TRUE),
    neonatal_mortality = mean(neonatal_mortality, na.rm = TRUE),
    u5_mortality = mean(u5_mortality, na.rm = TRUE),
    internet_users = mean(internet_users, na.rm = TRUE),
    mobile_subs = mean(mobile_subs, na.rm = TRUE),
    .groups = "drop"
  )

# Maternal mortality has a much larger scale, so this figure is best used for digital comparison only.
scale_factor_mobile <- max(mortality_digital_dual$u5_mortality, na.rm = TRUE) /
  max(mortality_digital_dual$mobile_subs, na.rm = TRUE)

p3_u5_mobile <- ggplot(mortality_digital_dual, aes(x = year)) +
  geom_line(aes(y = u5_mortality, linetype = "Under-5 Mortality"), linewidth = 0.9) +
  geom_line(aes(y = mobile_subs * scale_factor_mobile, linetype = "Mobile Subscriptions"), linewidth = 0.9) +
  scale_y_continuous(
    name = "Under-5 mortality (deaths per 1,000 live births)",
    sec.axis = sec_axis(
      ~ . / scale_factor_mobile,
      name = "Mobile subscriptions per 100 people"
    )
  ) +
  labs(
    title = "Under-5 Mortality and Mobile Subscriptions Over Time",
    subtitle = "Mortality shown on left axis; mobile subscriptions shown on right axis",
    x = "Year",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  "figures/figure_u5_mobile_dual_axis.png",
  p3_u5_mobile,
  width = 8,
  height = 5,
  dpi = 300
)


# Plot 4: Internet-use trends by selected countries.
selected_countries <- c(
  "Eswatini", "South Africa", "Botswana", "Lesotho", "Mozambique",
  "Namibia", "Zimbabwe", "Kenya", "Rwanda", "Nigeria"
)

internet_country_trends <- df %>%
  filter(country %in% selected_countries) %>%
  group_by(country, year) %>%
  summarise(internet_users = mean(internet_users, na.rm = TRUE), .groups = "drop")

p4_country <- ggplot(
  internet_country_trends,
  aes(x = year, y = internet_users, group = country, linetype = country)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Internet Use Across Selected African Countries",
    subtitle = "Percent of population using the internet",
    x = "Year",
    y = "Internet users (% of population)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave(
  "figures/figure_internet_by_country.png",
  p4_country,
  width = 9,
  height = 5.5,
  dpi = 300
)

# -------------------------------
# 13) Quick sample-size audit
# -------------------------------
sample_audit <- tibble(
  Outcome = outcome_labels[outcomes],
  Baseline_N = sapply(baseline_results, `[[`, "n"),
  Lag2_Core_N = sapply(lag2_results, `[[`, "n"),
  Distributed_Lag_N = sapply(distributed_results, `[[`, "n"),
  Interaction_N = sapply(interaction_results, `[[`, "n"),
  Expanded_N = sapply(expanded_results, `[[`, "n")
)

write_csv(sample_audit, "tables/sample_size_audit.csv")

sample_audit_latex <- kable(
  sample_audit,
  format = "latex",
  booktabs = TRUE,
  caption = "Sample Size Across Main Model Specifications",
  label = "tab:sample_size_audit"
)
writeLines(sample_audit_latex, "tables/sample_size_audit.tex")

cat("\nDone. Tables saved in the 'tables' folder and figures saved in the 'figures' folder.\n")



# -------------------------------
# 14) Additional model work: adolescent fertility control
# -------------------------------

if ("adolescent_fertility" %in% names(df)) {
  
  run_adolescent_fertility <- function(outcome, data) {
    model_vars <- c(
      "country", "year", outcome,
      "internet_l2", "mobile_l2",
      "log_gdp_pc", "fertility", "adolescent_fertility",
      "water", "sanitation"
    )
    
    d <- clean_model_data(data, model_vars)
    
    model <- feols(
      as.formula(paste0(
        outcome,
        " ~ internet_l2 + mobile_l2 + log_gdp_pc + fertility + adolescent_fertility + water + sanitation | country + year"
      )),
      data = d,
      vcov = ~ country
    )
    
    list(model = model, mean_y = get_y_mean(d, outcome), n = nrow(d))
  }
  
  adolescent_results <- lapply(outcomes, run_adolescent_fertility, data = df)
  names(adolescent_results) <- outcomes
  adolescent_models <- lapply(adolescent_results, `[[`, "model")
  
  export_etable(
    models = adolescent_models,
    headers = unname(outcome_labels[outcomes]),
    baseline_means = sapply(adolescent_results, `[[`, "mean_y"),
    file = "tables/appendix_adolescent_fertility_table.tex",
    title = "Appendix: Fixed Effects Models Controlling for Adolescent Fertility",
    label = "tab:appendix_adolescent_fertility",
    notes = "This table adds adolescent fertility as an additional demographic control to test whether the main estimates are sensitive to age-specific fertility pressures. All models include country and year fixed effects. Standard errors are clustered at the country level."
  )
  
} else {
  message("adolescent_fertility not found in df. Skipping adolescent fertility models.")
}


# -------------------------------
# 15) Model selection summary
# -------------------------------

model_selection_table <- bind_rows(lapply(outcomes, function(outcome) {
  
  baseline_m <- baseline_models[[outcome]]
  lag_m <- distributed_lag_models[[outcome]]
  interaction_m <- interaction_models[[outcome]]
  
  tibble(
    Outcome = outcome_labels[[outcome]],
    Model = c("Baseline FE", "Distributed Lag FE", "Interaction FE"),
    N = c(nobs(baseline_m), nobs(lag_m), nobs(interaction_m)),
    AIC = c(AIC(baseline_m), AIC(lag_m), AIC(interaction_m)),
    BIC = c(BIC(baseline_m), BIC(lag_m), BIC(interaction_m)),
    Within_R2 = c(
      fitstat(baseline_m, "wr2")$wr2,
      fitstat(lag_m, "wr2")$wr2,
      fitstat(interaction_m, "wr2")$wr2
    )
  )
})) %>%
  mutate(
    AIC = round(AIC, 2),
    BIC = round(BIC, 2),
    Within_R2 = round(Within_R2, 3)
  )

write_csv(model_selection_table, "tables/model_selection_summary.csv")

model_selection_latex <- kable(
  model_selection_table,
  format = "latex",
  booktabs = TRUE,
  caption = "Model Selection Summary",
  label = "tab:model_selection"
)

writeLines(model_selection_latex, "tables/model_selection_summary.tex")