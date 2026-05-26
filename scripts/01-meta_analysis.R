
# 0. Setup ----------------------------------------------------------------

project_path <- here::here()
data_path <- fs::path(project_path, "data")
processed_path <- fs::path(data_path, "processed")
results_path <- fs::path(project_path, "results")
figures_path <- fs::path(results_path, "figures")
tables_path <- fs::path(results_path, "tables")


# 1. Read meta-analysis data ----------------------------------------------

meta_data <- readRDS(fs::path(processed_path, "meta_analysis_data.rds"))

meta_data <- meta_data |> 
  dplyr::mutate(
    study_label = stringr::str_extract(study_authors, "^[^,]+"),
    study_label = paste0(study_label, " et al.")
  )

# 2. Fir random effects models --------------------------------------------

source("R/fit_meta.R")

meta_models <- meta_data |> 
  dplyr::group_by(factor) |> 
  tidyr::nest() |> 
  dplyr::mutate(
    k = purrr::map_int(data, nrow),
    model = purrr::map(data, fit_meta)
  )

# 3. Get model summaries --------------------------------------------------

source("R/get_meta_summary.R")

meta_summary <- meta_models |>
  dplyr::mutate(
    summary = purrr::map(model, get_meta_summary)
  ) |>
  tidyr::unnest(summary) |>
  dplyr::select(
    factor, k,
    pooled_hr, ci_lower, ci_upper, p_value, i2, tau2, tau, q, q_df, q_p_value
  ) |> 
  dplyr::ungroup()

meta_summary

readr::write_csv(
  meta_summary,
  fs::path(tables_path, "meta_analysis_summary.csv")
)

# 4. Make forest plots ----------------------------------------------------

source("R/make_forest_plot.R")

forest_files <- meta_models |>
  dplyr::mutate(
    forest_file = purrr::pmap_chr(
      list(model, data, factor),
      \(model, data, factor) {
        make_forest_plot(
          model = model,
          data = data,
          factor_name = factor,
          figures_path = figures_path
        )
      }
    )
  ) |>
  dplyr::select(factor, forest_file) |> 
  dplyr::ungroup()

forest_files
