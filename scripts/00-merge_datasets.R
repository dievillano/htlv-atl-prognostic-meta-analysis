
# 0. Setup ----------------------------------------------------------------

project_path <- here::here()
data_path <- fs::path(project_path, "data")
raw_path <- fs::path(data_path, "raw")
processed_path <- fs::path(data_path, "processed")

raw_files <- fs::dir_ls(raw_path)

na_char <- c("NR", "nr", "", "NA")

source("R/clean_effect_table.R")

# 1. Read raw effect tables -----------------------------------------------

age_colnames <- c(
  "study_title", "url", "definition", "n", "hr", "ci_lower", "ci_upper"
)

age <- readr::read_csv(
  raw_files[1],
  skip = 2,
  col_names = age_colnames,
  col_types = "ccciddd",
  na = na_char
) |> 
  clean_effect_table()


ldh_colnames <- c(
  "study_title", "url", "definition", "n", "hr", "ci_lower", "ci_upper", 
  "effect_adjusted_by"
)

ldh <- readr::read_csv(
  raw_files[2],
  skip = 2,
  col_names = ldh_colnames,
  col_types = "cccndddc",
  na = na_char
) |> 
  clean_effect_table()


performance_colnames <- c(
  "study_title", "url", "definition", "n", "hr", "ci_lower", "ci_upper"
)

performance <- readr::read_csv(
  raw_files[4],
  skip = 2,
  col_names = performance_colnames,
  col_types = "ccciddd",
  na = na_char
) |> 
  clean_effect_table()


sex_colnames <- c(
  "study_title", "url", "definition", "n", "hr", "ci_lower", "ci_upper", 
  "effect_adjusted_by"
)

sex <- readr::read_csv(
  raw_files[5],
  skip = 2,
  col_names = sex_colnames,
  col_types = "cccndddc",
  na = na_char
) |> 
  clean_effect_table()


sil2r_colnames <- c(
  "study_title", "url", "definition", "n", "hr", "ci_lower", "ci_upper", 
  "effect_adjusted_by"
)

sil2r <- readr::read_csv(
  raw_files[6],
  skip = 2,
  col_names = sil2r_colnames,
  col_types = "cccndddc",
  na = na_char
) |> 
  clean_effect_table()

# 2. Read study-level paper details ---------------------------------------

papers_colnames <- c(
  "study_id", "pmid", "study_title", "study_authors", "included", "comments",
  "study_country", "study_design", "study_population", "study_adjusted_by"
)

papers <- readr::read_csv(
  raw_files[3],
  skip = 1,
  col_names = papers_colnames,
  col_types = "cccccccccc",
  na = na_char
) |> 
  dplyr::mutate(dplyr::across(dplyr::where(is.character), stringr::str_squish))

papers_included <- papers |> 
  dplyr::filter(included == "Yes")

# 3. Merge effect tables with paper details -------------------------------

effects_datasets <- list(
  age = age,
  ldh = ldh,
  performance = performance,
  sex = sex,
  sil2r = sil2r
)

meta_data <- effects_datasets |> 
  purrr::imap(
    \(data, factor_name) {
      data |> 
        dplyr::mutate(factor = factor_name) |> 
        dplyr::inner_join(papers_included, by = "study_title")
    }
  ) |> 
  dplyr::bind_rows() |> 
  dplyr::relocate(factor) |> 
  dplyr::relocate(c(study_title, url), .after = study_id)

# 4. Harmonise definitions for pooling ------------------------------------

meta_data <- meta_data |> 
  dplyr::mutate(
    usable_definition = dplyr::case_when(
      factor == "age" & definition %in% c(">70") ~ TRUE,
      factor == "ldh" & definition %in% c("LDH>ULN") ~ TRUE,
      factor == "performance" & definition %in% c("2-4", ">1", "2-3") ~ TRUE,
      factor == "sex" & definition == "men" ~ TRUE,
      factor == "sil2r" & definition == ">20000" ~ TRUE,
      TRUE ~ FALSE
    ),
    usable_meta = !is.na(hr) & !is.na(ci_lower) & !is.na(ci_upper)
  )

# 5. Select one estimate for the sex factor -------------------------------

meta_data <- meta_data |> 
  dplyr::mutate(
    selected_effect = dplyr::case_when(
      factor == "sex" &
        stringr::str_detect(
          effect_adjusted_by, stringr::regex("categorical", ignore_case = TRUE)
        )
      ~ FALSE,
      TRUE ~ TRUE
    ),
    selection_reason = dplyr::case_when(
      factor == "sex" &
        stringr::str_detect(
          effect_adjusted_by, stringr::regex("categorical", ignore_case = TRUE)
        ) ~
        "Excluded duplicate sex estimate using categorical sIL-2R adjustment",
      TRUE ~ NA_character_
    )
  )

# 6. Create meta-analysis dataset -----------------------------------------

meta_data_usable <- meta_data |> 
  dplyr::filter(usable_meta, usable_definition, selected_effect) |> 
  dplyr::mutate(
    log_hr = log(hr),
    se_log_hr = (log(ci_upper) - log(ci_lower)) / (2 * 1.96)
  )

# 7. Checks ---------------------------------------------------------------

meta_data_usable |> 
  dplyr::count(study_id, factor) |> 
  dplyr::filter(n > 1)

# Only two valid LDH entries are the same. Removing LDH from meta-analysis.

meta_data_usable <- meta_data_usable |> 
  dplyr::filter(factor != "ldh")

# 8. Save meta-analysis dataset -------------------------------------------

out_filepath_rds <- fs::path(processed_path, "meta_analysis_data.rds")
out_filepath_csv <- fs::path(processed_path, "meta_analysis_data.csv")

saveRDS(meta_data_usable, out_filepath_rds)

readr::write_csv(meta_data_usable, out_filepath_csv)
