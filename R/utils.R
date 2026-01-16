#' @importFrom shinydashboard box tabBox
#' @importFrom dplyr filter mutate select group_by summarize ungroup arrange
#' @importFrom dplyr pull distinct left_join bind_rows slice_max slice_head
#' @importFrom dplyr collect desc n c_across all_of rowwise
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point geom_boxplot
#' @importFrom ggplot2 theme_bw theme_minimal theme element_text labs
#' @importFrom ggplot2 scale_fill_brewer scale_color_brewer coord_cartesian
#' @importFrom ggplot2 position_jitterdodge ggtitle
#' @importFrom here here
#' @importFrom DBI dbConnect dbGetQuery dbWriteTable
#' @importFrom duckdb duckdb
#' @importFrom tibble tibble column_to_rownames
#' @importFrom tidyr pivot_wider separate
#' @importFrom purrr map_dfr
#' @importFrom stringr str_glue str_extract str_split_i str_replace_all
#' @importFrom stringr str_to_lower str_trunc str_flatten str_match str_detect
#' @importFrom stringr str_remove str_to_sentence str_extract_all
#' @importFrom readr read_tsv read_csv
#' @importFrom rlang sym .data
#' @importFrom arrow read_parquet write_parquet
#' @importFrom plotly plot_ly layout colorbar renderPlotly plotlyOutput
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar unit
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom glue glue
#' @importFrom stats setNames
#' @importFrom utils packageVersion
NULL

# Species code regex (note: Esp. includes escaped period)
SPECIES_PATTERN <- "(Efa|Sau|Kpn|Aba|Pae|Esp\\.?)"

# normalize_species helper: make "Esp." and "Esp" equivalent by removing
# a single trailing dot for comparisons (preserves NA).
normalize_species <- function(x) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- NA_character_
  stringr::str_replace_all(x_chr, "\\.$", "")
}

getHoldoutsDrugChoices <- function(bug = NULL) {
  files <- c(
    here::here("shinyapp", "data", "country_models", "performance_metrics.parquet"),
    here::here("shinyapp", "data", "cross_models", "performance_metrics.parquet"),
    here::here("shinyapp", "data", "year_models", "performance_metrics.parquet"),
    here::here("shinyapp", "data", "cross_year", "performance_metrics.parquet")
  )

  # ---- helper to safely read parquet ----
  read_one <- function(fp) {
    if (!file.exists(fp)) {
      return(NULL)
    }
    tryCatch(
      {
        df <- arrow::read_parquet(fp)
        keep <- intersect(names(df), c("species", "drug_or_class_name"))
        if (!"drug_or_class_name" %in% keep) {
          return(NULL)
        }
        df <- df[, keep, drop = FALSE]
        # ensure character types
        df$species <- as.character(df$species)
        df$drug_or_class_name <- as.character(df$drug_or_class_name)
        df
      },
      error = function(e) NULL
    )
  }

  # ---- helper to collapse suffixes like "_Ghana" safely ----
  collapse_country_suffix <- function(v) {
    v <- unique(as.character(v))
    base <- sub("_[^_]+$", "", v) # remove only the final "_token"
    has_base <- base %in% v # only collapse if the base exists
    sort(unique(ifelse(has_base, base, v)))
  }

  dfs <- lapply(files, read_one)
  dfs <- Filter(Negate(is.null), dfs)
  if (!length(dfs)) {
    return(character(0))
  }
  df <- dplyr::bind_rows(dfs)
  if (!nrow(df)) {
    return(character(0))
  }

  # --- Normalise species tokens in the dataframe to a canonical form ---
  # e.g. "Esp." -> "Esp" so comparisons match UI values regardless of trailing dot
  if ("species" %in% names(df)) {
    df <- df %>% dplyr::mutate(species = normalize_species(species))
  }

  # ---- filter by 3-letter species code (also normalize the incoming bug arg) ----
  if (!is.null(bug) && "species" %in% names(df)) {
    bug_norm <- normalize_species(bug)
    df <- dplyr::filter(df, .data$species %in% bug_norm)
  }
  if (!nrow(df)) {
    return(character(0))
  }

  # ---- clean up and return drug/class names ----
  x <- df$drug_or_class_name
  keep_idx <- !is.na(x) &
    nzchar(trimws(x)) &
    !grepl("^(19|20)\\d{2}$", x) & # drop 4-digit years
    !grepl("^_", x) # drop leading-underscore labels
  drugs <- x[keep_idx]
  collapse_country_suffix(drugs)
}


amr_button <- function(id, label, icon_name, class_name) {
  div(
    style = "padding: 5px; text-align: center;",
    actionButton(
      inputId = id,
      label = tags$label(label, style = "font-size: 15px;"),
      icon = icon(icon_name),
      class = class_name,
      style = "width: 80%; font-weight: bold;"
    )
  )
}

styledBox <- function(outputId) {
  uiOutput(outputId, container = function(...) {
    div(style = "display:inline-block")
    div(style = "padding-top: 0px; padding-bottom: 10px; height: 80%", ...)
  })
}

# Select input helper
amr_select <- function(id, label, choices, multiple = TRUE, selected = NULL) {
  div(
    style = "padding: 10px;",
    selectInput(
      inputId = id,
      label = tags$label(label, style = "font-size: 15px;"),
      choices = choices,
      multiple = multiple,
      selectize = TRUE,
      width = "100%",
      selected = selected
    )
  )
}

# Bug choices
bug_choices <- c(
  "Staphylococcus aureus" = "Sau",
  "Klebsiella pneumoniae" = "Kpn",
  "Acinetobacter baumannii" = "Aba",
  "Pseudomonas aeruginosa" = "Pae",
  "Enterobacter spp." = "Esp.",
  "Enterococcus faecium" = "Efa"
)


getTopFeatures <- function(top_features_dir, pattern) {
  # List files in the directory
  file_list <- list.files(top_features_dir, pattern, full.names = TRUE)

  # Exclude files with double underscores
  valid_files <- file_list[!grepl("country__", basename(file_list)) &
    !(grepl("trimethoprim", basename(file_list)) &
      grepl("sulfonamides", basename(file_list)) &
      grepl("derivatives", basename(file_list)))]

  # load drugs names;
  df <- lapply(X = valid_files, FUN = function(x) {
    transformed_x <- stringr::str_replace_all(x, pattern = "United_Kingdom", "UK")

    df <- readr::read_tsv(file.path(x), show_col_types = F) |>
      dplyr::mutate(species = stringr::str_extract(transformed_x, pattern = SPECIES_PATTERN)) |>
      dplyr::mutate(drug_type = stringr::str_extract(transformed_x, pattern = "drug_class|drug")) |>
      dplyr::mutate(feature_type = stringr::str_extract(transformed_x, pattern = "proteins|domains|genes|struct")) |>
      dplyr::mutate(data_type = stringr::str_extract(transformed_x, pattern = "counts|binary|struct")) |>
      dplyr::mutate(configurations = ifelse(!is.na(stringr::str_extract(transformed_x, pattern = "PCA|shuffled")), stringr::str_extract(transformed_x, pattern = "PCA|shuffled"), "full")) |>
      dplyr::mutate(configurations = stringr::str_to_lower(configurations)) |>
      dplyr::mutate(source_file = basename(x))
    if (stringr::str_detect(transformed_x, pattern = "country_models")) {
      df <- dplyr::mutate(
        df,
        drug_or_class_name = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_type}_country_\\s*(.*?)\\s*_{feature_type}"))[, 2] |>
          str_remove("_[^_]+$")
      ) |>
        dplyr::mutate(trained_country = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_or_class_name}_\\s*(.*?)\\s*_{feature_type}"))[, 2]) |>
        dplyr::mutate(tested_country = trained_country)
    } else if (stringr::str_detect(transformed_x, pattern = "cross_models")) {
      df <- dplyr::mutate(
        df,
        drug_or_class_name = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_type}_country_\\s*(.*?)\\s*_{pattern}"))[, 2] |>
          stringr::str_remove("(_[^_]+_[^_]+)$")
      ) |>
        dplyr::mutate(trained_country = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_or_class_name}_\\s*(.*?)\\s*_{pattern}"))[, 2]) |>
        tidyr::separate(
          col = trained_country,
          into = c("trained_country", "tested_country"),
          sep = "_",
          remove = FALSE
        )
    } else if (stringr::str_detect(transformed_x, pattern = "year_models")) {
      df <- dplyr::mutate(
        df,
        drug_or_class_name = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_type}_year_\\s*(.*?)\\s*_(?=\\d)"))[, 2],
        trained_year = stringr::str_extract(transformed_x, pattern = stringr::str_glue("(\\d+)_(\\d+)")),
        tested_year = trained_year
      )
    } else if (stringr::str_detect(transformed_x, pattern = "cross_year")) {
      if (stringr::str_detect(x, "(\\d+)_(\\d+)_(\\d+)_(\\d+)")) {
        years <- unlist(stringr::str_extract_all(transformed_x, pattern = stringr::str_glue("(\\d+)_(\\d+)")))
        second_year <- as.integer(stringr::str_extract(years[2], pattern = "(\\d+)$"))
        df <- dplyr::mutate(
          df,
          drug_or_class_name = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_type}_year_\\s*(.*?)\\s*_(?=\\d)"))[, 2],
          trained_year = stringr::str_extract(transformed_x, pattern = stringr::str_glue("(\\d+)_(\\d+)")),
          trained_year = years[1],
          tested_year = stringr::str_glue("{second_year-4}_{second_year}")
        )
      }
    } else {
      df <- dplyr::mutate(
        df,
        drug_or_class_name = stringr::str_match(transformed_x, pattern = stringr::str_glue("{drug_type}_\\s*(.*?)\\s*_{feature_type}"))[, 2]
      )
    }
  }) |>
    bind_rows()
  # convert the feature_type with struct to genes
  df <- df |>
    dplyr::mutate(feature_type = ifelse(feature_type == "struct", "genes", feature_type))

  ## write to parquet file
  if (stringr::str_detect(top_features_dir, pattern = "country_models") | stringr::str_detect(top_features_dir, pattern = "cross_models")) {
    results_folder <- ifelse(stringr::str_detect(top_features_dir, pattern = "country_models"), "country_models", "cross_models")
    data_file <- ifelse(pattern == "top_features", "top_features.parquet", "performance_metrics.parquet")
    arrow::write_parquet(df, sink = here::here("shinyapp", "data", results_folder, data_file))
  } else if (stringr::str_detect(top_features_dir, pattern = "cross_year") | stringr::str_detect(top_features_dir, pattern = "year_models")) {
    results_folder <- ifelse(stringr::str_detect(top_features_dir, pattern = "cross_year"), "cross_year", "year_models")
    data_file <- ifelse(pattern == "top_features", "top_features.parquet", "performance_metrics.parquet")
    arrow::write_parquet(df, sink = here::here("shinyapp", "data", results_folder, data_file))
  } else {
    results_folder <- ifelse(pattern == "top_features", "top_features", "performance_metrics")
    data_file <- ifelse(pattern == "top_features", "top_features.parquet", "performance_metrics.parquet")
    arrow::write_parquet(df, sink = here::here("shinyapp", "data", results_folder, data_file))
  }
  return(df)
}

combinePerformanceMetrics <- function() {
  library(arrow)
  library(dplyr)

  # Define the paths to the individual performance_metrics.parquet files
  country_metrics_path <- here::here("shinyapp", "data", "country_models", "performance_metrics.parquet")
  cross_country_metrics_path <- here::here("shinyapp", "data", "cross_models", "performance_metrics.parquet")
  year_metrics_path <- here::here("shinyapp", "data", "year_models", "performance_metrics.parquet")
  cross_year_metrics_path <- here::here("shinyapp", "data", "cross_year", "performance_metrics.parquet")

  # Read the individual files and filter out rows with "country__" files
  country_metrics <- read_parquet(country_metrics_path) %>%
    dplyr::filter(!grepl("country__", source_file))

  cross_country_metrics <- read_parquet(cross_country_metrics_path) %>%
    dplyr::filter(!grepl("country__", source_file))

  year_metrics <- read_parquet(year_metrics_path) %>%
    dplyr::filter(!grepl("country__", source_file))

  cross_year_metrics <- read_parquet(cross_year_metrics_path) %>%
    dplyr::filter(!grepl("country__", source_file))

  # Combine all the data
  combined_metrics <- bind_rows(
    country_metrics,
    cross_country_metrics,
    year_metrics,
    cross_year_metrics
  )

  # Write the combined data to the main performance_metrics.parquet file
  combined_metrics_path <- here::here("shinyapp", "data", "performance_metrics", "performance_metrics.parquet")
  arrow::write_parquet(combined_metrics, combined_metrics_path)

  message("Combined performance_metrics.parquet file has been updated.")
}

getAllFilteredData <- function(amr_db_path, filtered_table_path) {
  amr_eskape_files <- list.files(amr_db_path)
  amr_filtered_tbl_db <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = filtered_table_path
  )
  for (i in seq_along(amr_eskape_files)) {
    eskape_path <- file.path(amr_db_path, amr_eskape_files[i])
    eskape_spp_name <- stringr::str_split_i(amr_eskape_files[i], "\\.", 1)

    eskape_db_conn <- DBI::dbConnect(
      duckdb::duckdb(),
      dbdir = eskape_path
    )
    # filter the filtered data table
    eskape_filterered_data_table <- dbGetQuery(
      eskape_db_conn,
      stringr::str_glue(
        "SELECT * FROM {eskape_spp_name}_filtered"
      )
    )
    # store this table in a new connections;
    dbWriteTable(
      amr_filtered_tbl_db,
      name = stringr::str_glue("{eskape_spp_name}_filtered"),
      value = eskape_filterered_data_table,
      overwrite = TRUE
    )
  }
}


loadAMRMetadata <- function(spp_name) {
  message(stringr::str_glue("Duckdb version {packageVersion('duckdb')}"))
  message(stringr::str_glue("DBI version {packageVersion('DBI')}"))
  cwd <- getwd()
  db_fp <- file.path(cwd, "data", "amr_filtered_tbls.db")
  cleaned_iso_src_fp <- file.path(cwd, "data", "llm_gpt_amr_isolation_source_cleaned.csv")
  cleaned_iso_src_df <- readr::read_csv(
    cleaned_iso_src_fp,
    show_col_types = FALSE
  )
  message(stringr::str_glue("Looking for DB at: {db_fp}"))
  if (!file.exists(db_fp)) {
    stop(glue::glue("Database file not found at {db_fp}"))
  }
  amr_filtered_tbl_db <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_fp
  )

  dplyr::tbl(
    amr_filtered_tbl_db,
    stringr::str_glue("{spp_name}_filtered")
  ) %>%
    dplyr::filter(!is.na(genome_drug.antibiotic)) %>%
    dplyr::filter(genome_drug.resistant_phenotype %in% c("Resistant", "Susceptible")) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      cleaned_iso_src_df,
      by = "genome.isolation_source"
    ) %>%
    dplyr::mutate(genome.isolation_source = group)
}

loadDrugClassMap <- function() {
  cwd <- getwd()
  # drug_class_map_fp <- file.path(cwd, "data", "drug_class_map.tsv")
  drug_class_map_fp <- system.file("extdata", "drug_class_map.tsv", package = "amRshiny")
  message(stringr::str_glue("loadDrugClassMap(): Looking for TSV at: {drug_class_map_fp}"))
  drug_class_map_df <- readr::read_tsv(
    here(drug_class_map_fp),
    show_col_types = FALSE
  ) %>%
    dplyr::select(drug.antibiotic_name, drug_class) %>%
    dplyr::distinct()
  return(drug_class_map_df)
}

loadMLResults <- function() {
  cwd <- getwd()
  ml_results_fp <- system.file("extdata", "All_Performances.tsv", package = "amRshiny")
  # ml_results_fp <- here::here("shinyapp", "data", "performance_metrics", "performance_metrics.parquet")
  message(stringr::str_glue("loadMLResults(): Looking for DB at: {ml_results_fp}"))
  return(readr::read_tsv(ml_results_fp, show_col_types = FALSE))
}

loadTopFeat <- function() {
  # Define the correct file path
  cwd <- getwd()
  tf_results_fp <- here::here("shinyapp", "data", "top_features", "top_features.parquet")
  message(stringr::str_glue("loadTopFeat(): Looking for file at: {tf_results_fp}"))

  # Use arrow::read_parquet to load Parquet files
  return(arrow::read_parquet(tf_results_fp))
}

getAmrDrugs <- function(amr_drug, amr_drug_class) {
  # Load the drug class mapping data
  amr_drug_class_df <- loadDrugClassMap()
  # Initialize amr_drugs as NULL
  amr_drugs <- NULL

  # If a specific drug is selected, use it
  if (length(amr_drug) > 0) {
    return(amr_drug)
  }

  # If a drug class is selected, get all drugs for this class
  if (length(amr_drug_class) > 0) {
    amr_drugs <- amr_drug_class_df %>%
      dplyr::filter(drug_class %in% amr_drug_class) %>%
      pull(drug.antibiotic_name) %>%
      unique()
  }

  # Return the vector of selected drugs
  return(amr_drugs)
}

quickStatBox <- function(title, value, unit = "", bg_color = "#f0f0f0", text_color = "black") {
  div(
    style = glue::glue("
      background-color: {bg_color};
      color: {text_color};
      padding: 20px;
      margin: 5px auto;
      border-radius: 10px;
      width:95%;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      text-align: center;
      font-family: sans-serif;
    "),
    div(style = "font-size: 16px;", title),
    div(
      style = "font-size: 12px; margin-top: 5px;",
      paste0(value, if (unit != "") paste0(" ", unit))
    )
  )
}

makeQuickStats <- function(data) { # , drug_class_df, spp_name, amr_drugs) {
  data_with_drug_class <- data
  # Sample stat calculations
  total_genomes <- nrow(data_with_drug_class)
  total_uniques_genomes <- length(unique(data_with_drug_class$genome_drug.genome_id))
  n_amr_drugs <- length(unique(data_with_drug_class$genome_drug.antibiotic))
  n_amr_drug_class <- length(unique(data_with_drug_class$drug_classes[!is.na(data_with_drug_class$drug_classes)]))
  total_strains <- data_with_drug_class[
    data_with_drug_class$genome_drug.resistant_phenotype %in% c("Resistant", "Susceptible"),
  ][["genome_drug.resistant_phenotype"]] |>
    table() |>
    as.data.frame()
  n_genomes_resistant <- total_strains[total_strains$Var1 == "Resistant", "Freq"]
  n_genomes_susceptible <- total_strains[total_strains$Var1 == "Susceptible", "Freq"]

  top_n_drugs <- data_with_drug_class %>%
    dplyr::group_by(genome_drug.antibiotic) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::pull(genome_drug.antibiotic)

  top_n_drugs_class <- data_with_drug_class %>%
    dplyr::filter(!is.na(drug_classes)) %>%
    dplyr::group_by(drug_classes) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::pull(drug_classes)

  top_n_countries <- data_with_drug_class %>%
    dplyr::filter(!grepl(pattern = "NA", x = genome.isolation_country)) %>%
    dplyr::group_by(genome.isolation_country) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n = 5) %>%
    dplyr::pull(genome.isolation_country)
  spp_name <- unique(data_with_drug_class$species)
  summary_paragraph <- tags$p(
    style = "text-align: center; font-weight: bold; padding: 12px 0; font-family: sans-serif; font-size: 20px;",
    stringr::str_glue("Data summary for {stringr::str_to_sentence(spp_name)}")
  )
  tagList(
    summary_paragraph,
    fluidRow(
      column(2, quickStatBox("# isolate-drug-AMR_phenotype combo", total_genomes,
        bg_color = "#000000", text_color = "white"
      )),
      column(2, quickStatBox("# unique genomes", total_uniques_genomes,
        bg_color = "#56B4E9", text_color = "black"
      )),
      column(2, quickStatBox("# resistant isolates", n_genomes_resistant,
        bg_color = "#009E73", text_color = "black"
      )),
      column(2, quickStatBox("# susceptible isolates", n_genomes_susceptible,
        bg_color = "#F0E442", text_color = "black"
      )),
      column(2, quickStatBox("# drugs", paste(n_amr_drugs, collapse = ", "),
        bg_color = "#0072B2"
      ), text_color = "white"),
      column(2, quickStatBox("# drug classes", paste(n_amr_drug_class, collapse = ", "),
        bg_color = "#D55E00", text_color = "white"
      )),
    ),
    fluidRow(
      column(4, quickStatBox("Top 5 drugs", paste(top_n_drugs, collapse = "\n"),
        bg_color = "#CC79A7", , text_color = "black"
      )),
      column(4, quickStatBox("Top 5 drug classes", paste(top_n_drugs_class, collapse = "\n"),
        bg_color = "#999999", text_color = "black"
      )),
      column(4, quickStatBox("Top countries", paste(top_n_countries, collapse = "\n"),
        bg_color = "#E69F00", text_color = "white"
      ))
    )
  )
}

makeDatAvailabilityPlot <- function(data) {
  data <- data %>%
    dplyr::group_by(genome_drug.antibiotic, genome_drug.resistant_phenotype) %>%
    count() %>%
    ungroup()
  ggplot(
    data,
    aes(
      x = genome_drug.antibiotic,
      y = n,
      color = genome_drug.resistant_phenotype,
      fill = genome_drug.resistant_phenotype
    )
  ) +
    geom_col() +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    labs(
      x = "Drugs",
      y = "No. of isolates",
      color = "AMR phenotype",
      fill = "AMR phenotype"
    )
}

makeGeoChloroPlot <- function(data) {
  library(countrycode)
  data$iso3 <- countrycode(data$genome.isolation_country, origin = "country.name", destination = "iso3c")
  plot_ly(
    data = data,
    type = "choropleth",
    locations = ~iso3,
    z = ~count,
    text = ~ paste(iso3, "<br>Genomes:", count),
    hoverinfo = "text",
    colorscale = "Viridis",
    marker = list(line = list(width = 0.5, color = "white"))
  ) %>%
    colorbar(title = list(text = "No. of isolates", font = list(size = 14))) %>%
    layout(
      # title = list(text = "Geographic distribution", font = list(size = 14)),
      geo = list(
        projection = list(type = "natural earth"),
        showcoastlines = TRUE,
        showland = TRUE,
        landcolor = "lightgrey"
      ),
      font = list(size = 14)
    )
}

makeTimeSeriesAMRPlot <- function(data, amr_drug) {
  whole_data_title <- stringr::str_glue(
    "AMR resistance trend"
  )

  title_amr_drug <- stringr::str_glue(
    "AMR resistance trend for {amr_drug}"
  )

  title_amr_drug <- ifelse(
    amr_drug == "all",
    whole_data_title,
    title_amr_drug
  )

  ggplot(
    data,
    aes(
      x = genome.collection_year,
      y = n,
      colour = genome_drug.resistant_phenotype
    )
  ) +
    geom_line() +
    geom_point() +
    labs(
      title = title_amr_drug,
      x = "Year",
      y = "No. of isolates",
      colour = "AMR phenotype"
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
}

makeHostIsolatePlot <- function(data) {
  data <- data %>%
    dplyr::mutate(
      genome.host_common_name = stringr::str_to_lower(genome.host_common_name),
      genome.isolation_source = stringr::str_to_lower(genome.isolation_source)
    )

  host_df <- data %>%
    dplyr::group_by(genome_drug.antibiotic, genome.host_common_name) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup()

  ggplot(
    host_df,
    aes(
      x = genome_drug.antibiotic,
      y = n,
      fill = genome.host_common_name
    )
  ) +
    geom_col(position = "stack") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10),
    ) +
    labs(
      x = "Drugs",
      y = "No. of isolates",
      fill = "Host"
    )
}

makeIsolationSourcesPlot <- function(data) {
  isolation_source <- data %>%
    dplyr::mutate(genome.isolation_source = stringr::str_to_lower(genome.isolation_source)) |>
    dplyr::group_by(genome_drug.antibiotic, genome.isolation_source) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup()

  top_isolate_source <- isolation_source |>
    dplyr::group_by(genome.isolation_source) |>
    dplyr::summarize(n = sum(n)) |>
    dplyr::filter(genome.isolation_source != "") |>
    dplyr::slice_max(order_by = n, n = 10) |>
    dplyr::pull(genome.isolation_source)

  # Filter the isolation source data to include only the top five sources
  isolation_source <- isolation_source |>
    dplyr::mutate(
      genome.isolation_source_ = ifelse(
        genome.isolation_source %in% top_isolate_source,
        genome.isolation_source,
        "Other"
      )
    )
  isolation_source <- isolation_source %>%
    mutate(genome.isolation_source_ = stringr::str_trunc(genome.isolation_source_, width = 20))

  isolation_source_plot <- ggplot(
    isolation_source,
    aes(
      x = genome_drug.antibiotic,
      y = n,
      fill = genome.isolation_source_
    )
  ) +
    geom_col(position = "stack") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      legend.text = element_text(size = 10),
      axis.title = element_text(size = 10)
    ) +
    labs(
      x = "",
      y = "No. of isolates",
      fill = "Isolation source"
    )
  return(isolation_source_plot)
}

# makeModelPerformancePlot(data, input$bug_search_amr, input$model_scale, input$model_metrics, input$data_type)
makeModelPerformancePlot <- function(bug, model_scale, data_type, metrics, amr_drug_class, amr_drug) {
  plotMetrics <- function(
    bug,
    metrics_df,
    title = "Performance metrics",
    metric_col = "nmcc",
    fill_col = "model_colors",
    facet_col = "model_colors",
    selected_drug = ""
  ) {
    # Load required libraries
    library(tidyverse)

    # Ensure the metrics_df is a data frame
    if (!is.data.frame(metrics_df)) {
      stop("metrics_df must be a data frame")
    }

    # ESKAPE order (species codes)
    eskape_order <- c("Efa", "Sau", "Kpn", "Aba", "Pae", "Esp")
    metrics_df <- metrics_df %>%
      dplyr::mutate(!!rlang::sym(bug) := normalize_species(.data[[bug]]))

    # set factor levels to the intersection of the canonical order and the species present,
    # preserving the canonical order for any species present in the data
    present <- unique(metrics_df[[bug]])
    levels_order <- intersect(eskape_order, present)

    # apply factor with safe levels (if none present, leave as character)
    if (length(levels_order) > 0) {
      metrics_df <- metrics_df %>%
        dplyr::mutate(!!rlang::sym(bug) := factor(.data[[bug]], levels = levels_order))
    } else {
      # fallback: keep as character if no matching tokens found
      metrics_df <- metrics_df %>% dplyr::mutate(!!rlang::sym(bug) := as.character(.data[[bug]]))
    }

    # Plot nMCC across species, data types, and molecular scales
    g <- ggplot(
      metrics_df,
      aes(x = get(bug), y = get(metric_col), fill = get(fill_col), color = get(fill_col))
    ) +
      geom_boxplot(outliers = F, alpha = 0.4) +
      labs(
        title = title,
        x = "Species",
        y = metric_col
      ) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Dark2") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      coord_cartesian(ylim = c(0, 1))

    if (!is.null(amr_drug) && amr_drug != "") {
      g <- g +
        geom_point(
          data = metrics_df %>% dplyr::filter(drug_or_drug_class == amr_drug),
          position = position_jitterdodge(jitter.width = 0.1),
          size = 4
        )
    } else {
      g <- g + ggtitle(title)
    }
    return(g)
  }
  # subset the data based on the selected bug and model scale
  data <- readr::read_tsv(here::here("shinyapp", "data", "performance_metrics", "all_metrics.tsv")) %>%
    dplyr::filter(species %in% bug) %>%
    dplyr::filter(scale %in% model_scale) %>%
    dplyr::filter(data %in% data_type)

  # inner join the drug class mapping
  if (amr_drug_class != "all") {
    drug_within_class_vec <- readr::read_csv(here::here("shinyapp", "data", "drug_cleanup.csv")) %>%
      dplyr::mutate(drug_classes = stringr::str_replace_all(drug_classes, pattern = " |-", "_")) %>%
      dplyr::filter(drug_classes %in% amr_drug_class) %>%
      dplyr::select(predicted_drug, drug_classes) %>%
      dplyr::pull(predicted_drug) %>%
      unique()
    drug_within_class_vec <- c(
      drug_within_class_vec[!is.na(drug_within_class_vec)],
      amr_drug_class
    )
    data <- data %>%
      dplyr::filter(drug_or_drug_class %in% drug_within_class_vec)
  }


  # plot the models stats
  plotMetrics(
    "species",
    data,
    title = "Performance metrics",
    metric_col = metrics,
    fill_col = "model_colors",
    selected_drug = amr_drug
  )
}

# make the important feature plots
# [1] "domain_acc_binary"       "domain_acc_binary_class" "domain_all_binary"       "domain_all_binary_class" "domain_all_count"        "domain_all_count_class"
# [7] "gene_acc_binary"         "gene_acc_binary_class"   "gene_all_binary"         "gene_all_binary_class"   "gene_all_count"          "gene_all_count_class"
# [13] "gene_all_struct"         "gene_all_struct_class"
makeFeatureImportancePlot <- function(bug, amr_drug, model_scale, data_type_, top_n_features, feature_importance_tabset) {
  # fix: replace ~ with . (as used in the ML models)
  # Define the scale to match annotated files.
  scale <- dplyr::case_when(
    model_scale == "proteins" ~ "protein",
    model_scale == "domains" ~ "domain",
    model_scale == "genes" ~ "gene"
  )

  top_n_features <- ifelse(
    top_n_features == "all", "all",
    as.numeric(top_n_features)
  )

  # normalize selected bug to remove any trailing dot (so "Esp."-> "Esp")
  bug_norm <- normalize_species(bug)

  top_features_df <- arrow::read_parquet("data/top_features/top_features.parquet") %>%
    dplyr::mutate(species = normalize_species(species)) %>%
    dplyr::filter(species %in% bug_norm) |>
    dplyr::filter(drug_or_class_name %in% amr_drug) %>%
    dplyr::filter(feature_type == model_scale) %>%
    dplyr::filter(data_type == data_type_) %>%
    dplyr::filter(configurations == "full")

  # load annotated table
  annotated_files <- list.files(
    path = here::here("shinyapp", "data", "Annotated"),
    pattern = stringr::str_glue(stringr::str_flatten(bug_norm, collapse = "|")),
    full.names = TRUE
  )
  annotated_files <- Filter(
    function(x) grepl(pattern = stringr::str_glue("{scale}"), x = x),
    annotated_files
  )
  # inner join each annotated table per species
  annotated_table <- purrr::map_dfr(
    .x = annotated_files,
    .f = function(x) {
      sp <- stringr::str_extract(basename(x), pattern = SPECIES_PATTERN)
      sp_norm <- normalize_species(sp)
      arrow::read_parquet(x) |> dplyr::mutate(species = sp_norm)
    }
  )

  # make the current selection of the top feature empty;
  # readr::write_tsv(tibble::tibble(), here::here("shinyapp","data", "top_features", "current_top_features.tsv"))

  # group column
  group_column <- dplyr::case_when(
    feature_importance_tabset == "across_drug" ~ "drug_or_class_name",
    feature_importance_tabset == "across_bug" ~ "species"
  )
  join_by_expr <- switch(paste(group_column, scale, sep = "_"),
    "species_protein" = join_by(Variable == "proteinID", "species" == "species"),
    "species_domain" = join_by(Variable == "PfamID", "species" == "species"),
    "species_gene" = join_by(Variable == "Gene", "species" == "species"),
    "drug_or_class_name_protein" = join_by(Variable == "proteinID"),
    "drug_or_class_name_domain" = join_by(Variable == "PfamID"),
    "drug_or_class_name_gene" = join_by(Variable == "Gene"),
    stop("Invalid combination of group_column and scale")
  )

  # join the top_features with annotated tables;
  if (scale == "protein") {
    annotated_table <- annotated_table |> dplyr::mutate(proteinID = stringr::str_replace(proteinID, "\\|", "."))
    top_features_df <- top_features_df %>%
      dplyr::inner_join(
        annotated_table,
        by = join_by_expr
      ) %>%
      dplyr::filter(!is.na(COG_name))
  }
  if (scale == "domain") {
    top_features_df <- top_features_df |> dplyr::mutate(Variable = stringr::str_split_i(Variable, "_", 1))
    top_features_df <- top_features_df %>%
      dplyr::inner_join(
        annotated_table,
        by = join_by_expr
      ) %>%
      dplyr::select(-Variable) %>%
      dplyr::filter(!is.na(COG_name))
  }
  # gene
  if (scale == "gene") {
    top_features_df <- top_features_df %>%
      dplyr::inner_join(
        annotated_table,
        by = join_by_expr
      ) %>%
      dplyr::filter(!is.na(COG_name))
  }

  ## group by the group columns + cog name and take max;
  top_features_df <- top_features_df %>%
    dplyr::group_by(!!rlang::sym(group_column), COG_name) %>%
    dplyr::summarize(Importance = max(Importance, na.rm = TRUE), .groups = "drop")

  # Min-max standardization for each species
  top_features_df <- top_features_df |>
    dplyr::group_by(!!rlang::sym(group_column)) |>
    dplyr::mutate(
      Importance = (Importance - min(Importance, na.rm = TRUE)) / (max(Importance, na.rm = TRUE) - min(Importance, na.rm = TRUE))
    )

  # Get the top N features based on the selected top N features options
  if (top_n_features != "all") {
    message(stringr::str_glue("Top {top_n_features} features per species and COG"))
    top_features_df <- top_features_df %>%
      dplyr::select(
        !!rlang::sym(group_column),
        COG_name,
        Importance
      ) %>%
      dplyr::distinct_all() %>%
      dplyr::group_by(!!rlang::sym(group_column)) %>%
      dplyr::slice_max(order_by = Importance, n = top_n_features) %>%
      dplyr::ungroup()
    print(top_features_df |> dplyr::group_by(!!rlang::sym(group_column)) |> summarise(n = n()) |> dplyr::arrange(desc(n)))
  }

  # across bug
  if (feature_importance_tabset == "across_bug") {
    # inner join to get the annotations + everything else
    top_features_df <- top_features_df %>%
      dplyr::inner_join(
        annotated_table,
        by = join_by("COG_name" == "COG_name", "species" == "species"),
        keep = F
      )
    readr::write_tsv(top_features_df, here::here("shinyapp", "data", "top_features", "current_top_features.tsv"))

    # build wide table (COG x species)
    vi_importance_tbl_wider <- top_features_df %>%
      dplyr::select(COG_name, Importance, species) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = species, values_from = Importance)

    # determine species columns (everything except COG_name)
    species_cols <- setdiff(colnames(vi_importance_tbl_wider), "COG_name")

    # nothing to plot if no species columns present
    if (length(species_cols) == 0) {
      return(NULL)
    }

    # compute per-COG metrics: how many species present, and secondary key (max importance)
    vi_importance_tbl_wider <- vi_importance_tbl_wider %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        species_count = sum(!is.na(c_across(all_of(species_cols)))),
        max_imp = if (all(is.na(c_across(all_of(species_cols))))) NA_real_ else max(c_across(all_of(species_cols)), na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    # sort COGs in heatmap based on # of species found in & importance score
    # order rows: primary = species_count desc, secondary = max_imp desc
    vi_importance_tbl_wider <- vi_importance_tbl_wider %>%
      dplyr::arrange(dplyr::desc(species_count), dplyr::desc(max_imp))

    # drop helper columns and create matrix in this row order
    vi_importance_tbl_mat <- vi_importance_tbl_wider %>%
      dplyr::select(-species_count, -max_imp) %>%
      tibble::column_to_rownames("COG_name") %>%
      as.matrix()

    # enforce column order
    eskape_order <- c("Efa", "Sau", "Kpn", "Aba", "Pae", "Esp")
    col_order <- intersect(eskape_order, colnames(vi_importance_tbl_mat))
    if (length(col_order) > 0) {
      vi_importance_tbl_mat <- vi_importance_tbl_mat[, col_order, drop = FALSE]
    }

    if (feature_importance_tabset == "across_bug") {
      vi_importance_tbl_mat <- vi_importance_tbl_wider %>%
        tibble::column_to_rownames("COG_name") %>%
        as.matrix()

      # Enforce ESKAPE x-axis (column) order
      eskape_order <- c("Efa", "Sau", "Kpn", "Aba", "Pae", "Esp")
      col_order <- intersect(eskape_order, colnames(vi_importance_tbl_mat))
      if (length(col_order) > 0) {
        vi_importance_tbl_mat <- vi_importance_tbl_mat[, col_order, drop = FALSE]
      }
    }
  }
  # across drugs
  if (feature_importance_tabset == "across_drug") {
    # inner join to get the annotations + everything else
    top_features_df <- top_features_df %>%
      dplyr::inner_join(
        annotated_table,
        by = join_by("COG_name" == "COG_name"),
        keep = F
      )
    readr::write_tsv(top_features_df, here::here("shinyapp", "data", "top_features", "current_top_features.tsv"))

    # ensure drug_or_class_name is a clean character column
    top_features_df <- top_features_df %>%
      dplyr::mutate(drug_or_class_name = stringr::str_trim(as.character(drug_or_class_name)))

    # build wide table (COG x drug) ensuring a single value per COG x drug (take max Importance)
    vi_importance_tbl_wider <- top_features_df %>%
      dplyr::select(COG_name, Importance, drug_or_class_name) %>%
      dplyr::group_by(COG_name, drug_or_class_name) %>%
      dplyr::summarise(Importance = max(Importance, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = drug_or_class_name, values_from = Importance)

    # determine drug columns (everything except COG_name)
    drug_cols <- setdiff(colnames(vi_importance_tbl_wider), "COG_name")

    # nothing to plot if no drug columns present
    if (length(drug_cols) == 0) {
      return(NULL)
    }

    # compute per-COG metrics: how many drugs present, and max importance score
    vi_importance_tbl_wider <- vi_importance_tbl_wider %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        drug_count = sum(!is.na(c_across(all_of(drug_cols)))),
        max_imp = if (all(is.na(c_across(all_of(drug_cols))))) NA_real_ else max(c_across(all_of(drug_cols)), na.rm = TRUE)
      ) %>%
      dplyr::ungroup()

    # order rows: primary = drug_count desc, secondary = max_imp desc
    vi_importance_tbl_wider <- vi_importance_tbl_wider %>%
      dplyr::arrange(dplyr::desc(drug_count), dplyr::desc(max_imp))

    # drop helper columns and create matrix in this row order
    vi_importance_tbl_mat <- vi_importance_tbl_wider %>%
      dplyr::select(-drug_count, -max_imp) %>%
      tibble::column_to_rownames("COG_name") %>%
      as.matrix()
  }

  # ARG vs Non-ARG coloring
  max_val <- max(vi_importance_tbl_mat, na.rm = TRUE)
  min_val <- min(vi_importance_tbl_mat, na.rm = TRUE)
  if (min_val == max_val) {
    min_val <- min_val - 0.00001
  }
  if (!exists("vi_importance_tbl_mat")) {
    print("vi_importance_tbl_mat does not exist, returning NULL.")
    return(NULL)
  }
  heatmap_vi_comparison <- ComplexHeatmap::Heatmap(
    vi_importance_tbl_mat,
    name = "Importance score",
    # column_title = column_title,
    col = circlize::colorRamp2(c(min_val, max_val), c("lightgreen", "darkgreen")),
    row_title_side = "left",
    column_title_side = "top",
    row_names_side = "left",
    column_names_side = "top",
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    row_names_gp = grid::gpar(fontsize = 12),
    column_names_gp = grid::gpar(fontsize = 12, fontface = "bold"),
    row_names_max_width = unit(12, "cm"),
    column_names_rot = 65
  )
  # makeFeatureImportancePlot(c("Sau", "Aba", "Kpn", "Pae", "Efa"), c("gentamicin"), "gene", "all_count")
  return(heatmap_vi_comparison)
}


makeCrossModelFeatureImportancePlot <- function(bug, drug, cross_model, top_n_features) {
  # load country models;
  country_top_features_df <- arrow::read_parquet("data/country_models/top_features.parquet", as_data_frame = F)
  year_top_features_df <- arrow::read_parquet("data/year_models/top_features.parquet", as_data_frame = F)

  top_n_features <- ifelse(
    top_n_features == "all", "all",
    as.numeric(top_n_features)
  )

  # get annotation table
  annotated_files <- list.files(
    path = here::here("shinyapp", "data", "Annotated"),
    pattern = stringr::str_glue(stringr::str_flatten(bug, collapse = "|")),
    full.names = TRUE
  )
  # for cross model gene is the only scale
  annotated_files <- Filter(
    function(x) grepl(pattern = stringr::str_glue("gene"), x = x),
    annotated_files
  )
  # inner join the each annotated table per species
  annotated_table <- purrr::map_dfr(
    .x = annotated_files,
    .f = function(x) {
      sp <- stringr::str_extract(basename(x), pattern = SPECIES_PATTERN)
      sp <- normalize_species(sp)
      arrow::read_parquet(x) |> dplyr::mutate(species = sp)
    }
  )
  # replace ~ with .
  annotated_table <- annotated_table %>%
    dplyr::mutate(Gene = stringr::str_replace_all(Gene, "~", "."))
  features_table <- NULL
  if (cross_model == "country") {
    country_top_features_df <- country_top_features_df %>%
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::collect()
    features_table <- country_top_features_df

    if (top_n_features != "all" & cross_model == "country") {
      country_top_features_df <- country_top_features_df %>%
        dplyr::group_by(trained_country) %>%
        dplyr::slice_max(order_by = Importance, n = top_n_features) %>%
        dplyr::ungroup()
    }

    vi_importance_tbl_wider <- country_top_features_df %>%
      dplyr::select(
        Variable,
        Importance,
        trained_country
      ) %>%
      tidyr::pivot_wider(
        names_from = c("trained_country"),
        values_from = "Importance"
      )
  }
  if (cross_model == "time") {
    year_top_features_df <- year_top_features_df %>%
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::collect()
    features_table <- year_top_features_df
    # save the data to
    if (top_n_features != "all") {
      year_top_features_df <- year_top_features_df %>%
        dplyr::group_by(trained_year) %>%
        dplyr::slice_max(order_by = Importance, n = top_n_features) %>%
        dplyr::ungroup()
    }

    vi_importance_tbl_wider <- year_top_features_df %>%
      dplyr::select(
        Variable,
        Importance,
        trained_year
      ) %>%
      tidyr::pivot_wider(
        names_from = c("trained_year"),
        values_from = "Importance"
      )
  }

  # inner join before storing
  joined_features_table <- dplyr::inner_join(
    features_table,
    annotated_table,
    by = join_by("Variable" == "Gene")
  )
  # save the mapped data in-order to visualize
  readr::write_tsv(joined_features_table, here::here("shinyapp", "data", "top_features", "cross_model_top_features.tsv"))

  # make the matrix
  vi_importance_tbl_mat <- vi_importance_tbl_wider %>%
    tibble::column_to_rownames("Variable") %>%
    as.matrix()

  # column wise standardization
  vi_importance_tbl_mat <- vi_importance_tbl_mat |>
    apply(2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

  max_val <- max(vi_importance_tbl_mat, na.rm = TRUE)
  min_val <- min(vi_importance_tbl_mat, na.rm = TRUE)

  # make heatmap
  heatmap_vi_comparison <- ComplexHeatmap::Heatmap(
    vi_importance_tbl_mat,
    name = "Importance score",
    # column_title = column_title,
    col = circlize::colorRamp2(c(min_val, max_val), c("lightgreen", "darkgreen")),
    row_title_side = "left",
    column_title_side = "top",
    row_names_side = "left",
    column_names_side = "top",
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    row_names_gp = grid::gpar(fontsize = 12),
    column_names_gp = grid::gpar(fontsize = 12, fontface = "bold"),
    row_names_max_width = unit(12, "cm"),
    column_names_rot = 65
  )
  return(heatmap_vi_comparison)
}

makeCrossModelPerformancePlot <- function(bug, drug, cross_model) {
  # country models
  country_models_df <- arrow::read_parquet("data/country_models/performance_metrics.parquet", as_data_frame = F)
  cross_country_models_df <- arrow::read_parquet("data/cross_models/performance_metrics.parquet", as_data_frame = F)

  # time models
  year_models_df <- arrow::read_parquet("data/year_models/performance_metrics.parquet", as_data_frame = F)
  cross_year_models_df <- arrow::read_parquet("data/cross_year/performance_metrics.parquet", as_data_frame = F)

  # Filter the data based on the selected bug and drug
  if (cross_model == "country") {
    country_models_df <- country_models_df %>%
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::select(trained_country, tested_country, bal_acc) %>%
      dplyr::collect()

    # cross models
    cross_country_models_df <- cross_country_models_df |>
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::select(trained_country, tested_country, bal_acc) %>%
      dplyr::collect()

    # bind the data
    models_performance <- bind_rows(country_models_df, cross_country_models_df) |>
      tidyr::pivot_wider(
        names_from = trained_country,
        values_from = bal_acc
      )
    models_performance <- models_performance %>%
      tibble::column_to_rownames("tested_country") %>%
      as.matrix()
    row_label <- "Tested country"
    column_label <- "Trained country"
  }
  if (cross_model == "time") {
    year_models_df <- year_models_df %>%
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::select(trained_year, tested_year, bal_acc) %>%
      dplyr::collect()

    # cross models
    cross_year_models_df <- cross_year_models_df |>
      dplyr::filter(species %in% bug) %>%
      dplyr::filter(drug_or_class_name %in% drug) %>%
      dplyr::select(trained_year, tested_year, nmcc) %>%
      dplyr::collect()

    # bind the data
    models_performance <- bind_rows(year_models_df, cross_year_models_df) |>
      tidyr::pivot_wider(
        names_from = trained_year,
        values_from = bal_acc
      )
    models_performance <- models_performance %>%
      tibble::column_to_rownames("tested_year") %>%
      as.matrix()

    row_label <- "Tested years"
    column_label <- "Trained years"
  }

  # join the slides;
  min_val <- min(models_performance, na.rm = TRUE)
  max_val <- max(models_performance, na.rm = TRUE)

  if (min_val == max_val) {
    min_val <- min_val - 0.00001
  }

  # heatmap
  heatmap_model_performance <- ComplexHeatmap::Heatmap(
    models_performance,
    name = "Balanced Accuracy (bal_acc)",
    col = circlize::colorRamp2(c(min_val, max_val), c("lightblue", "darkblue")),
    row_title = "Tested Country", # Row title
    column_title = "Trained Country", # Column title
    row_title_side = "left", # Position Tested Country left of the row names
    column_title_side = "top", # Position Trained Country above the column names
    row_title_gp = grid::gpar(fontsize = 14, fontface = "bold"), # Adjust font size and style for row title
    column_title_gp = grid::gpar(fontsize = 14, fontface = "bold"), # Adjust font size and style for column title
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    show_column_names = TRUE,
    row_names_gp = grid::gpar(fontsize = 12), # Font size for individual row names (country labels)
    column_names_gp = grid::gpar(fontsize = 12, fontface = "bold"), # Font size for individual column names (country labels)
    row_names_max_width = unit(12, "cm"), # Adjust row name width
    column_names_rot = 0, # Ensure column names are horizontal for better alignment with Trained Country
    heatmap_legend_param = list(
      title = "Balanced Accuracy", # Title of the legend
      at = round(seq(min_val, max_val, length.out = 5), 2), # Tick positions rounded to two decimals
      labels = round(seq(min_val, max_val, length.out = 5), 2) # Labels rounded to two decimals
    )
  )
  return(heatmap_model_performance)
}

makeFeatureImportTable <- function(feature_import_table) {
  # Early exit for empty/zero-column data
  if (is.null(feature_import_table) || ncol(feature_import_table) == 0) {
    return(DT::datatable(tibble::tibble(), options = list(dom = "t"), rownames = FALSE))
  }

  # Preferred display order (only those that exist will be used)
  cols_priority <- c(
    "species", "drug_or_class_name",
    "COG_name", "COG_description", "ARG_name", "ARG_description",
    "Gene", "Annotation", "accession",
    "feature_type", "data_type", "Importance"
  )
  existing <- intersect(cols_priority, names(feature_import_table))

  feature_import_table <- feature_import_table |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ formatC(.x, format = "e", digits = 3))
    ) |>
    dplyr::mutate(ARG_name = stringr::str_replace_all(string = ARG_name, pattern = "non-ARG", "-")) |>
    # Reorder for display: preferred columns first, then everything else
    dplyr::select(dplyr::all_of(existing), dplyr::everything())

  DT::datatable(
    feature_import_table |>
      dplyr::mutate(accession = stringr::str_split_i(accession, "\\.", 1)) |>
      dplyr::mutate(
        COG_name = stringr::str_glue("<a href='https://www.ncbi.nlm.nih.gov/research/cog/cog/{COG_name}' target='_blank' style='color:#1a73e8; text-decoration: underline;'>{COG_name}</a>")
      ) |>
      dplyr::mutate(
        accession = dplyr::case_when(
          !is.na(accession) ~ stringr::str_glue("<a href='https://www.ncbi.nlm.nih.gov/genome/annotation_prok/evidence/{accession}' target='_blank'style='color:#1a73e8; text-decoration: underline;'>{accession}</a>"),
          TRUE ~ accession
        )
      ),
    options = list(scrollX = TRUE, autoWidth = FALSE, orderClasses = TRUE),
    class = "display nowrap stripe",
    rownames = FALSE,
    width = "100%",
    escape = FALSE
  )
}
