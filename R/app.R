#' AMR Shiny dashboard application
#'
#' Creates and returns a Shiny application for exploring antimicrobial
#' resistance data and machine learning model results.
#'
#' @return A Shiny application object
#' @export
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom utils head write.csv
#' @importFrom shinyjs useShinyjs
#' @examples
#' if (interactive()) {
#'   app <- launchAMRDashboard()
#'   shiny::runApp(app)
#' }
launchAMRDashboard <- function() {
  # Bug choices
  bug_choices <- c(
    "Enterococcus faecium" = "Efa",
    "Staphylococcus aureus" = "Sau",
    "Klebsiella pneumoniae" = "Kpn",
    "Acinetobacter baumannii" = "Aba",
    "Pseudomonas aeruginosa" = "Pae",
    "Enterobacter spp." = "Esp.",
    "Escherichia coli" = "Eco",
    "Campylobacter jejuni" = "Cje",
    "Staphylococcus epidermidis" = "Sep"
  )

  # ESKAPE bugs (sorted in ESKAPE order)
  eskape_bugs <- c(
    "Enterococcus faecium" = "Efa",
    "Staphylococcus aureus" = "Sau",
    "Klebsiella pneumoniae" = "Kpn",
    "Acinetobacter baumannii" = "Aba",
    "Pseudomonas aeruginosa" = "Pae",
    "Enterobacter spp." = "Esp."
  )

  # UI
  ui <- tagList(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(system.file("app/www/style.css", package = "amRshiny"))),
    tags$head(
      tags$style(HTML("
                      .innerbox {
                        /*border: 2px solid black;*/
                        box-shadow: 2px 2px 3px 3px #ccc;
                        margin: auto;
                        padding: 20px;
                      }

                      .bord {
                        margin: auto;
                        padding: 20px;
                      }

                      .lightblue-link{
                        color:#11aad9;
                      }

                      .iMargin{
                        margin: 5px;
                      }

                     a{
                        color: #141414;
                        text-decoration:none;
                      }


                      a:visited{
                        color:none;
                      }

                      .noDec{
                        color:white;
                        text-decoration:none;
                      }

                      .zoom:hover {
                        /*color:white;*/
                        transform: scale(1.3);
                      }

                      .note-box {
                        margin: auto;
                        margin-top: 10px;
                        margin-bottom: 10px;
                        width: 80%;
                        border: 1px solid #78adff;
                        padding: 10px;
                        background-color: #b7dceb;
                        border-radius: 5px;
                        text-align: center;
                      }
                    "))
    ),
    navbarPage(
      id = "tabselected",
      selected = "home",
      title = "",
      tabPanel(
        title = div(class = "zoom", "AMR"),
        value = "dashboard",
      ),
      # 2. Home icon tab (right of AMR dashboard)
      tabPanel(
        title = icon("home", class = "home-tab-icon"),
        value = "home",
        fluidPage(
          h2("From sequence to signature: Machine learning uncovers multiscale feature landscapes that predict AMR across ESKAPE pathogens"),
          tags$p(
            tags$strong("Authors: "),
            "Abhirupa Ghosh¹,#, Evan P Brenner¹,#, Charmie K Vang¹,#, Ethan P Wolfe¹,#, Emily Boyer¹, Raymond L Lesiyon¹, Keenan R Manpearl¹, Vignesh Sridhar², Joseph T Burke², Jacob D Krol¹, Jill MR Bilodeaux¹, Janani Ravi¹,*."
          ),
          tags$p(
            "¹Department of Biomedical Informatics, Center for Health Artificial Intelligence, University of Colorado Anschutz, Aurora, CO, USA; ",
            "²Department of Pathobiology and Diagnostic Investigation, Michigan State University, East Lansing, MI, USA. ",
            tags$span(style = "font-style: italic", "#Co-primary, contributed equally. *Corresponding author: janani.ravi@cuanschutz.edu")
          ),
          br(),
          h4("Abstract"),
          tags$p("Since the clinical introduction of antibiotics in the 1940s, antimicrobial resistance (AMR) has become an increasingly dire threat to global public health. Pathogens acquire AMR much faster than we discover new drugs (antibiotics), warranting innovative methods to better understand its molecular underpinnings. Traditional approaches for detecting AMR in novel bacterial strains are time-consuming and labor-intensive. However, advances in sequencing technology offer a plethora of bacterial genome data, and computational approaches like machine learning (ML) provide an optimistic scope for in silico AMR prediction."),
          tags$p("Here, we introduce a comprehensive multiscale ML approach to predict AMR phenotypes and identify AMR molecular features associated with a single drug or drug family, stratified by time and geographical locations. As a case study, we focus on a subset of the World Health Organization's Bacterial Priority Pathogens, the frequently drug-resistant and nosocomial ESKAPE pathogens: Enterococcus faecium, Staphylococcus aureus, Klebsiella pneumoniae, Acinetobacter baumannii, Pseudomonas aeruginosa, and Enterobacter species."),
          tags$p("We started with sequenced genomes with lab-derived AMR phenotypes, constructed pangenomes, clustered gene and protein sequences, and extracted protein domains to generate pangenomic features across molecular scales. To uncover the molecular mechanisms behind drug-/drug class-specific resistance, we trained logistic regression ML models on our datasets. These yielded ranked lists of AMR-associated genes, proteins, and domains. In addition to recapitulating known AMR features, our models identified novel candidates for experimental validation. The models were performant across molecular scales, data types, and drugs while achieving a median normalized Matthews correlation coefficient of 0.89."),
          tags$p("Prediction performance showed resilience even when evaluated on geographical and temporal holdouts. We also evaluated model generalizability and cross-resistance across the drug-/drug class-specific models cross-tested on other available drug-/drug class genomes. Finally, we uncovered multiple drug class resistance features using multiclass and multilabel models."),
          tags$p("Our holistic approach promises reliable prediction of existing and developing resistance in newly sequenced pathogen genomes, while pinpointing the mechanistic molecular contributors of AMR. All our models and results are available at our interactive web app, https://jravilab.org/amr."),
          tags$hr(),
        )
      ),
      # other tabs
      metadataUI(),
      modelPerfUI(),
      featureImportanceUI(),
      crossModelComparisonUI(),
      queryDataUI()
    ),
    tags$footer(
      class = "footer",
      tags$div(
        "© JRaviLab 2026 | ",
        tags$a(href = "https://jravilab.github.io", "jravilab.github.io"),
        " | ",
        tags$a(href = "https://twitter.com/jravilab", "@jravilab"),
        " | janani.ravi@cuanschutz.edu |",
        tags$a(
          href = "https://docs.google.com/forms/d/e/1FAIpQLSdwFo5Wwt_t4WGthDGgc1EYhvvKagUEb3RiNLdsbnpDlYTk7Q/viewform?usp=dialog",
          icon("question-circle"),
          " Help Doc"
        )
      )
    ),
  )
  ## Server
  server <- function(input, output, session) {
    # Initialize reactive values
    genome_data <- reactiveVal(NULL)
    drug_class_map <- reactiveVal(NULL)
    amr_drugs <- reactiveVal(NULL)
    ml_metrics_results <- reactiveVal(NULL)
    ml_ui_trigger <- reactiveVal(0)

    # reactive reader
    feature_import_table_reactive <- reactiveFileReader(
      intervalMillis = 1000, # check every 1 second
      session = session,
      filePath = here::here("shinyapp", "data", "top_features", "current_top_features.tsv"),
      readFunc = readr::read_tsv
    )

    cross_model_import_feature_table_reactive <- reactiveFileReader(
      intervalMillis = 1000, # check every 1 second
      session = session,
      filePath = here::here("shinyapp", "data", "top_features", "cross_model_top_features.tsv"),
      readFunc = readr::read_tsv
    )

    loadDrugClassMapRec <- reactive({
      loadDrugClassMap() %>%
        dplyr::select(drug.antibiotic_name, drug_class) %>%
        dplyr::distinct()
    })

    # reactive normalized UI selection for bug_search_amr_across_bug
    bug_norm_input <- reactive({
      req(input$bug_search_amr_across_bug) # ensure value exists
      normalize_species(input$bug_search_amr_across_bug)
    })

    # dynamic ML ui
    output$ml_drug_toggle_ui <- renderUI({
      # For example: only allow selection among the *other two* categories
      choices <- switch(input$ml_drug_toggle_top,
        "bug" = c("Drug Class" = "class", "Drug" = "drug"),
        "class" = c("Bug" = "bug", "Drug" = "drug"),
        "drug" = c("Bug" = "bug", "Drug Class" = "class")
      )

      radioButtons(
        inputId = "ml_drug_toggle",
        label = tags$label("Select by", style = "font-size: 15px;"),
        choices = choices,
        selected = names(choices)[1],
        inline = TRUE
      )
    })

    # observe to load all the data
    observe({
      drug_class_map(loadDrugClassMapRec())
    })

    # clear the selected bug when the user changes the drug/drug class
    observeEvent(input$across_bug_id, {
      updateSelectInput(
        session,
        inputId = "bug_search_amr_across_bug",
        choices = bug_choices,
        selected = unname(bug_choices) # Default to ESKAPE bugs
      )
    })

    observeEvent(c(input$bug_search_amr_across_bug, input$across_bug_id, input$bug_drug_comp_model_scale, input$data_type), {
      req(input$bug_search_amr_across_bug)
      message("Across bug feature comparison: ")

      # use reactive normalized selection
      bn <- bug_norm_input()

      drug_or_class_name_vec <- arrow::read_parquet("data/top_features/top_features.parquet") |>
        dplyr::filter(normalize_species(species) %in% bn) |>
        dplyr::filter(feature_type %in% input$bug_drug_comp_model_scale) |>
        dplyr::filter(data_type %in% input$data_type) |>
        dplyr::pull(drug_or_class_name) |>
        unique()

      if (identical(input$across_bug_id, "drug")) {
        drugs_vec <- unique(drug_class_map() |> dplyr::pull(drug.antibiotic_name))
        drugs_vec <- sort(intersect(drugs_vec, drug_or_class_name_vec))
        sel <- intersect("gentamicin", drugs_vec)
        if (length(sel) == 0) sel <- head(drugs_vec, 1)
        updateSelectInput(session, "amr_drug_ml_across_bug", choices = drugs_vec, selected = sel)
      } else {
        drugs_class_vec <- unique(drug_class_map() |> dplyr::pull(drug_class))
        drugs_class_vec <- sort(intersect(drugs_class_vec, drug_or_class_name_vec))
        sel <- head(drugs_class_vec, 1)
        updateSelectInput(session, "amr_drug_class_ml_across_bug", choices = drugs_class_vec, selected = sel)
      }
    })


    # For drug/drug class (across drug) — FIX bug input and filter by scale/data type
    observeEvent(c(input$bug_search_amr_across_drug, input$across_drug_id, input$bug_drug_comp_model_scale, input$data_type), {
      req(input$bug_search_amr_across_drug)
      message("Across drug feature comparison: ")

      # normalize selection for the across-drug observer
      bn <- normalize_species(input$bug_search_amr_across_drug)

      drug_or_class_name_vec <- arrow::read_parquet("data/top_features/top_features.parquet") |>
        dplyr::filter(normalize_species(species) %in% bn) |> # <-- was _across_bug
        dplyr::filter(feature_type %in% input$bug_drug_comp_model_scale) |>
        dplyr::filter(data_type %in% input$data_type) |>
        dplyr::pull(drug_or_class_name) |>
        unique()

      if (identical(input$across_drug_id, "drug")) {
        drugs_vec <- unique(drug_class_map() |> dplyr::pull(drug.antibiotic_name))
        drugs_vec <- sort(intersect(drugs_vec, drug_or_class_name_vec))
        # keep previous selection if valid; else use preferred defaults or first few
        prev <- isolate(input$amr_drug_ml_across_drug)
        sel <- prev[prev %in% drugs_vec]
        if (length(sel) == 0) {
          pref <- c("oxacillin", "penicillin", "methicillin")
          sel <- intersect(pref, drugs_vec)
          if (length(sel) == 0) sel <- head(drugs_vec, min(3, length(drugs_vec)))
        }
        updateSelectInput(session, "amr_drug_ml_across_drug", choices = drugs_vec, selected = sel)
      } else {
        drugs_class_vec <- unique(drug_class_map() |> dplyr::pull(drug_class))
        drugs_class_vec <- sort(intersect(drugs_class_vec, drug_or_class_name_vec))
        prev <- isolate(input$amr_drug_class_ml_across_drug)
        sel <- prev[prev %in% drugs_class_vec]
        if (length(sel) == 0) {
          pref <- c("cephalosporins", "lincosamides", "macrolides", "penicillins")
          sel <- intersect(pref, drugs_class_vec)
          if (length(sel) == 0) sel <- head(drugs_class_vec, min(4, length(drugs_class_vec)))
        }
        updateSelectInput(session, "amr_drug_class_ml_across_drug", choices = drugs_class_vec, selected = sel)
      }
    })

    # Update the model performance tables
    observeEvent(input$bug_ml_perf_id, {
      data <- readr::read_tsv(here::here("shinyapp", "data", "performance_metrics", "all_metrics.tsv")) %>%
        dplyr::filter(species %in% input$bug_ml_perf_id)
      # get drug classes;
      drug_class_vec <- pull(dplyr::filter(data, drug_level == "drug_class"), drug_or_drug_class) %>%
        unique() %>%
        sort()

      # update the drug and drug class inputs
      updateSelectInput(
        session,
        inputId = "drug_class_ml_perf_id",
        choices = c("all", drug_class_vec),
        selected = "aminoglycosides" # ifelse(length(drug_class_vec) > 0, "all", NULL)
      )
    })
    # model holdouts filtering

    observeEvent(input$bug_holdouts_id,
      {
        req(input$bug_holdouts_id)

        # Build choices filtered to the selected 3-letter species code
        choices <- getHoldoutsDrugChoices(bug = input$bug_holdouts_id)

        # Keep the user’s current selection if still valid; otherwise pick first
        prev <- isolate(input$holdouts_drug)
        sel <- if (!is.null(prev) && prev %in% choices) prev else if (length(choices)) choices[[1]] else NULL

        # Update the dropdown (use updateSelectizeInput if your UI uses selectize=TRUE)
        updateSelectizeInput(
          session,
          inputId  = "holdouts_drug",
          choices  = choices,
          selected = sel,
          server   = TRUE
        )
      },
      ignoreInit = FALSE
    ) # run once on app load so it populates immediately


    observeEvent(input$drug_class_ml_perf_id, {
      req(input$drug_class_ml_perf_id)
      data <- readr::read_tsv(here::here("shinyapp", "data", "performance_metrics", "all_metrics.tsv")) %>%
        dplyr::filter(species %in% input$bug_ml_perf_id)
      drug_vec <- pull(dplyr::filter(data, drug_level == "drug"), drug_or_drug_class) %>%
        unique() %>%
        sort()
      if (input$drug_class_ml_perf_id != "all") {
        # load drug maps
        drug_within_class_vec <- readr::read_csv(here::here("shinyapp", "data", "drug_cleanup.csv")) %>%
          dplyr::select(predicted_drug, drug_classes) %>%
          dplyr::mutate(drug_classes = stringr::str_replace_all(drug_classes, pattern = " |-", "_")) %>%
          dplyr::filter(drug_classes %in% input$drug_class_ml_perf_id) %>%
          dplyr::filter(predicted_drug %in% drug_vec) %>%
          dplyr::pull(predicted_drug)
        drug_within_class_vec <- drug_within_class_vec[!is.na(drug_within_class_vec)]

        # get the drug class for the selected drug
        updateSelectInput(
          session,
          inputId = "drug_ml_perf_id",
          choices = drug_within_class_vec,
          selected = ifelse(length(drug_within_class_vec) > 0, drug_within_class_vec[1], NULL)
        )
      } else {
        # if all drug classes are selected, then update the drug input with all drugs
        updateSelectInput(
          session,
          inputId = "drug_ml_perf_id",
          choices = drug_vec,
          selected = "gentamicin"
          # ifelse(length(drug_vec) > 0, drug_vec[1], NULL)
        )
      }
    })


    ## plot UI
    output$geo_isolate_plot_ui <- renderUI({
      fluidRow(
        div(
          box(
            title = "",
            width = 12,
            plotly::plotlyOutput("geo_isolate_plot")
          )
        )
      )
    })

    output$r_s_across_time_ui <- renderUI({
      fluidRow(
        box(
          title = "", # "Resistance vs Susceptible across time",
          width = 12,
          plotOutput("r_s_across_time_plot")
        )
      )
    })

    output$host_isolate_plot_ui <- renderUI({
      fluidRow(
        box(
          title = "", # "Host",
          width = 12,
          plotOutput("host_isolate_plot")
        )
      )
    })

    output$isolation_source_ui <- renderUI({
      fluidRow(
        box(
          title = "", # "Isolation source",
          width = 12,
          plotOutput("isolation_source_plot")
        )
      )
    })

    output$resistance_vs_susceptible_ui <- renderUI({
      fluidRow(
        box(
          title = "", # "Data availability",
          width = 12,
          plotOutput("resistance_vs_susceptible_plot")
        )
      )
    })

    ## get a quick summary plot;
    observeEvent(input$bug_metadata_id, {
      output$quick_metadata_stats <- renderUI({
        metadata <- purrr::map_dfr(
          .x = input$bug_metadata_id,
          .f = function(x) {
            fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
            if (file.exists(fp)) {
              arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
            } else {
              return(tibble())
            }
          }
        )
        makeQuickStats(metadata)
      })
    })

    ## rendering plots
    observe({
      if (
        is.null(input$bug_search) &&
          is.null(input$amr_drug_search) &&
          is.null(input$amr_drug_class_search)
      ) {
        return(
          shiny::wellPanel(
            h4("Please select a bug and drug/drug class to display the Sankey plot.")
          )
        )
      }

      output$sankey_all_metadata_plot <- networkD3::renderSankeyNetwork({
        data <- genome_data()
        amr_drugs <- amrDrugsRec()

        if (!is.null(amr_drugs) && length(amr_drugs) > 0) {
          data <- data %>%
            dplyr::filter(genome_drug.antibiotic %in% amr_drugs)
        }
        makeSankeyPlot(data)
      })
    })

    ## making the data availability plot
    observe({
      req(input$bug_metadata_id)

      output$resistance_vs_susceptible_plot <- renderPlot({
        metadata <- purrr::map_dfr(
          .x = input$bug_metadata_id,
          .f = function(x) {
            fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
            if (file.exists(fp)) {
              arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
            } else {
              return(tibble())
            }
          }
        )
        makeDatAvailabilityPlot(metadata)
      })
    })

    output$geo_isolate_plot <- plotly::renderPlotly({
      data <- purrr::map_dfr(
        .x = input$bug_metadata_id,
        .f = function(x) {
          fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
          print(fp)
          if (file.exists(fp)) {
            arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
          } else {
            return(tibble())
          }
        }
      )

      data <- data %>%
        dplyr::filter(genome.isolation_country != "") %>%
        dplyr::mutate(
          genome_drug.evidence = case_when(
            genome_drug.laboratory_typing_method %in%
              c(
                "Disk diffusion",
                "MIC",
                "Broth dilution",
                "Agar dilution"
              ) ~ "Laboratory Method",
            genome_drug.laboratory_typing_method ==
              "Computational Prediction" ~ "Computational Method",
            TRUE ~ genome_drug.laboratory_typing_method
          )
        ) %>%
        group_by(genome.isolation_country, genome_drug.antibiotic) %>%
        summarize(count = n(), .groups = "drop") %>%
        dplyr::group_by(genome.isolation_country) %>%
        dplyr::summarise(count = sum(count), .groups = "drop") %>%
        dplyr::collect()

      makeGeoChloroPlot(data)
    })

    output$r_s_across_time_plot <- renderPlot({
      data <- purrr::map_dfr(
        .x = input$bug_metadata_id,
        .f = function(x) {
          fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
          if (file.exists(fp)) {
            arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
          } else {
            return(tibble())
          }
        }
      )

      data <- data %>%
        dplyr::mutate(
          genome_drug.evidence = case_when(
            genome_drug.laboratory_typing_method %in%
              c(
                "Disk diffusion",
                "MIC",
                "Broth dilution",
                "Agar dilution"
              ) ~ "Laboratory Method",
            genome_drug.laboratory_typing_method ==
              "Computational Prediction" ~ "Computational Method",
            TRUE ~ genome_drug.laboratory_typing_method
          )
        ) %>%
        dplyr::filter(!is.na(genome.collection_year)) %>%
        group_by(
          genome_drug.antibiotic,
          genome_drug.resistant_phenotype,
          genome.isolation_country,
          genome.collection_year
        ) %>%
        summarize(n = n()) %>%
        dplyr::collect()
      # print(data)
      makeTimeSeriesAMRPlot(data, input$amr_drug_search)
    })

    output$host_isolate_plot <- renderPlot({
      data <- purrr::map_dfr(
        .x = input$bug_metadata_id,
        .f = function(x) {
          fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
          if (file.exists(fp)) {
            arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
          } else {
            return(tibble())
          }
        }
      )

      data <- data %>%
        dplyr::filter(genome.host_common_name != "") %>%
        dplyr::mutate(
          genome_drug.evidence = case_when(
            genome_drug.laboratory_typing_method %in%
              c(
                "Disk diffusion",
                "MIC",
                "Broth dilution",
                "Agar dilution"
              ) ~ "Laboratory Method",
            genome_drug.laboratory_typing_method ==
              "Computational Prediction" ~ "Computational Method",
            TRUE ~ genome_drug.laboratory_typing_method
          )
        ) %>%
        dplyr::collect()

      makeHostIsolatePlot(data)
    })

    output$isolation_source_plot <- renderPlot({
      data <- purrr::map_dfr(
        .x = input$bug_metadata_id,
        .f = function(x) {
          fp <- here::here("shinyapp", "data", "Metadata", stringr::str_glue("{x}_metadata.parquet"))
          if (file.exists(fp)) {
            arrow::read_parquet(fp) |> dplyr::mutate(species = input$bug_metadata_id)
          } else {
            return(tibble())
          }
        }
      )

      data <- data %>%
        dplyr::filter(genome.host_common_name != "") %>%
        dplyr::mutate(
          genome_drug.evidence = case_when(
            genome_drug.laboratory_typing_method %in%
              c(
                "Disk diffusion",
                "MIC",
                "Broth dilution",
                "Agar dilution"
              ) ~ "Laboratory Method",
            genome_drug.laboratory_typing_method ==
              "Computational Prediction" ~ "Computational Method",
            TRUE ~ genome_drug.laboratory_typing_method
          )
        ) %>%
        dplyr::collect()
      makeIsolationSourcesPlot(data)
    })


    ## ML Metrics
    # plotly::renderPlotly
    output$model_perfomance_plot <- renderPlot({
      makeModelPerformancePlot(
        input$bug_ml_perf_id,
        input$model_scale,
        input$data_type,
        input$model_metrics,
        input$drug_class_ml_perf_id,
        input$drug_ml_perf_id
      )
    })

    observe({
      output$across_bug_feature_importance_plot <- renderPlot({
        # print("Feature importance across bug: drugs")
        if (is.null(input$across_bug_id)) {
          return(NULL)
        }
        if (input$across_bug_id == "drug") {
          ht <- makeFeatureImportancePlot(
            input$bug_search_amr_across_bug,
            input$amr_drug_ml_across_bug,
            input$bug_drug_comp_model_scale,
            input$data_type,
            input$top_n_features,
            input$feature_importance_tabset
          )
          return(
            draw(
              ht,
              heatmap_legend_side = "right"
            )
          )
        }

        if (input$across_bug_id == "drug_class") {
          ht <- makeFeatureImportancePlot(
            input$bug_search_amr_across_bug,
            input$amr_drug_class_ml_across_bug,
            input$bug_drug_comp_model_scale,
            input$data_type,
            input$top_n_features,
            input$feature_importance_tabset
          )
          return(
            draw(
              ht,
              heatmap_legend_side = "right"
            )
          )
        }
      })
    })

    observe({
      output$across_drug_feature_importance_plot <- renderPlot({
        if (is.null(input$across_drug_id)) {
          return(NULL)
        }
        if (input$across_drug_id == "drug") {
          ht <- makeFeatureImportancePlot(
            input$bug_search_amr_across_drug,
            input$amr_drug_ml_across_drug,
            input$bug_drug_comp_model_scale,
            input$data_type,
            input$top_n_features,
            input$feature_importance_tabset
          )
          return(
            draw(
              ht,
              heatmap_legend_side = "right"
            )
          )
        }

        if (input$across_drug_id == "drug_class") {
          ht <- makeFeatureImportancePlot(
            input$bug_search_amr_across_drug,
            input$amr_drug_class_ml_across_drug,
            input$bug_drug_comp_model_scale,
            input$data_type,
            input$top_n_features,
            input$feature_importance_tabset
          )
          return(
            draw(
              ht,
              heatmap_legend_side = "right"
            )
          )
        }
      })
    })

    # observe({
    #   cat("Current input values:\n")
    #   for (id in names(input)) {
    #     cat(id, ":", input[[id]], "\n")
    #   }
    #   cat("------\n")
    # })
    observe({
      output$feature_importance_plot <- renderPlot({
        # req(input$bug_search_amr, input$amr_drug_ml_across_bug)
        ht <- makeFeatureImportancePlot(
          input$bug_ml_perf_id,
          input$amr_drug_ml_across_bug,
          input$model_scale,
          input$data_type,
          input$top_n_features,
          input$feature_importance_tabset
        )
        return(
          draw(
            ht,
            heatmap_legend_side = "right"
          )
        )
      })
      output$feature_importance_table <- DT::renderDataTable({
        # req(input$bug_search_amr, input$amr_drug_ml_across_bug)
        feature_import_table <- feature_import_table_reactive()
        if (is.null(feature_import_table)) {
          return(NULL)
        }
        makeFeatureImportTable(feature_import_table)
      })
    })
    # Feature importance tables;
    # Across bug and across drug.
    output$across_bug_feature_importance_table <- DT::renderDataTable({
      req(feature_import_table_reactive())
      feature_import_table <- feature_import_table_reactive()
      if (is.null(feature_import_table)) {
        return(NULL)
      }
      makeFeatureImportTable(feature_import_table)
    })

    output$across_drug_feature_importance_table <- DT::renderDataTable({
      req(feature_import_table_reactive())
      feature_import_table <- feature_import_table_reactive()
      if (is.null(feature_import_table)) {
        return(NULL)
      }
      makeFeatureImportTable(feature_import_table)
    })

    # show cross model feature importance table;
    output$cross_model_feature_importance_table <- DT::renderDataTable({
      req(cross_model_import_feature_table_reactive())
      feature_import_table <- cross_model_import_feature_table_reactive()
      # delete the file;
      if (is.null(feature_import_table)) {
        return(NULL)
      }
      makeFeatureImportTable(feature_import_table)
    })

    # model comparisons;
    observeEvent(input$bug_cross_model_comparison_id,
      {
        bug <- input$bug_cross_model_comparison_id

        # Gather all Drug/Drug class options across holdout sources for this bug
        drugs_vec <- getHoldoutsDrugChoices(bug)

        # Initial default = "lincosamides" if present, else first option (users can still change it)
        sel <- if ("lincosamides" %in% drugs_vec) "lincosamides" else if (length(drugs_vec)) drugs_vec[1] else NULL

        updateSelectInput(
          session,
          inputId = "drug_cross_model_comparison_id",
          choices = drugs_vec,
          selected = sel
        )
      },
      ignoreInit = FALSE
    )

    observe({
      output$cross_model_perf_plot <- renderPlot({
        # req(input$bug_cross_model_comparision_id, input$drug_cross_model_comparision_id)
        ht <- makeCrossModelPerformancePlot(
          input$bug_cross_model_comparison_id,
          input$drug_cross_model_comparison_id,
          input$cross_model_comparison
        )
        return(
          draw(
            ht,
            heatmap_legend_side = "left"
          )
        )
      })
      output$cross_model_feature_importance_plot <- renderPlot({
        ht <- makeCrossModelFeatureImportancePlot(
          input$bug_cross_model_comparison_id,
          input$drug_cross_model_comparison_id,
          input$cross_model_comparison,
          input$cross_model_top_n_features
        )
        return(
          draw(
            ht,
            heatmap_legend_side = "left"
          )
        )
      })

      ## Query Data Tab logic
      # Load performance metrics data
      queryData <- reactiveVal(loadMLResults())

      observe({
        data <- queryData() # Get data

        # Exclude columns
        valid_columns <- setdiff(names(data), c("model_shapes", "model_colors"))

        # Update dropdown options
        updateSelectizeInput(
          session,
          "query_data_columns",
          choices = valid_columns, # Get column names
          selected = valid_columns # Default selection: all columns
        )
      })

      # Render the data table
      output$queryDataTable <- DT::renderDataTable({
        data <- queryData() # Get data

        # Exclude unwanted columns
        valid_columns <- setdiff(names(data), c("model_shapes", "model_colors"))

        selected_cols <- input$query_data_columns

        if (is.null(selected_cols)) {
          data[, valid_columns, drop = FALSE] # Return data
        } else {
          data[, intersect(valid_columns, selected_cols), drop = FALSE] # Return filtered data
        }
      })

      # Download filtered data
      output$query_data_download <- downloadHandler(
        filename = function() {
          paste("query_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          data <- queryData() # Get data
          # Exclude unwanted columns
          valid_columns <- setdiff(names(data), c("model_shapes", "model_colors")) # Filter columns

          selected_cols <- input$query_data_columns # Get user-selected columns

          if (is.null(selected_cols)) {
            write.csv(data[, valid_columns, drop = FALSE], file, row.names = FALSE) # Download only valid columns
          } else {
            write.csv(data[, intersect(valid_columns, selected_cols), drop = FALSE], file, row.names = FALSE) # Download user selection intersected with valid columns
          }
        }
      )
      # Top Features Table Logic
      topFeatures <- reactiveVal(loadTopFeat()) # Use the loadTopFeat() function to load data

      observe({
        data <- topFeatures() # Fetch data

        # Dynamically update dropdown with column names
        updateSelectizeInput(
          session,
          "top_features_columns",
          choices = names(data), # Populate dropdown with column names
          selected = names(data) # Default: all columns selected
        )
      })

      output$topFeaturesTable <- DT::renderDataTable({
        data <- topFeatures() # Get the data

        selected_columns <- input$top_features_columns # Get user-selected columns

        if (is.null(selected_columns)) {
          data # Show full data
        } else {
          data[, selected_columns, drop = FALSE] # Filter columns
        }
      })

      output$top_features_download <- downloadHandler(
        filename = function() {
          paste("top_features_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          data <- topFeatures() # Fetch data

          selected_columns <- input$top_features_columns # Get user-selected columns

          if (is.null(selected_columns)) {
            write.csv(data, file, row.names = FALSE) # Write complete data
          } else {
            write.csv(data[, selected_columns, drop = FALSE], file, row.names = FALSE) # Filter columns in downloaded file
          }
        }
      )
    })
  }

  # Return the Shiny application object
  shiny::shinyApp(ui = ui, server = server)
}
