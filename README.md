
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amR_shiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/NewShinyPackage)](https://CRAN.R-project.org/package=NewShinyPackage)
[![R-CMD-check](https://github.com/JRaviLab/NewShinyPackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JRaviLab/NewShinyPackage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**amR_shiny** is an interactive Shiny dashboard for exploring
antimicrobial resistance (AMR) data and machine learning model results.

Part of the **AMR package suite**: - **amR_data**: Data (and metadata)
preparation from BV-BRC - **amR_ml**: ML modeling and analysis -
**amR_shiny**: Interactive visualization (this package)

## Features

- **Metadata exploration**: Geographic distribution, temporal trends,
  host analysis
- **Model performance**: Compare ML models across species, drugs, and
  molecular scales (genes, proteins, domains, structures)
- **Feature importance**: Identify key predictive features with
  interactive heatmaps
- **Cross-model analysis**: Compare models trained on different
  stratifications (country, year)
- **Publication-quality exports**: Download plots and tables
- **Modular design**: Extensible UI components

## Installation

### Current (development version)

The package is currently available via GitHub and will be submitted to
Bioconductor.

``` r
# Install BiocManager if needed
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# Install Bioconductor dependencies
BiocManager::install("ComplexHeatmap")

# Install amR_shiny from GitHub
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

devtools::install_github("JRaviLab/amR_shiny")
```

### Future (Bioconductor release)

Once submitted to Bioconductor, installation will be:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("amR_shiny")
```

### Optional dependencies

``` r
# For Sankey diagrams (if available for your R version)
install.packages("sankeyD3")

# For enhanced data processing
BiocManager::install("arrow")
```

## Quick start

``` r
library(amR_shiny)

# Launch the dashboard
launch_dashboard()
```

The dashboard will open in your default web browser.

## Usage

### Dashboard navigation

The dashboard includes several tabs:

1.  **Home**: Overview and project information
2.  **Metadata**: Explore geographic, temporal, and host metadata
    - Interactive maps and treemaps
    - Temporal trends
    - Host distribution
3.  **Model performance**: Compare ML model metrics
    - Filter by species, drug, molecular scale
    - View confusion matrices
    - Compare performance across models
4.  **Feature importance**: Analyze predictive features
    - Top features by importance
    - Cross-species/drug comparisons
    - Heatmaps and bar plots
5.  **Cross-model comparison**: Compare models across stratifications
    - Country-based models
    - Year-based models
    - Performance and feature consistency
6.  **Query data**: Custom data queries
    - Filter by multiple criteria
    - Export filtered results

### Example: Exploring model performance

``` r
launch_dashboard()

# In the dashboard:
# 1. Navigate to "Model Performance" tab
# 2. Select species: "Campylobacter jejuni"
# 3. Select drug: "ciprofloxacin"
# 4. Select molecular scale: "genes"
# 5. View performance metrics and confusion matrix
```

### Data requirements

The dashboard works with pre-computed data files located in
`inst/app/data/`:

- `amr_filtered_tbls.db`: DuckDB database with AMR data
- `all_performances.tsv`: Model performance metrics
- `drug_class_map.tsv`: Drug classification mapping
- `metadata/`: Species-specific metadata files

To use your own data, structure files following the same schema.

## Data Schema

### Performance metrics

Required columns: - `bug`: Species code - `antibiotic`: Drug name -
`scale`: Molecular scale (gene, protein, domain, struct) - `type`: Count
or binary features - `bal_acc`: Balanced accuracy - `f1`: F1 score -
`nmcc`: Normalized Matthews correlation coefficient - Additional columns
for other metrics

### Metadata files

Location: `inst/app/data/metadata/{species}.parquet`

Required columns: - `genome_id`: Unique genome identifier -
`genome.isolation_country`: Country of isolation -
`genome.collection_year`: Collection year - `genome.host_name`: Host
organism - Additional metadata columns as needed

## Development

### Package structure

    amR_shiny/
    ├── R/
    │   └── launch_dashboard.R    # Main launch function
    ├── inst/
    │   └── app/
    │       ├── app.R             # Main Shiny app
    │       ├── utils.R           # Utility functions
    │       ├── modules/          # UI modules
    │       ├── data/             # Dashboard data files
    │       └── www/              # Static assets (CSS, images)
    ├── man/                      # Documentation
    └── DESCRIPTION

## Citation

If you use `amR_shiny` in your research, please cite:

    Boyer E, Lesiyon R, Mayer D, Brenner E, Ghosh A, Vang C, Ravi J. (2025).
    amR_shiny: Interactive dashboard for AMR data and model visualization.
    R package version 0.99.0.
    https://github.com/JRaviLab/amR_shiny

## For Bioconductor submission

This package is being prepared for Bioconductor submission. It includes:

- **biocViews**: GUI, MicrobialGenomics, Pathogen, Visualization
- **Bioconductor dependencies**: ComplexHeatmap
- **R version requirement**: R \>= 4.1.0
- **Documentation**: Comprehensive function documentation with examples
- **Data**: Pre-computed AMR model results included in `inst/app/data/`

## Contributing

We welcome contributions! Please see our [Contributing
Guidelines](CONTRIBUTING.md) for details.

### Reporting Issues

Report bugs and request features at:
<https://github.com/JRaviLab/amR_shiny/issues>

## Related projects

- [amR_data](https://github.com/JRaviLab/amR_data): Data preparation
  pipeline
- [amR_ml](https://github.com/JRaviLab/amR_ml): ML modeling framework
- [BV-BRC](https://www.bv-brc.org/): Bacterial and Viral Bioinformatics
  Resource Center

## Code of conduct

Please note that the amR_shiny project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

## License

BSD 3-Clause License. See [LICENSE](LICENSE) for details.

## Contact

**Corresponding author**: Janani Ravi (<janani.ravi@cuanschutz.edu>)

**JRaviLab**: <https://jravilab.github.io>
