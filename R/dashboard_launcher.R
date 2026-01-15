#' Launch the AMR Dashboard
#'
#' This function launches the AMR Shiny Dashboard from the 'inst/app/' directory
#' of the installed package.
#'
#' @export
launch_amr_dashboard <- function() {
  # Locate the app directory within the package
  app_dir <- system.file("app", package = "amRshiny")

  # If the app directory is not found, stop with an error message
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Shiny app directory not found. Please ensure the package is installed correctly.")
  }

  # Launch the Shiny dashboard
  shiny::runApp(app_dir)
}
