expect_running <- function(sleep){
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  x <- processx::process$new(
    "R", 
    c(
      "-e", 
      "pkgload::load_all(here::here());launchAMRDashboard()"
    )
  )
  Sys.sleep(sleep)
  expect_true(x$is_alive())
  x$kill()
} 
test_that(
  "app launches",{
    skip_if_not(interactive())
    expect_running(sleep = 5)
  }
)
