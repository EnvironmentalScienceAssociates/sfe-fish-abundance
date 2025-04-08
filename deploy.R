options(repos = c(ESA = "https://environmentalscienceassociates.r-universe.dev",
                  CRAN = "https://cloud.r-project.org"))

rsconnect::deployApp(forceUpdate = TRUE)