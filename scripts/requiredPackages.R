requiredPackages <- c("shiny", "shinydashboard", "plotly", "jsonlite", "shinyjs", "pracma", "nloptr")
for (name in requiredPackages) {
  if (!require(name, character.only = TRUE)) {
    install.packages(name, repos="https://cran.rstudio.com")
  }
}
