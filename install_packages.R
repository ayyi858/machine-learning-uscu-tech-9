# Jalankan script ini SEKALI untuk install semua package
pkgs <- c(
  "shiny", "shinydashboard", "leaflet", "leaflet.extras",
  "dplyr", "ggplot2", "cluster", "dbscan", "DT", "plotly",
  "factoextra", "shinycssloaders", "shinyWidgets", "scales",
  "tidyr", "httr", "jsonlite"
)
need <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(need) > 0) {
  install.packages(need, repos = "https://cloud.r-project.org")
  cat("✅ Installed:", paste(need, collapse=", "), "\n")
} else {
  cat("✅ Semua package sudah tersedia!\n")
}
cat("\nCara menjalankan app:\n")
cat("  shiny::runApp('app.R')\n")
