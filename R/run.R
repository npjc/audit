#' @export
run <- function() {
    appDir <- system.file("audit-main", package = "audit", mustWork = TRUE)
    shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}