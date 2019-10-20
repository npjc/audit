library(growr)

# UI ----------------------------------------------------------------------

downloadObjUI <- function(id) {
    ns <- NS(id)

    tagList(downloadButton(ns("data_download"), label = "Download .csv"),
            br(),
            verbatimTextOutput(ns("glimpse"))
            )

}

downloadObj <- function(input, output, session, data, slug) {

    output$data_download <- downloadHandler(
        filename = function() {
            paste0(Sys.Date(), "_", slug, ".csv")
        },
        content = function(file) {
            readr::write_csv(data(), file)
        }
    )

    output$glimpse <- renderText({
        paste0(capture.output(dplyr::glimpse(data(), width = 40)), collapse = '\n')
    })
}


# module helpers ----------------------------------------------------------
