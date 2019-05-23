library(growr)

# UI ----------------------------------------------------------------------

downloadObjUI <- function(id) {
    ns <- NS(id)
    downloadButton(ns("data_download"), label = "Download .csv")
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
}


# module helpers ----------------------------------------------------------
