# UI ----------------------------------------------------------------------

experimentBoxUI <- function(id) {
    ns <- NS(id)
    
    box(
        DT::dataTableOutput(outputId = ns("table")),
        title = tagList(span(icon("table"), style = 'opacity:0.3;'), span("Experiment Summary"))
    )
}


# SERVER ------------------------------------------------------------------

experimentBox <- function(input, output, session, datafile) {
    eSummary <- reactive({
        summary_counts(datafile())
    })
    
    output$table <- DT::renderDataTable({
        eSummary()
    }, options = list(pageLength = 5, dom = "tp"), rownames = FALSE)
    
    selected <- reactive({
        # b/c we are inside the module just use <outputId>_rows_selected
        d <- eSummary()
        # str(input$table_rows_selected)
        dplyr::filter(d, dplyr::row_number() %in% input$table_rows_selected)
        
    })
    
    return(selected)
}


# module helpers ----------------------------------------------------------

summary_counts <- function(d) {
    l <- dplyr::group_split(d, run)
    purrr::map_df(l, summary_counts_per_run)
}

summary_counts_per_run <- function(d) {
    counts <- dplyr::count(d, plate, well)
    n_measures <- as.character(glue::glue_collapse(unique(counts$n), sep = ", ", last = " or " ))
    counts <- dplyr::count(counts, plate)
    n_wells <- as.character(glue::glue_collapse(unique(counts$n), sep = ", ", last = " or "))
    n_plates <- NROW(counts)
    tibble::tibble(run = d$run[1], n_plates = n_plates, n_wells = n_wells, n_measures = n_measures)
}
