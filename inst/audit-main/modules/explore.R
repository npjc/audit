library(mtpview1)
library(tidyverse)

# UI ----------------------------------------------------------------------

exploreContentsUI <- function(id) {
    ns <- NS(id)

    box(
        selectInput(ns("metric"), "metric to fill wells:",
                    choices = c("A", "mu", "lambda"),
                    selected = "A"),
        plotOutput(ns("experimentGlance")),
        tableOutput(ns("summarytbl")),

        width = 12,
        title = tagList(span(icon("eye"), style = 'opacity:0.3;'), span("Experiment at a glance"))
        )

}


# SERVER ------------------------------------------------------------------

exploreContents <- function(input, output, session, summary) {

    output$summarytbl <- renderTable({
        head(summary())
    })


    output$experimentGlance <- renderPlot({
        d <- summary()
        n <- NROW(dplyr::distinct(dplyr::ungroup(d), well))

        mtpview1::mtp_ggplot(d,aes(plate = plate, well = well)) +
            mtp_spec_impl(n) +
            mtpview1::geom_footprint() +
            # mtpview1::geom_notched_border() +
            mtpview1::geom_col_label() +
            mtpview1::geom_row_label() +
            mtpview1::geom_well_rect(aes_string(fill = input$metric)) +
            facet_wrap(~interaction(run, plate)) +
            scale_fill_viridis_c()
    })
}


# module helpers ----------------------------------------------------------

mtp_spec_impl <- function(n) {
    f <- switch(as.character(n),
                '24' = mtpview1::mtp_spec_24well,
                '48' = mtpview1::mtp_spec_48well,
                '96' = mtpview1::mtp_spec_96well,
                '384' = mtpview1::mtp_spec_384well,
                '1536' = mtpview1::mtp_spec_1536well)
    return(f())
}
