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
        mtpview1::mtp_ggplot(summary(),aes(plate = plate, well = well)) +
            mtpview1::mtp_spec_96well() +
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
