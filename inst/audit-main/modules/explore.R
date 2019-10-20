library(mtpview1)
library(dplyr)
library(tidyr)
library(shiny)
library(mtpview1)

# UI ----------------------------------------------------------------------

exploreContentsUI <- function(id) {
    ns <- NS(id)

    expt_glance_box <- box(
        selectInput(ns("metricSelector"), "metric to fill wells:",
                    choices = c(''),
                    selected = ''),
        uiOutput(ns("metricSelectorUI")),
        plotOutput(ns("experimentGlance")),
        # tableOutput(ns("summarytbl")),

        width = 12,
        title = tagList(span(icon("eye"), style = 'opacity:0.3;'), span("Experiment at a glance"))
        )

    grouping_box <- box(width = 12,
        title = tagList(span(icon("object-group"), style = 'opacity:0.3;'), span("Grouping")),
        textAreaInput(ns('groups'), 'Add Groups (one per row)'),
        fluidRow(
            column(6,
                   numericInput(ns('nrow'), 'rows (n) of grouping plate?', value = 8, min = 1, max = 48, step = 1)
                   ),
            column(6,
                   numericInput(ns('ncol'), 'col (n) of grouping plate?', value = 12, min = 1, max = 48, step = 1)
                   ))
        ,

    )

    tagList(expt_glance_box, grouping_box)

}


# SERVER ------------------------------------------------------------------

exploreContents <- function(input, output, session, summary) {

    modelData <- reactive({
        d <- summary()
        d <- dplyr::select(d, -components, -observations)
        d <- tidyr::unnest(d, cols = c(model))
    })

    componentsData <- reactive({
        d <- summary()
        d <- dplyr::select(d, -model, -observations)
        d <- tidyr::unnest(d, cols = c(components))
    })

    # observationsData <- reactive({
    #     d <- summary()
    #     d <- dplyr::select(d, -model, -components)
    #     d <- tidyr::unnest(d, cols = c(observations))
    # })

    # output$summarytbl <- renderTable({
    #     head(componentsData())
    # })

    # output$summarynames <- renderText({
    #     names(summary())
    # })

    output$metricSelectorUI <- renderUI({
        metrics <- base::setdiff(names(componentsData()), c("run", "plate", "well"))
        updateSelectInput(session, "metricSelector",
                          choices = metrics,
                          selected = metrics[1])
        shiny::br()
    })

    output$experimentGlance <- renderPlot({
        validate(
            need(input$metricSelector != '', 'Choose a metric to display')
        )

        d <- componentsData()
        d <- dplyr::ungroup(d)
        d <- tidyr::unite(d, run_plate, run, plate, sep = ':')
        n <- NROW(dplyr::distinct(d, well))

        mtpview1::mtp_ggplot(d,aes(plate = run_plate, well = well)) +
            mtp_spec_impl(n) +
            mtpview1::geom_footprint() +
            # mtpview1::geom_notched_border() +
            mtpview1::geom_col_label() +
            mtpview1::geom_row_label() +
            mtpview1::geom_well_rect(aes_string(fill = input$metricSelector)) +
            facet_wrap(~run_plate) +
            scale_fill_viridis_c()
    })

    #' grouping related
    groups <- reactive({
        unlist(stringr::str_split(input$groups, '\n'))
    })
}


# module helpers ----------------------------------------------------------

mtp_spec_impl <- function(n) {
    f <- switch(as.character(n),
                '24' = mtpview1::mtp_spec_24well,
                '48' = mtpview1::mtp_spec_48well,
                '96' = mtpview1::mtp_spec_96well,
                '100' = mtpview1:::mtp_spec_bioscreen,
                '384' = mtpview1::mtp_spec_384well,
                '1536' = mtpview1::mtp_spec_1536well)
    return(f())
}