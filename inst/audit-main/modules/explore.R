# library(mtpview1)
# library(dplyr)
# library(tidyr)
# library(shiny)
# library(mtpview1)

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
        uiOutput(ns("runplateSelectorUI")),
        column(4,
               selectInput(ns("runplateSelector"), "Plate to apply grouping:",
                           choices = c(''),
                           selected = '')),
        column(4,
               numericInput(ns('nrow'), 'rows (n) of grouping plate?', value = NA, min = 1, max = 48, step = 1)),
        column(4,
               numericInput(ns('ncol'), 'col (n) of grouping plate?', value = NA, min = 1, max = 48, step = 1)),
        textAreaInput(ns('groups'), 'Add Groups (one per row)'),
        br(),
        h4("Line Plots (per group):"),
        plotOutput(ns("observationsByGroupPlot")),
        br(),
        h4("Mean Metrics (per group):"),
        DT::dataTableOutput(ns("groupedDataPreview"))

    )

    tagList(expt_glance_box, grouping_box)

}


# SERVER ------------------------------------------------------------------

exploreContents <- function(input, output, session, summary) {

    modelData <- reactive({
        validate(
            need(!is.null(summary()), 'Need preprocessed and summarised data.')
        )
        d <- summary()
        d <- dplyr::select(d, -components, -observations)
        d <- tidyr::unnest(d, cols = c(model))
    })

    componentsData <- reactive({
        validate(
            need(!is.null(summary()), 'Need preprocessed and summarised data.')
        )
        d <- summary()
        d <- dplyr::select(d, -model, -observations)
        d <- tidyr::unnest(d, cols = c(components))
    })

    observationsData <- reactive({
        validate(
            need(!is.null(summary()), 'Need preprocessed and summarised data.')
        )
        d <- summary()
        d <- dplyr::select(d, -model, -components)
        d <- tidyr::unnest(d, cols = c(observations))
    })

    # output$summarytbl <- renderTable({
    #     head(componentsData())
    # })

    # output$summarynames <- renderText({
    #     names(summary())
    # })

    output$metricSelectorUI <- renderUI({
        metrics <- base::setdiff(names(componentsData()), c("run", "plate", "well"))
        updateSelectInput(session, "metricSelector",
                          choices = c('',metrics),
                          selected = '')
        shiny::br()
    })


    output$runplateSelectorUI <- renderUI({

        plates <- dplyr::distinct(componentsData(), run, plate)
        plates <- tidyr::unite(plates, run_plate, run, plate, sep = ":")
        plates <- dplyr::pull(plates, run_plate)
        # str(plates)

        updateSelectInput(session, "runplateSelector",
                          choices = c('', plates),
                          selected = '')
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


    currentPlateObservationsData <- reactive({
        validate(
            need(input$runplateSelector != '', "must specify a plate to apply well grouping to")
        )
        run_plate <- stringr::str_split_fixed(input$runplateSelector, ':', 2)

        dplyr::filter(observationsData(),
                      run == run_plate[1],
                      plate == run_plate[2])
    })

    currentPlateComponentsData <- reactive({
        validate(
            need(input$runplateSelector != '', "must specify a plate to apply well grouping to")
        )
        run_plate <- stringr::str_split_fixed(input$runplateSelector, ':', 2)

        dplyr::filter(componentsData(),
                      run == run_plate[1],
                      plate == run_plate[2])
    })

    #' grouping related
    groups <- reactive({
        unlist(stringr::str_split(input$groups, '\n'))
    })

    groupedComponentsData <- reactive({
        validate(
            need(groups() != "", 'must specify one or more groups.'),
            need(!is.na(input$nrow), 'must specify number of rows.'),
            need(!is.na(input$ncol), 'must specify  number of columns')
        )

        growr:::add_groupings(currentPlateComponentsData(), groups(), input$nrow, input$ncol)
    })

    groupedObservationsData <- reactive({
        validate(
            need(groups() != "", 'must specify one or more groups.'),
            need(!is.na(input$nrow), 'must specify number of rows.'),
            need(!is.na(input$ncol), 'must specify  number of columns')
        )

        growr:::add_groupings(currentPlateObservationsData(), groups(), input$nrow, input$ncol)
    })

    output$groupedDataPreview <- DT::renderDataTable({
        d <- dplyr::filter(groupedComponentsData(), !is.na(is_ref))
        d <- dplyr::group_by(d, group, is_ref)
        d <- dplyr::summarise_if(d, is.numeric, .funs = list(mean = mean))
        d <- dplyr::ungroup(d)
        d
    })

    output$observationsByGroupPlot <- renderPlot({
        observations <- groupedObservationsData()
        observations <- dplyr::filter(observations, !is.na(is_ref))
        aspect_ratio <- max(observations$x) / max(observations$.fitted)
        n_groups <- NROW(dplyr::distinct(observations, group))
        facet_nrow <- NULL
        if (n_groups < 9)
            facet_nrow <- 1

        ggplot(observations) +
            geom_line(aes(x = x, y = .fitted, group = well, alpha = is_ref, lty = is_ref),
                      size = 0.5) +
            coord_fixed(ratio = aspect_ratio) +
            facet_wrap(~group, nrow = facet_nrow) +
            theme_bw() +
            scale_linetype_manual(values = c("solid", "dashed")) +
            scale_alpha_manual(values = c(1, 0.3)) +
            labs(x = "Runtime", y = "Fitted Values (Measures)") +
            guides(lty = FALSE, alpha = FALSE)
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