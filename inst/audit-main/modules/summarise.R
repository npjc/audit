library(growr)

# UI ----------------------------------------------------------------------

summariseTabContentsUI <- function(id) {
    ns <- NS(id)

    input_box <- box(
        title = tagList(span(icon("filter"), style = 'opacity:0.3;'), span("Summarise")),
        fluidRow(
            column(6,
                   estimationMethodInput(ns('estimationMethod'))
            )
        ),
        fluidRow(
            column(12, formulaInput(ns('formula')))
        ),
        fluidRow(
            column(12, startValuesInput(ns('startValues')))
        ),
        actionButton(ns('summariseGo'), label = 'Summarise Experiment')
    )

    tbl_box <- box(
        title = tagList(shiny::icon("table"), "Wells"),
        width = 6,
        DT::dataTableOutput(ns('table'))
    )

    preview_box <- box(
        title = tagList(shiny::icon("chart-area"), "Preview"),
        width = 12,
        plotOutput(ns("preview")),
        br(),
        h4("Metrics:"),
        tableOutput(ns("previewComponentsTable")),
        br(),
        h4("Fit Stats:"),
        tableOutput(ns("previewModelTable"))
    )


    # grouping_box <- box(
    #     title = tagList(span(icon("object-group"), style = 'opacity:0.3;'), span("Grouping")),
    #     textAreaInput(ns('groups'), 'Add Groups (one per row)'),
    #     numericInput(ns('nrow'), 'rows (n) of grouping plate?', value = 8, min = 1, max = 48, step = 1),
    #     numericInput(ns('ncol'), 'col (n) of grouping plate?', value = 12, min = 1, max = 48, step = 1)
    # )

    # tagList(useShinyalert(), input_box, tbl_box, preview_box,grouping_box)

    fluidRow(
        tagList(useShinyalert(), input_box, tbl_box, preview_box)
    )

}


# SERVER ------------------------------------------------------------------

summariseTabContents <- function(input, output, session, inputData) {
    outputData <- reactiveVal(value = NULL)

    observeEvent(input$estimationMethodInfo, estimationMethodInfoAlert())
    observeEvent(input$formulaInfo, formulaInfoAlert())
    observeEvent(input$startValuesInfo, startValuesInfoAlert())
    observeEvent(input$summariseGo, {
        # message('summarise button clicked')

        d <- dplyr::group_by(inputData(), run, plate, well)

        if (input$estimationMethod == 'manual') {
            param_formula <- NULL
            param_start <- NULL
            if (input$formula != '') {
                param_formula <- rlang::eval_tidy(rlang::parse_expr(input$formula))
            }
            if (input$startValues != '') {
                param_start <- rlang::eval_tidy(rlang::parse_expr(sprintf("list(%s)", input$startValues)))
            }
            out <- summarise_fit_switch(d,
                                 method = input$estimationMethod,
                                 formula = param_formula,
                                 start = param_start
            )
        }else {
            out <- summarise_fit_switch(d, method = input$estimationMethod)
        }

        outputData(out)
        shinyalert(text = 'summarised experiment.',type = 'success')
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    distinctWells <- reactive({
        validate(
            need(!is.null(inputData()), "start by uploading data in the input tab")
        )

        dplyr::distinct(inputData(), run, plate, well)
    })

    output$table <- DT::renderDataTable({
        distinctWells()
    }, options = list(pageLength = 5, dom = "tp"), rownames = FALSE)


    dataSubset <- reactive({
        validate(
            need(input$table_rows_selected, "select one or more wells to preview from the 'Wells' table.")
        )
        filter <- distinctWells()[input$table_rows_selected,]
        subset <- dplyr::semi_join(inputData(), filter, by = c("run", "plate", "well"))
        dplyr::group_by(subset, run, plate, well)
    })


    summarisedDataSubset <- reactive({
        d <- dplyr::group_by(dataSubset(), run, plate, well)

        if (input$estimationMethod == 'manual') {
            param_formula <- NULL
            param_start <- NULL
            if (input$formula != '') {
                param_formula <- rlang::eval_tidy(rlang::parse_expr(input$formula))
            }
            if (input$startValues != '') {
                param_start <- rlang::eval_tidy(rlang::parse_expr(sprintf("list(%s)", input$startValues)))
            }
            summarise_fit_switch(d,
                                 method = input$estimationMethod,
                                 formula = param_formula,
                                 start = param_start
            )
        }else {
            summarise_fit_switch(d, method = input$estimationMethod)
        }
    })

    output$preview <- renderPlot({
        fitted_plot(summarisedDataSubset())
    })

    # outputData <- reactive({
    #     if (groups() == "")
    #         return(summarisedData())
    #     growr:::add_groupings(summarisedData(), groups(), input$nrow, input$ncol)
    # })


    # groups <- reactive({
    #     unlist(stringr::str_split(input$groups, '\n'))
    # })


    output$previewComponentsTable <- renderTable({
        # validate(need(outputData(),"summarise experiment for tabular preview"))
        d <- dplyr::select(summarisedDataSubset(), -model, -observations)
        d <- tidyr::unnest(d, cols = c(components))
        head(d)
    })

    output$previewModelTable <- renderTable({
        d <- dplyr::select(summarisedDataSubset(), -components, -observations)
        d <- tidyr::unnest(d, cols = c(model))
        head(d)
    })

    return(outputData) # format: tibble(model = tibble(), components = tibble(), observations = tibble())
}


# module helpers ----------------------------------------------------------

summarise_fit_switch <- function(d, method = "smooth.spline", ...) {
    f <- switch(method,
                "smooth.spline" = growr::fit_smooth_spline,
                "gompertz" = growr::fit_nls_gompertz,
                "richards" = growr::fit_nls_richards,
                "logistic" = growr::fit_nls_logistic,
                "manual" = growr::fit_nls
    )

    errlist = list(model = tibble(fail = TRUE),
                   components = tibble(fail = TRUE),
                   observations = tibble(fail = TRUE))


    safe_f <- possibly(f, otherwise = errlist, quiet = TRUE)

    d <- dplyr::summarise(d, fit = list(safe_f(runtime, measure, ...)))
    d <- dplyr::ungroup(d)
    # d <- tidyr::hoist(d, fit, components = "components")
    # d <- dplyr::select(d, -fit)
    # d <- tidyr::unnest_wider(d, components)
    d <- tidyr::unnest_wider(d, fit)
    d
}

get_components <- function(d) {
    d <- dplyr::select(d, -model, -observations)
    tidyr::unnest(d, cols = c(components))
}



# derivedInput <- function(inputId) {
#     choices <- c("Lag Phase Duration" = "lambda",
#                  "Maximum Growth Rate" = "mu",
#                  "Maximum Density Reached" = "A",
#                  "Efficiency of Growth" = "integral",
#                  "Time to 5 Doublings" = "AvgG")
#     checkboxGroupInput(
#         inputId = inputId,
#         label = "Choose derived values",
#         choices = choices,
#         selected = choices)
# }

estimationMethodInput <- function(inputId) {
    choices <- c("smooth.spline", "gompertz", "richards", "logistic", "manual")

    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Estimation Method')
    selectInput(inputId,
                label = lbl,
                choices = choices,
                multiple = FALSE,
                selected = 'no')
}

# styled action link for use in input label
action_link_lbl <- function(inputId, label) {
    labelList <- tagList(
        span(label, style = 'text-decoration: none;opacity: 1;color: black;'),
        span(icon("info-circle"), style = "opacity:0.8;font-size: 10pt;")
    )

    actionLink(inputId, label = labelList)
}


estimationMethodInfoAlert <- function() {
    h <- shiny::includeMarkdown(file.path('docs','estimationMethodInfo.md'))

    InfoAlert("Estimation Method", h)
}

formulaInfoAlert <- function() {
    h <- shiny::includeMarkdown(file.path('docs','estimationMethodInfo.md'))

    InfoAlert("Formula", h)
}

startValuesInfoAlert <- function() {
    h <- shiny::includeMarkdown(file.path('docs','estimationMethodInfo.md'))

    InfoAlert("Start Values", h)
}

InfoAlert <- function(title, htmltext) {

    shinyalert(title = title,
               text = htmltext,
               type = "info",
               closeOnClickOutside = TRUE,
               showConfirmButton = FALSE,
               html = TRUE)
}

formulaInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Formula')
    textInput(inputId, label = lbl, placeholder = 'y ~ m * x + b (manual estimation)')
}

startValuesInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Start Values')
    textInput(inputId, label = lbl, placeholder = 'm = 0.5, b = 0 (manual estimation)')
}

fitted_plot <- function(x) {
    plot_data <- dplyr::select(x, -model, -components)
    plot_data <- tidyr::unnest(plot_data, cols = c(observations))
    plot_data <-  dplyr::mutate(plot_data,
                                ymin = if_else(y >= .fitted, .fitted, y),
                                ymax = if_else(y >= .fitted, y, .fitted)
    )

    ggplot(plot_data, aes(x = x)) +
        geom_point(aes(y = y), size = 0.5) +
        geom_line(aes(y = .fitted), size = 0.5) +
        # geom_linerange(aes(ymin = ymin, ymax = ymax), col = 'red', alpha = 0.5)
        geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = 'red', alpha = 0.25) +
        theme_bw() +
        facet_wrap(~interaction(run, plate, well))

}