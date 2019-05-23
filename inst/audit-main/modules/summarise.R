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
        )
    )

    fluidRow(
        input_box,
        box(
            title = tagList(span(icon("object-group"), style = 'opacity:0.3;'), span("Grouping")),
            textAreaInput(ns('groups'), 'Add Groups (one per row)'),
            numericInput(ns('nrow'), 'rows (n) of grouping plate?', value = 8, min = 1, max = 48, step = 1),
            numericInput(ns('ncol'), 'col (n) of grouping plate?', value = 12, min = 1, max = 48, step = 1)
            ),
        box(
            tableOutput(ns('parsed_groupings'))
        )
    )

}


# SERVER ------------------------------------------------------------------

summariseTabContents <- function(input, output, session, inputData) {

    observeEvent(input$estimationMethodInfo, estimationMethodInfoAlert())

    groups <- reactive({
        unlist(stringr::str_split(req(input$groups), '\n'))
    })

    output$parsed_groupings <- renderTable({
        l <- growr:::parse_groupings(groups(), input$nrow, input$ncol)
        d <- purrr::map_df(l, tibble::as_tibble)
        d
    })

    output$table <- DT::renderDataTable({
        d <- dplyr::group_by(inputData(), run, plate, well)
        d <- dplyr::summarise(d,
                              metrics = list(growr::summarise_fit(runtime, measure_pp,
                                                   method = input$estimationMethod)))
        tidyr::unnest(d, cols(metrics))
    }, options = list(pageLength = 5, dom = "tp"), rownames = FALSE)

    outputData <- reactive({
        growr:::add_groupings(inputData(), groups(), input$nrow, input$ncol)
    })

    return(outputData)
}


# module helpers ----------------------------------------------------------

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
    choices <- c("smooth.spline", "gompertz", "richards", "logistic")

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
        span('?', style = "opacity:0.4;font-weight: 200;")
    )

    actionLink(inputId, label = labelList)
}


estimationMethodInfoAlert <- function() {
    h <- shiny::includeMarkdown(file.path('docs','estimationMethodInfo.md'))

    InfoAlert("Estimation Method", h)
}

InfoAlert <- function(title, htmltext) {

    shinyalert(title = title,
               text = htmltext,
               type = "info",
               closeOnClickOutside = TRUE,
               showConfirmButton = FALSE,
               html = TRUE)
}