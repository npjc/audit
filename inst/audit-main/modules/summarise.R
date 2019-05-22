# UI ----------------------------------------------------------------------

summariseTabContentsUI <- function(id) {
    ns <- NS(id)

    fluidRow(
        box(title = tagList(span(icon("filter"), style = 'opacity:0.3;'), span("Summarise")),
        DT::dataTableOutput(outputId = ns("table"))),
        box(
            title = tagList(span(icon("object-group"), style = 'opacity:0.3;'), span("Grouping")),
            textAreaInput(ns('groups'), 'Add Groups (one per row)')
            ),
        box(
            tableOutput(ns('parsed_groupings'))
        )
    )
    
}


# SERVER ------------------------------------------------------------------

summariseTabContents <- function(input, output, session, inputData) {
    
    groups <- reactive({
        unlist(stringr::str_split(req(input$groups), '\n'))
    })
    
    output$parsed_groupings <- renderTable({
        
        l <- growr:::parse_groupings(groups(), 8, 12)
        d <- purrr::map_df(l, tibble::as_tibble)
        d
    })
    
    output$table <- DT::renderDataTable({
        inputData()
    }, options = list(pageLength = 5, dom = "tp"), rownames = FALSE)
    
    outputData <- reactive({
        growr:::add_groupings(inputData(), groups(), 8, 12)
    })
    
    return(outputData)
}


# module helpers ----------------------------------------------------------