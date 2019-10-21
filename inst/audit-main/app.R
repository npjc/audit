# 00 - setup --------------------------------------------------------------

## CRAN dependencies
library(shiny)
library(shinydashboard)
library(shinyalert)
library(ggplot2)
library(plater)

## remotes and local dependencies
library(readcg12)
library(readgp1)
library(readyg)
library(readbioscreen)
source(file.path("modules","input.R"))
source(file.path("modules","experiment-table.R"))
source(file.path("modules","quickview.R"))
source(file.path("modules","preprocess.R"))
source(file.path("modules","summarise.R"))
source(file.path("modules","explore.R"))
source(file.path("modules","output.R"))
library(mtpview1)
library(mtpview)
library(growr)

# 01 - UI -----------------------------------------------------------------

header <- dashboardHeader(title = 'AUDIT')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Input", tabName = "inp", icon = icon("upload")),
        menuItem("Preprocess", tabName = "pre", icon = icon("broom")),
        menuItem("Summarise", tabName = "sum", icon = icon("filter")),
        menuItem("Explore", tabName = "exp", icon = icon("eye"), badgeLabel = "experimental", badgeColor = "yellow"),
        menuItem("Output", tabName = "out", icon = icon("download"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "inp",
                fluidRow(
                    box(autoFileInput("datafile"),
                        title = tagList(span(icon("upload"), style = 'opacity:0.3;'), span("Input"))),
                    experimentBoxUI('experiment')
                ),
                fluidRow(
                    quickviewBoxUI('viewbox')
                )
        ),
        tabItem(tabName = "pre",
                fluidRow(
                    preprocessTabContentsUI('preprocess'),
                    tableOutput('preprocesstbl')
                    )
        ),
        tabItem(tabName = "sum",
                summariseTabContentsUI("summarised")
                ),
        tabItem(tabName = "exp",
                fluidRow(exploreContentsUI("explore"))
        ),
        tabItem(tabName = "out",
                fluidRow(
                    box(width = 12,
                        title = tagList(span(icon("download"), style = 'opacity:0.3;'), span("Output")),
                        column(4,
                               helpText("Tidied and preprocessed measures data"),
                               downloadObjUI(id = "download1"),
                            helpText("one row per measurement")
                               ),
                        column(4,
                               helpText("Summary Metrics data"),
                               downloadObjUI(id = "download2"),
                               helpText("one row per well (per plate, per run)")
                               ),
                        column(4,
                               helpText("Model Quality data (fit quality and params)"),
                               downloadObjUI(id = "download3"),
                               helpText("one row per well (per plate, per run)")
                        )

                    )
                )
            )
    )
)

ui <- dashboardPage(header, sidebar, body)


# 02 - Server -------------------------------------------------------------

server <- function(input, output, session) {

    datafile <- callModule(autoFile, "datafile")
    experiment <- callModule(experimentBox, "experiment", datafile)
    viewbox <- callModule(quickviewBox, "viewbox", experiment, datafile)
    preprocess <- callModule(preprocessTabContents, 'preprocess', datafile)

    summarised <- callModule(summariseTabContents, 'summarised', preprocess)

    model_data <- reactive({
        validate(
            need(!is.null(summarised()), 'Need preprocessed and summarised data.')
        )
        d <- summarised()
        d <- dplyr::select(d, -components, -observations)
        d <- tidyr::unnest(d, cols = c(model))
    })
    components_data <- reactive({
        validate(
            need(!is.null(summarised()), 'Need preprocessed and summarised data.')
        )
        d <- summarised()
        d <- dplyr::select(d, -model, -observations)
        d <- tidyr::unnest(d, cols = c(components))
    })
    # observations_data <- reactive({
    #     d <- summarised()
    #     d <- dplyr::select(d, -model, -components)
    #     d <- tidyr::unnest(d, cols = c(observations))
    # })

    callModule(exploreContents, 'explore', summarised)
    callModule(downloadObj, id = "download1", data = preprocess, "measures-data")
    callModule(downloadObj, id = "download2", data = components_data, "summary-metrics-data")
    callModule(downloadObj, id = "download3", data = model_data, "fit-quality-stats")

    output$preprocesstbl <- renderTable({
        head(preprocess())
    })


}

shinyApp(ui, server)
