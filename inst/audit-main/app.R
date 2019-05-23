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
                exploreContentsUI("explore")
        ),
        tabItem(tabName = "out",
                fluidRow(
                    box(
                        title = tagList(span(icon("filter"), style = 'opacity:0.3;'), span("Summarise")),
                        column(6,
                               helpText("Tidied and preprocessed measures data"),
                               downloadObjUI(id = "download1")
                               ),
                        column(6,
                               helpText("Summarised data (one row per well per group)"),
                               downloadObjUI(id = "download2")
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
    callModule(exploreContents, 'explore', summarised)
    callModule(downloadObj, id = "download1", data = preprocess, "measures-data")
    callModule(downloadObj, id = "download2", data = summarised, "summarised-data")

    output$preprocesstbl <- renderTable({
        head(preprocess())
    })


}

shinyApp(ui, server)
