# UI ----------------------------------------------------------------------

quickviewBoxUI <- function(id) {
    ns <- NS(id)
    preview_box <- box(
        title = tagList(shiny::icon("chart-area"), "Preview"),
        width = 12,
        plotOutput(ns("preview"))
    )
    # preview_box
    fluidRow(
    uiOutput(ns("preview"))
    )
}


# SERVER ------------------------------------------------------------------

quickviewBox <- function(input, output, session, experiment, datafile) {

    selectedData <- reactive({
        dplyr::semi_join(datafile(), experiment(), by = "run")
    })

    output$preview <- renderUI({
        d <- selectedData()
        # by_run <- dplyr::group_split(d, run)
        # l <- purrr::map(by_run, mtpview_auto_ui)
        # mutli_run_view(d)
        by_plate <- dplyr::group_split(d, run, plate)
        l <- purrr::map(by_plate, box_plate)
        tagList(l)
    })

    out <- reactive({

        experiment()
    })

    return(out)
}


# module helpers ----------------------------------------------------------

mtpview_auto_ui <- function(d) {
    run_name <- dplyr::pull(dplyr::distinct(d, run), run)
    title <- tagList(span(icon("eye"), style = 'opacity:0.3;'), span(run_name))

    tabBox(
        title = title, width = 12,
        tabPanel("plot",
                 renderPlot({
                     mtpview::mtp_plot(dplyr::rename(d, x = runtime, y = measure), mtpview:::vars(run, plate))
                 })
                 ),
        tabPanel("data",
                 renderTable(({head(d)}))
                 ),
        tabPanel('download',
                 downloadLink('downloadData', 'Download Joined Data as csv.'),
                 br(),
                 downloadLink('downloadPlot', 'Download Overview Plot as png.')
                 )
    )
}


mutli_run_view <- function(d) {
 d <- dplyr::rename(d, x = runtime, y = measure)
 box(
     title = tagList(shiny::icon("chart-area"), "Preview"),
     width = 12,
     height = "600px",
     renderPlot({
         mtp_plot(d, vars(run, plate))
     })
 )

}

box_plate <- function(d) {
    first_row <- d[1,]
    title <- sprintf("%s (%s)", first_row$run, first_row$plate)
    title <- tagList(span(icon("eye"), style = 'opacity:0.3;'), span(title))
    box(
        title = title,
        width  = 6,
        renderPlot({
            mtpview::mtp_auto(dplyr::rename(d, x = runtime, y = measure), draw = TRUE)
        })
    )
}
