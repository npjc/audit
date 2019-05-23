library(growr)
library(shinyalert)

# UI ----------------------------------------------------------------------

preprocessTabContentsUI <- function(id, label = NULL) {
    ns <- NS(id)
    input_box <- shinydashboard::box(
        title = tagList(span(icon("broom"), style = 'opacity:0.3;'), span("Preprocess")),
        fluidRow(
            column(6, logInput(ns('log'))),
            column(6, runmedInput(ns('runmed')))
        ),
        fluidRow(
            column(6, runmavInput(ns('runmav'))),
            column(6, forceIncInput(ns('increasing')))
        ),
        fluidRow(
            column(6, bgSubtractInput(ns('background'))),
            column(6, calibrationInput(ns('calibration')))
        ),
        actionButton(ns('preprocessGo'), label = 'Preprocess Experiment'),
        uiOutput(ns('preprocessMessage'))
    )

    tbl_box <- box(
        title = tagList(shiny::icon("table"), "Wells"),
        width = 6,
        DT::dataTableOutput(ns('table'))
    )

    preview_box <- box(
        title = tagList(shiny::icon("chart-area"), "Preview"),
        width = 12,
        plotOutput(ns("preview"))
    )

    tagList(useShinyalert(), input_box, tbl_box, preview_box)

}


# SERVER ------------------------------------------------------------------

preprocessTabContents <- function(input, output, session, datafile) {
    outputData <- reactiveVal(value = NULL)

    observeEvent(input$logInfo, logInfoAlert())
    observeEvent(input$runmedInfo, runmedInfoAlert())
    observeEvent(input$runmavInfo, runmavInfoAlert())
    observeEvent(input$increasingInfo, increasingInfoAlert())
    observeEvent(input$backgroundInfo, backgroundInfoAlert())
    observeEvent(input$calibrationInfo, calibrationInfoAlert())
    observeEvent(input$preprocessGo, {
        message('preprocess button clicekd')
        out <- dplyr::group_by(datafile(), run, plate, well)
        out <- dplyr::mutate(out, measures_pp = preprocessFxn()(measure))
        outputData(out)
        shinyalert(text = 'applied preprocesssing to experiment.',type = 'success')
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    distinctWells <- reactive({
        dplyr::distinct(datafile(), run, plate, well)
    })

    output$table <- DT::renderDataTable({
        distinctWells()
    }, options = list(pageLength = 5, dom = "tp"), rownames = FALSE)

    output$selected <- renderText({
        input$table_rows_selected
    })

    dataSubset <- reactive({
        validate(need(input$table_rows_selected, message = "select one or more wells to preview from the 'Wells' table."))
        filter <- distinctWells()[input$table_rows_selected,]
        subset <- dplyr::semi_join(datafile(), filter)
        dplyr::group_by(subset, run, plate, well)
    })

    preprocessFxn <- reactive({

        log_base <- rlang::eval_tidy(rlang::parse_expr(input$log))
        runmed <- as.integer(input$runmed)
        runmav <- as.integer(input$runmav)
        increasing <- input$increasing
        background <- rlang::eval_tidy(rlang::parse_expr(paste0("function(y){",input$background, "}")))
        calibration <- rlang::eval_tidy(rlang::parse_expr(paste0("function(y){",input$calibration, "}")))

        function(y) {
            growr::preprocess(y,
                              log_base = log_base,
                              runmed_k = runmed,
                              runmav_n = runmav,
                              force_inc = increasing,
                              bg_subtract = background,
                              calibrate_fxn = calibration)
        }
    })

    output$preprocessMessage <- renderUI({
        preprocess_message(input$log, input$runmed, input$runmav, input$increasing, input$background, input$calibration)
    })

    output$preview <- renderPlot({

        subset <- dplyr::mutate(dataSubset(), measure_pp = preprocessFxn()(measure))
        p <- ggplot2::ggplot(subset, aes(group = interaction(plate, well)))
        p <- p + geom_line(aes(x = runtime, y = measure), alpha = 0.6)
        p <- p + geom_line(aes(x = runtime, y = measure_pp), lty = 'dashed', alpha = 0.6)
        p + theme_bw() + facet_wrap(~interaction(plate, well))
    })



    return(outputData)
}


# helpers -----------------------------------------------------------------

preprocess_message <- function(log_base,
                               runmed,
                               runmav,
                               increasing,
                               background,
                               calibration) {

    msg <- ''
    if (log_base != FALSE) {
        msg <- c(msg,
                 '<p><i class="fa fa-check text-success"></i> log-ratio transformation applied.</p>')
    }
    if (as.integer(runmed) > 1) {
        newitem <- paste0('<p><i class="fa fa-check text-success"></i> median filter <span style = "opacity:0.5;">(n = ',
               runmed,
               ')</span> applied.</p>')
        msg <- c(msg, newitem)
    }
    if (as.integer(runmav) > 1) {
        newitem <- paste0('<p><i class="fa fa-check text-success"></i> mean filter <span style = "opacity:0.5;">(n = ',
                          runmav,
                          ')</span> applied.</p>')
        msg <- c(msg,
                 newitem)
    }
    if (increasing) {
        newitem <- '<p><i class="fa fa-check text-success"></i> enforce increasing applied.</p>'
        msg <- c(msg, newitem)
    }
    if (background != 'y') {
        newitem <- paste0('<p><i class="fa fa-check text-success"></i> background subtraction <span style = "opacity:0.5;">(',
                          background,
                          ')</span> applied.</p>')
        msg <- c(msg, newitem)
    }
    if (calibration != 'y') {
        newitem <- paste0('<p><i class="fa fa-check text-success"></i> calibration <span style = "opacity:0.5;">(',
                          calibration,
                          ')</span> applied.</p>')
        msg <- c(msg, newitem)
    }

    msg <- paste0(msg, collapse = '\n')
    HTML(msg)
}



calibrationInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Calibration')
    textInput(inputId, label = lbl, value = 'y')
}

bgSubtractInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Background Subtraction')
    textInput(inputId, label = lbl,
              value = 'y - min(y)')
}

forceIncInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Enforce Increasing')
    selectInput(inputId, label = lbl, choices = c('no' = FALSE, 'yes' = TRUE), selected = FALSE)
}

runmavInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Running Mean Smooth')
    numericInput(inputId,
                 label = lbl,
                 min = 1, value = 1, step = 2)
}

runmedInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                               label = 'Running Median Smooth')
    numericInput(inputId,
                label = lbl,
                min = 1, value = 1, step = 2)
}

calibrationInfoAlert <- function() {
    h <- '<p>How Should calibration be performed?</p>
    <br/>
    <p>A calibration function is applied to the measures to correct for a non-linear relationship between the observed (measured) signal and the actual (true) signal. For example, to correct for underestimating the OD values due to multiple scattering at higher densities of cells.</p>
    <br/>
    <p>An arbitrary function can be provided to perform the calibration on the input measures (y):</p>
    <pre>y + 0.81 * y ^ 3</pre>
    <br/>
    <p>By default, no calibration is applied as the specified function simply returns the input measures (y):</p>
    <pre>y</pre>
    <br/>
<p>See ?growr::preprocess in R documentation for more info.</p>
    '

    InfoAlert("Calibration", h)
}

backgroundInfoAlert <- function() {
    h <- '<p>How should background subtraction be performed?</p>
    <br/>
    <p>Subtract background value(s) to ensure that the measured values are specific to the intended signal.  or a function can be applied if the background is expected to vary across wells.</p>
    <br/>
    <p>A fixed background value can be subtracted for example:</p>
    <pre>y - 0.081</pre>
    <br/>
    <p>A dynamic background subtraction, removing the minimum measure from each well. (The Default)</p>
    <pre>y - min(y)</pre>
    <br/>
    <p>A dynamic background subtraction, removing the mean of the first 5 values from each well.</p>
    <pre>y - mean(y[1:5])</pre>
    <br/>
    '

    InfoAlert("Background Subtraction", h)
}

increasingInfoAlert <- function() {
    h <- '<p>Enforce measures to be increasing?</p>
    <br/>
    <p>If this is applied each measure is tested to have an equal or larger magnitude than its previous value. If this is not the case then the current measure is replaced with the previous value against which it was compared.</p>
    <br/>
<p>See ?growr:::enforce_mono_inc in R documentation for more info.</p>
    '

    InfoAlert("Enforce Increasing", h)
}


runmavInfoAlert <- function() {
    h <- '<p>Measures can be smoothed with a running mean filter. Choose the size of the filter (n).</p>
    <br/>
<p>n is the number of data points that are used to calculate the mean that will replace the initial value. Larger n implies more smoothing.</p>
<br/>
<p>By default (n = 1), no smoothing is applied. This is because the mean of only one value is that value.</p>
<br/>
<p>See ?growr:::runmav in R documentation for more info.</p>
'



    InfoAlert("Running Mean Smoothing", h)
}

runmedInfoAlert <- function() {
    h <- '<p>Measures can be smoothed with a running median filter. Choose the size of the filter (n).</p>
    <br/>
<p>n is the number of data points that are used to calculate the median that will replace the initial value. Larger n implies more smoothing.</p>
<br/>
<p>By default (n = 1), no smoothing is applied. This is because the median of only one value is that value.</p>
<br/>
<p>See ?stats::runmed in R documentation for more info.</p>'


    InfoAlert("Running Median Smoothing", h)
}

logInfoAlert <- function() {
    h <- '<p>Raw measures can be log-ratio transformed. Chose the base of the logarithm to use.</p>
    <br/>
<p>Prior to taking the log, the measures (y) will be divided by its minimum. The operation is therefore:</p>
<br/>
<pre>log(y / min(y))</pre>
<br/>
<p>Some models assume that the y values are log-ratio transformed measures when computing summary metrics. For example, extracting the growth rate from an exponential fit or maximum growth rate from a gompertz fit.</p>'


    InfoAlert("Log Ratio Transformation", h)
}

InfoAlert <- function(title, htmltext) {

    shinyalert(title = title,
               text = htmltext,
               type = "info",
               closeOnClickOutside = TRUE,
               showConfirmButton = FALSE,
               html = TRUE)
}

logInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                               label = 'Log Transformation')
    selectInput(inputId,
                label = lbl,
                choices = c('no' = FALSE, 'ln'  = 'exp(1)', 'log2' = '2', 'log10' = '10'),
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

