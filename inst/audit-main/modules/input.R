library(plater)
library(readgp1)
library(readcg12)
library(readyg)
library(readbioscreen)
library(readr)


# UI ----------------------------------------------------------------------

autoFileInput <- function(id) {
    ns <- NS(id)
    tagList(
        filesInput(ns("files")),
        shiny::tags$div(style = 'height: 230px; overflow-y: scroll', uiOutput(ns("messages")))
        )
}


# SERVER ------------------------------------------------------------------

autoFile <- function(input, output, session) {
    options(shiny.maxRequestSize = 30*1024^2) # max upload to 30mb

    observeEvent(input$filesInfo, filesInfoAlert())

    userFiles <- reactive({
        validate(need(input$files, message = 'start by uploading file(s)')) # If no files do nothing
        d <- input$files
        d <- add_identities(d)
        add_experiment_key(d)
    })

    datafile <- reactive({
        # only files for which we guessed the format?
        d <- userFiles()
        by_run <- dplyr::group_split(d, run)
        by_run <- purrr::map(by_run, join_run)
        out <- dplyr::bind_rows(by_run)
        out
    })

    output$messages <- renderUI({
        d <- dplyr::select(userFiles(), name, size, format, class, run)
        tagList(purrr::pmap(d, upload_message))
    })

    return(datafile)
}


# module helpers ----------------------------------------------------------


# just alternative parametrisation
add_experiment_key <- function(d) {
    files <- d$name
    classes <- d$class
    is_design <- classes == "design"
    mf <- files[!is_design]
    df <- files[is_design]
    ekey <- key_experiment(measures_files = mf, design_files = df)

    dplyr::left_join(d, ekey, by = c("name", "class"))
}

#' determine the keys for each run in an experiment
#'
#' @param measures_files list of files containing run measures
#' @param design_files list of files containing run designs
#' @param longer pivot output into longer form?
#'
#' @return a [tibble::tibble()] with `run`, `measures_file`, and `design_file`
#' @export
#'
#' @examples
#' # test cases highlighting intended behaviour:
#' key_experiment(NULL, NULL)
#' key_experiment("file1.txt", NULL)
#' key_experiment(sprintf("file%s.txt", 1:3), NULL)
#' key_experiment( NULL, "lonely_design.txt")
#' key_experiment( NULL, sprintf("lonely%s_design.txt", 1:3))
#' key_experiment(sprintf("file%s.txt", 1:3), sprintf("file%s_design.txt", 1:3))
#' key_experiment(sprintf("file%s.txt", 1:3), c(sprintf("file%s_design.txt", 1:3), "extra_design.txt"))
#' key_experiment(c(sprintf("file%s.txt", 1:3), "extra-run.txt"), sprintf("file%s_design.txt", 1:3))
#' key_experiment("one-run.txt", "with-one_design.txt")
#' key_experiment(sprintf("file%s.txt", 1:3), "common_design.txt")
#' key_experiment("common-run.txt", sprintf("design%s_design.txt", 1:3))
#' key_experiment(sprintf("run-%s.txt", 1:10), sprintf("design-%s", 11:20))
key_experiment <- function(measures_files = NULL, design_files = NULL, longer = TRUE) {

    lm <- length(measures_files)
    ld <- length(design_files)

    if (lm == 0 & ld == 0) {
        return(tibble::tibble())
    }
    if (lm >= 1 & ld == 0) {
        out <- list(as_key(measures_files), measures_files, NA_character_)
    }
    if (lm == 0 & ld >= 1) {
        out <- list(as_key(design_files), NA_character_, design_files)
    }
    if (lm >= 1 & ld == 1) {
        out <- list(as_key(measures_files), measures_files, design_files)
    }
    if (lm == 1 & ld > 1) {
        out <- list(as_key(design_files), measures_files, design_files)
    }
    if (lm > 1 & ld > 1) {
        mkeys <- as_key(measures_files)
        dkeys <- as_key(design_files)

        measures_tbl <- tibble::tibble(k = mkeys, m = measures_files)
        designs_tbl <- tibble::tibble(k = dkeys, d = design_files)

        out <- dplyr::full_join(measures_tbl, designs_tbl, by = "k")
    }
    d <- tibble::tibble(
        run = out[[1]],
        measures = out[[2]],
        design = out[[3]]
    )

    if (longer) {
        d <- tidyr::pivot_longer(d, -run, names_to = "class", values_to = "name")
    }
    d
}


as_key <- function(file) {
    stringr::str_remove(basename(file), "(_design)?\\.[^\\.]+(\\.gz)?$")
}


#' files <- list.files("tests", full.names = T)
#' add_identities(tibble(datapath = files))
add_identities <- function(d, name = datapath) {
    name <- rlang::enquo(name)
    d <- dplyr::mutate(d, format = purrr::map_chr(!!name, guess_type))
    dplyr::left_join(d, file_types, by = 'format')
}

guess_type <- function(file) {
    line <- readr::read_lines(file, n_max = 1)
    patterns <- c(
        cg12 = 'Results of Photometric1',
        gp1 = 'Plate\\[\\d\\]',
        yg = '^\\s*\\[\\s*(.+?)\\s*]',
        bioscreen = '^([Tt]ime)|^([Ll]abel)|^([Ii]nfo)',
        plater = "^[^,]+,1",
        generic_csv = "^(run,)?(plate,)?(well)|^(run,)?(plate,)(well)?"
    )
    index <- which(stringr::str_detect(line, patterns))
    if (length(index) != 1) {
        return(NA_character_)
    }
    names(patterns[index])
}

file_types <- tibble::tribble(
    ~format, ~class, ~read_pkg, ~read_fxn, ~read_fxn_opts,
    "cg12", "measures", "readcg12", "read_cg12", list(),
    "gp1", "measures", "readgp1", "read_gp1", list(),
    "yg", "measures", "readyg", "read_yg", list(),
    "bioscreen", "measures", "readbioscreen","read_bioscreen", list(),
    "plater", "design", "plater", "read_plate", list(well_ids_column = "well"),
    "generic_csv", "design", "readr", "read_csv", list()
)

join_run <- function(d) {
    check_run(d)
    if (nrow(d) == 1) {
        out <- do.call(d$read_fxn, c(file = d$datapath, d$read_fxn_opts[[1]]))
        out <- dplyr::mutate(out, run = unique(d$run))
        return(out)
    }

    dd <- dplyr::filter(d, class == "design")
    out_design <- do.call(dd$read_fxn, c(file = dd$datapath, dd$read_fxn_opts[[1]]))

    dm <- dplyr::filter(d, class == "measures")
    out_measures <- do.call(dm$read_fxn, c(file = dm$datapath, dm$read_fxn_opts[[1]]))

    by <- intersect(names(out_design), names(out_measures))
    joined <- dplyr::left_join(out_measures, out_design, by = by)
    dplyr::mutate(joined, run = unique(d$run))
}

check_run <- function(d) {
    n <- NROW(d)
    if (n > 2) {
        msg <- sprintf("cannot combine data for a run with more than 2 files: %s", d$run)
        stop(msg, call. = FALSE)
    }
    if (n == 0) {
        msg <- sprintf("cannot combine data for 0-file run: %s", d$run)
        stop(msg, call. = FALSE)
    }
}



format_message <- function(file, format) {
    symbol <- crayon::red(clisymbols::symbol$cross)
    file_name <- crayon::underline(file)
    outcome <- "could not be parsed, could not guess format..."
    template <- "{symbol} {file_name} {outcome}"
    if (!is.na(format)) {
        symbol <- crayon::green(clisymbols::symbol$tick)
        read_fxn <- crayon::bold(paste0(format, '()'))
        outcome <- glue::glue("was parsed via {read_fxn}.")
    }
    glue::glue(template)
}

upload_message <- function(name, size, format, class, run) {

    trunc_name <- stringr::str_trunc(name, width = 40, side = "center")
    was_guessed <- !is.na(format)

    if (size <= 1E5)
        size_nice <- paste(signif(size / 1E3, 3), "Kb")
    if (size > 1E5)
        size_nice <- paste(signif(size / 1E6, 3), "Mb")

    if (was_guessed) {
        s <- '
            <i class="fa fa-check text-success"></i>
            Parsed <strong>{class}</strong> file
            <span style="text-decoration: underline;">{trunc_name}</span>
            <span style = "opacity:0.5;">({format}, {size_nice})</span>
            and added to run: "{run}".
            <br>
            '
    } else {
        s <- '
            <i class="fa fa-times text-danger"></i>
            FAILED to parse
            <span style="text-decoration: underline;">{trunc_name}</span>.
            <br>
            '
    }
    HTML(glue::glue(s))
}




filesInput <- function(inputId) {
    lbl <- action_link_lbl(inputId = paste0(inputId, 'Info'),
                           label = 'Measures & Design Data')
    fileInput(inputId, label = lbl, multiple = TRUE)
}

# styled action link for use in input label
action_link_lbl <- function(inputId, label) {
    labelList <- tagList(
        span(label, style = 'text-decoration: none;opacity: 1;color: black;'),
        span('?', style = "opacity:0.4;font-weight: 200;")
    )

    actionLink(inputId, label = labelList)
}


filesInfoAlert <- function() {
    h <- shiny::includeMarkdown(file.path('docs','filesInfo.md'))

    InfoAlert("Measures & Design Data", h)
}

InfoAlert <- function(title, htmltext) {

    shinyalert(title = title,
               text = htmltext,
               type = "info",
               closeOnClickOutside = TRUE,
               showConfirmButton = FALSE,
               html = TRUE)
}