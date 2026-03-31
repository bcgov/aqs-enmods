# app.R ----
# Dashboard for importer benchmark logs (template-style CSVs)

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(DT)
library(scales)
library(plotly)
library(tidyr)

# Null-coalescing operator (not in base R)
`%||%` <- function(a, b) if (!is.null(a)) a else b

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

readRenviron(paste0(getwd(), "./.Renviron"))

# ---- Configuration ----
url_background <- Sys.getenv("URL_Background")
url_import <- Sys.getenv("URL_Import")

# ---- Date stamp (YYYY-MM-DD) ----
download_date <- Sys.Date()

# ---- Output filenames ----
file_background <- sprintf(
  "7-days-prod-bcmoe-background-processor-stats-%s.csv",
  download_date
)

file_import <- sprintf(
  "7-days-prod-bcmoe-import-processor-stats-%s.csv",
  download_date
)

# ---- Download files ----
download.file(
  url = url_background,
  destfile = file_background,
  mode = "wb"
)

download.file(
  url = url_import,
  destfile = file_import,
  mode = "wb"
)

message("Downloads complete:")
message(" - ", file_background)
message(" - ", file_import)

# ---- 1) Data ingestion helpers ----

# --- File 1: User file submission metadata ---
required_cols_submissions <- c(
  "submission_id",
  "file_name",
  "original_file_name",
  "submission_date",
  "submitter_user_id",
  "submitter_agency_name",
  "submission_status_code",
  "sample_count",
  "results_count",
  "results_count_old"
)

# --- File 2: Importer performance ---
required_cols_performance <- c(
  "submission_id",
  "submission_date",
  "local_validation_time",
  "obs_validation_time",
  "local_import_time",
  "obs_import_time",
  "total_time"
)

# --- Files 3, 4: Background and import job count ---
required_cols_background_import <- c(
  "startTime",
  "jobCount"
)

read_csv_robust <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    guess_max = 5000
  )
}

read_submissions_csv <- function(path) {
  df <- read_csv_robust(path)

  missing <- setdiff(required_cols_submissions, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Submissions file '%s' is missing columns: %s",
      basename(path), paste(missing, collapse = ", ")
    ))
  }

  df %>%
    mutate(
      submission_date        = ymd_hms(submission_date, quiet = TRUE, tz = "UTC"),
      submission_status_code = as.character(submission_status_code),
      submitter_user_id      = as.character(submitter_user_id),
      submitter_agency_name  = as.character(submitter_agency_name),
      file_name              = as.character(file_name),
      original_file_name     = as.character(original_file_name),
      source_file            = basename(path)
    ) %>%
    mutate(across(
      c(sample_count, results_count, results_count_old),
      ~ suppressWarnings(as.numeric(.x))
    )) %>%
    dplyr::filter(submission_date >= as.POSIXct("2026-03-05", format = "%Y-%m-%d", tz = "America/Vancouver"))
}

read_performance_csv <- function(path) {
  df <- read_csv_robust(path)

  missing <- setdiff(required_cols_performance, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Performance file '%s' is missing columns: %s",
      basename(path), paste(missing, collapse = ", ")
    ))
  }

  df %>%
    mutate(
      submission_date = ymd_hms(submission_date, quiet = TRUE, tz = "UTC"),
      source_file     = basename(path)
    ) %>%
    mutate(across(
      c(
        local_validation_time, obs_validation_time,
        local_import_time, obs_import_time, total_time
      ),
      ~ suppressWarnings(as.numeric(.x))
    )) %>%
    dplyr::filter(submission_date >= as.POSIXct("2026-03-05", format = "%Y-%m-%d", tz = "America/Vancouver"))
}

read_background_import_csv <- function(path) {
  df <- read_csv_robust(path)

  missing <- setdiff(required_cols_background_import, names(df))
  if (length(missing) > 0) {
    stop(sprintf(
      "Background file '%s' is missing columns: %s",
      basename(path), paste(missing, collapse = ", ")
    ))
  }

  df %>%
    mutate(
      start_date = ymd_hms(startTime, quiet = TRUE, tz = "UTC"),
      job_count = as.numeric(jobCount),
      source_file = basename(path)
    ) %>%
    dplyr::select(-c(startTime, jobCount)) %>%
    mutate(across(
      c(job_count),
      ~ suppressWarnings(as.numeric(.x))
    )) %>%
    dplyr::filter(start_date >= as.POSIXct("2026-03-05", format = "%Y-%m-%d", tz = "America/Vancouver"))
}

# Combine many uploads of the same type
read_many <- function(paths, read_fn) {
  out <- lapply(paths, function(p) {
    tryCatch(read_fn(p), error = function(e) {
      warning(sprintf("Skipping '%s': %s", basename(p), e$message))
      NULL
    })
  })
  bind_rows(out) %>% unique()
}

# ---- 2) UI ----

ui <- page_sidebar(
  title = "EDT Benchmarking Dashboard",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    fileInput(
      "files_submissions",
      tags$span(
        tags$b("Upload submission metadata CSV"), tags$br(),
        tags$small(
          "Required: submission_id, file_name, original_file_name,",
          "submitter_user_id, submitter_agency_name, submission_status_code,",
          "submission_date, sample_count, results_count", "results_count_old"
        )
      ),
      multiple = TRUE,
      accept = c(".csv")
    ),
    fileInput(
      "files_performance",
      tags$span(
        tags$b("Upload importer performance CSV"), tags$br(),
        tags$small(
          "Required: submission_id, submission_date,",
          "local_validation_time, obs_validation_time,",
          "local_import_time, obs_import_time, total_time"
        )
      ),
      multiple = TRUE,
      accept = c(".csv")
    ),
    fileInput(
      "files_background",
      tags$span(
        tags$b("Upload AQS background metadata CSV"), tags$br(),
        tags$small(
          "Start time", "Job Count"
        )
      ),
      multiple = TRUE,
      accept = c(".csv")
    ),
    fileInput(
      "files_import",
      tags$span(
        tags$b("Upload AQS import metadata CSV"), tags$br(),
        tags$small(
          "Start time", "Job Count"
        )
      ),
      multiple = TRUE,
      accept = c(".csv")
    ),
    uiOutput("date_ui"),
    # selectizeInput("status", "Status", choices = c("SUBMITTED", "VALIDATED", "REJECTED"), selected = c("SUBMITTED", "VALIDATED", "REJECTED"), multiple = TRUE),
    # textInput("file_pattern", "File name contains (regex or text)", value = ""),
    checkboxInput("show_zeros", "Include zero times (0)", value = TRUE),
    checkboxInput("show_empty_files", "Include empty files", value = TRUE),
    hr(),
    tags$p(
      class = "text-muted small mb-1",
      "Downloads reflect the current date range and zero-time filter."
    ),
    downloadButton(
      "download_submissions",
      "Download submissions CSV",
      class = "btn btn-outline-primary btn-sm w-100 mb-2"
    ),
    downloadButton(
      "download_performance",
      "Download performance CSV",
      class = "btn btn-outline-primary btn-sm w-100 mb-2"
    ),
    downloadButton(
      "download_joined",
      "Download joined CSV",
      class = "btn btn-outline-primary btn-sm w-100 mb-2"
    )
  ),
  layout_columns(
    # Left column: KPIs on top, Time per 200 below
    layout_columns(
      col_widths = 12,
      card(
        card_header(tags$span(class = "fs-4", "KPIs")),
        card_body(
          tags$table(
            class = "table table-sm table-borderless mb-0 kpi-table",
            tags$tbody(
              tags$tr(
                tags$td("Missing times in the dataset"),
                tags$td(textOutput("missing_times", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Number of unique users"),
                tags$td(textOutput("users_total", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Number of unique organizations"),
                tags$td(textOutput("orgs_total", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Total file submissions to EDT"),
                tags$td(textOutput("files_total", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Validated (%)"),
                tags$td(textOutput("files_validated", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Submitted (%)"),
                tags$td(textOutput("files_submitted", container = tags$span), class = "kpi-val")
              ),
              # Rejected (%) hidden — subcategories below make it redundant
              # tags$tr(
              #   tags$td("Rejected (%)"),
              #   tags$td(textOutput("files_rejected", container = tags$span), class = "kpi-val")
              # ),
              tags$tr(
                tags$td("Rejected and Validated (%)"),
                tags$td(textOutput("files_rejected_then_validated", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Rejected and Submitted (%)"),
                tags$td(textOutput("files_rejected_then_submitted", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Rejected but Not Submitted (%)"),
                tags$td(textOutput("files_rejected_not_submitted", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Submitted: Total observation count"),
                tags$td(textOutput("kpi_submitted_total_results", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Submitted: Total samples count"),
                tags$td(textOutput("kpi_submitted_total_samples", container = tags$span), class = "kpi-val")
              ),
              # Submitted-specific rows
              tags$tr(
                tags$td("Submitted: Median observation count"),
                tags$td(textOutput("kpi_submitted_median_results", container = tags$span), class = "kpi-val")
              ),
              tags$tr(
                tags$td("Submitted: Time per 200 observations"),
                tags$td(textOutput("kpi_submitted_min_per_200", container = tags$span), class = "kpi-val")
              )
            )
          )
        )
      ),
      card(
        card_header(
          layout_columns(
            h4("Time per 200 observations"),
            selectizeInput(
              inputId = "time200_status",
              label = NULL,
              choices = c("REJECTED", "SUBMITTED", "VALIDATED"),
              selected = "SUBMITTED", # pick a sensible default,
              options = list(
                placeholder = "Select status…",
                allowEmptyOption = TRUE
              ),
              width = "100%"
            ),
            col_widths = c(8, 4)
          )
        ),
        plotOutput("plot_time_status", height = 300)
      )
    ),
    # Right column: Submission counts over time
    # card(
    #   card_header("KPIs"),
    #   card_body(
    #     tags$div(
    #       class = "kpi-table",
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Total submissions"),
    #         tags$div(class = "kpi-value", textOutput("kpi_total"))
    #       ),
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Validated (%)"),
    #         tags$div(class = "kpi-value", textOutput("kpi_validated"))
    #       ),
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Rejected (%)"),
    #         tags$div(class = "kpi-value", textOutput("kpi_rejected"))
    #       ),
    #
    #
    #       # --- Submitted-specific KPIs (two rows) ---
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Submitted: minutes per 200 observations"),
    #         tags$div(class = "kpi-value", textOutput("kpi_submitted_min_per_200"))
    #       ),
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Submitted: median results_count"),
    #         tags$div(class = "kpi-value", textOutput("kpi_submitted_median_results"))
    #       ),
    #       tags$div(
    #         class = "kpi-row",
    #         tags$div(class = "kpi-label", "Median total time"),
    #         tags$div(class = "kpi-value", textOutput("kpi_median_time"))
    #       )
    #     )
    #   )
    # ),
    # card(
    #   card_header("KPIs"),
    #   layout_columns(
    #     value_box("Total submissions", textOutput("kpi_total")),
    #     value_box("Validated (%)", textOutput("kpi_validated")),
    #     value_box("Rejected (%)", textOutput("kpi_rejected")),
    #     value_box("Median total time", textOutput("kpi_median_time"))
    #   )
    # ),
    card(
      card_header(
        tags$div(
          style = "display: flex; align-items: center; gap: 12px; flex-wrap: nowrap;",
          tags$span(class = "fs-4", style = "white-space: nowrap;", "Submission counts over time"),
          tags$div(style = "flex: 1; min-width: 160px;", uiOutput("submissions_status_ui")),
          tags$div(style = "flex: 1; min-width: 160px;", uiOutput("submissions_agency_ui"))
        )
      ),
      plotly::plotlyOutput("plot_submissions", height = 380)
    ),
    card(
      card_header(
        layout_columns(
          h4("Tracking AQS Sync/Speed"),
          selectizeInput(
            inputId = "aqstracking_choice",
            label = NULL,
            choices = c("Raw time series", "Event summary statistics – Background", "Event summary statistics – Import"),
            selected = "Raw time series", # pick a sensible default,
            options = list(
              placeholder = "Select plot type",
              allowEmptyOption = FALSE
            ),
            width = "100%"
          ),
          col_widths = c(8, 4)
        )
      ),
      card_body(
        plotly::plotlyOutput("plot_weekly_job_counts", height = 500),
        # ---- Time series plot: Background vs Import job counts ----
        #   tags$p(
        #     class = "text-muted small",
        #     "Time series showing background and import job counts over the past 7 days.",
        #     "Primary Y-axis (left): Background job count. Secondary Y-axis (right): Import job count.",
        #     "X-axis: Time period."
        #   )),
        # plotly::plotlyOutput("plot_weekly_job_counts", height = 500),
        tags$hr(),
        # # ---- Time series of daily speed of running benchmark file ----
        tags$p(
          class = "text-muted small",
          "Daily benchmark times to track speed of system.",
          "Each point is the time estimated for a benchmark file on all days with at least 3 SUBMITTED files",
          "X-axis = Day."
        ),
        plotly::plotlyOutput("plot_sync_benchmarks", height = 380)
      )
    ),
    # tags$div(
    #   style = "display: flex; align-items: center; gap: 12px; flex-wrap: nowrap;",
    #   tags$span(class = "fs-4", style = "white-space: nowrap;", "Tracking AQS Sync/Speed"),
    #   tags$div(style = "flex: 1; min-width: 160px;", uiOutput("submissions_status_ui")),
    # )
    #
    # card(
    #   card_header(tags$span(class = "fs-4", "Time per 200 observations (in min, by status)")),
    #   plotOutput("plot_time_status", height = 280)
    # ) # ,
    # card(
    #   card_header("Raw / filtered records"),
    #   DTOutput("tbl")
    # )
  )
)

# ---- 3) Server ----

server <- function(input, output, session) {
  # ---- High-res image modebar button (shared across all Plotly plots) ----
  # Icon: Font Awesome fa-search-plus (512×512)
  hires_btn <- list(
    name = "open_hires",
    title = "Open high-res image",
    icon = list(
      width = 512,
      height = 512,
      path = paste0(
        "M505 442.7L405.3 343c-4.5-4.5-10.6-7-17-7H372c27.6-35.3 44-79.7 44-128C416 93.1 322.9 0 208 0",
        "S0 93.1 0 208s93.1 208 208 208c48.3 0 92.7-16.4 128-44v16.3c0 6.4 2.5 12.5 7 17l99.7 99.7",
        "c9.4 9.4 24.6 9.4 33.9 0l28.3-28.3c9.4-9.4 9.4-24.6.1-34z",
        "M208 336c-70.7 0-128-57.2-128-128 0-70.7 57.2-128 128-128 70.7 0 128 57.2 128 128",
        " 0 70.7-57.2 128-128 128z",
        "m60-134c0 6.6-5.4 12-12 12h-36v36c0 6.6-5.4 12-12 12h-24c-6.6 0-12-5.4-12-12v-36",
        "h-36c-6.6 0-12-5.4-12-12v-24c0-6.6 5.4-12 12-12h36v-36c0-6.6 5.4-12 12-12h24",
        "c6.6 0 12 5.4 12 12v36h36c6.6 0 12 5.4 12 12v24z"
      )
    ),
    click = htmlwidgets::JS(
      "function(gd) {",
      "  Plotly.toImage(gd, {format: 'png', scale: 4}).then(function(dataUrl) {",
      "    Shiny.setInputValue('hires_img_src',",
      "      {src: dataUrl, ts: new Date().getTime()},",
      "      {priority: 'event'});",
      "  });",
      "}"
    )
  )

  # Modal that shows the high-res PNG generated by the button above
  observeEvent(input$hires_img_src, {
    src <- input$hires_img_src$src
    showModal(modalDialog(
      title = NULL,
      size = "xl",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        style = "text-align: center; padding: 4px;",
        tags$img(src = src, style = "max-width: 100%; height: auto; display: block; margin: auto;")
      )
    ))
  })

  # ---- Reactive data sources ----

  raw_submissions <- reactive({
    req(input$files_submissions)
    df <- read_many(input$files_submissions$datapath, read_submissions_csv)
    if (nrow(df) == 0) {
      stop("No valid rows loaded from submissions file. Check column names.")
    }
    df
  })

  raw_performance <- reactive({
    req(input$files_performance)
    df <- read_many(input$files_performance$datapath, read_performance_csv)
    if (nrow(df) == 0) {
      stop("No valid rows loaded from performance file. Check column names.")
      # req(nrow(df) > 0) # , "No valid rows loaded from performance file. Check column names."))
    }
    df
  })

  raw_background <- reactive({
    req(input$files_background)
    df <- read_many(input$files_background$datapath, read_background_import_csv)
    print(nrow(df))
    if (nrow(df) == 0) {
      stop("No valid rows loaded from background file. Check column names.")
    }
    df
  })

  raw_import <- reactive({
    req(input$files_import)
    df <- read_many(input$files_import$datapath, read_background_import_csv)
    print(nrow(df))
    if (nrow(df) == 0) {
      stop("No valid rows loaded from import file. Check column names.")
    }
    df
  })

  # Joined dataset — requires both files; joins on submission_id
  raw_data <- reactive({
    subs <- raw_submissions()
    perf <- raw_performance() %>%
      select(
        submission_id, local_validation_time, obs_validation_time,
        local_import_time, obs_import_time, total_time
      )

    df <- dplyr::inner_join(subs, perf, by = "submission_id")
    if (nrow(df) == 0) {
      # req(nrow(df) > 0) # , "
      stop("Join produced no rows. Ensure both files share submission_id values.")
    }
    df
  })

  # Date range UI after data is loaded
  output$date_ui <- renderUI({
    df <- raw_data()
    rng <- range(df$submission_date, na.rm = TRUE)
    dateRangeInput(
      "date_range",
      "Submission date range",
      start = as.Date(with_tz(rng[1], tz = "America/Vancouver")),
      end = as.Date(with_tz(rng[2], tz = "America/Vancouver"))
    )
  })

  # Dynamic dropdowns for the Submission counts over time card
  output$submissions_status_ui <- renderUI({
    df <- raw_data()
    choices <- c("All Statuses", sort(unique(df$submission_status_code)))
    tags$div(
      style = "margin-bottom: 0;",
      selectInput(
        inputId  = "submissions_status_filter",
        label    = NULL,
        choices  = choices,
        selected = "All Statuses",
        width    = "100%"
      )
    )
  })

  output$submissions_agency_ui <- renderUI({
    df <- raw_data()
    choices <- c("All Agencies", sort(unique(df$submitter_agency_name)))
    tags$div(
      style = "margin-bottom: 0;",
      selectInput(
        inputId  = "submissions_agency_filter",
        label    = NULL,
        choices  = choices,
        selected = "All Agencies",
        width    = "100%"
      )
    )
  })

  observeEvent(raw_data(),
    {
      df <- raw_data()
      choices <- sort(unique(df$submission_status_code))
      updateSelectizeInput(session, "status", choices = choices, selected = choices)
    },
    ignoreInit = TRUE
  )

  base_filtered <- reactive({
    df <- raw_data() %>% dplyr::filter(as.Date(with_tz(submission_date, tz = "America/Vancouver")) >= as.Date("2026-03-05"))

    # Date filter
    # Convert the user-selected Date boundaries to POSIXct in Vancouver time so
    # that comparisons stay in the same timezone throughout.  Comparing a plain
    # Date (no tz) against a POSIXct can cause R to coerce the Date using UTC
    # midnight, silently undoing the Vancouver-time conversion on submission_date.
    if (!is.null(input$date_range)) {
      start_van <- as.POSIXct(format(input$date_range[1], "%Y-%m-%d"), format = "%Y-%m-%d", tz = "America/Vancouver")
      end_van <- as.POSIXct(format(input$date_range[2], "%Y-%m-%d"), format = "%Y-%m-%d", tz = "America/Vancouver") + 86399
      df <- df %>%
        filter(
          with_tz(submission_date, tz = "America/Vancouver") >= start_van,
          with_tz(submission_date, tz = "America/Vancouver") <= end_van
        )
    }

    # File name pattern filter (search file_name + original_file_name)
    pat <- str_trim(input$file_pattern %||% "")
    if (nzchar(pat)) {
      df <- df %>%
        filter(str_detect(file_name, pat) | str_detect(original_file_name, pat))
    }

    # Zero-time handling
    if (!isTRUE(input$show_zeros)) {
      df <- df %>% filter(is.na(total_time) | total_time > 0)
    }

    # Zero-time handling
    if (!isTRUE(input$show_empty_files)) {
      df <- df %>% filter(submission_status_code == "SUBMITTED" & results_count > 0 | submission_status_code %in% c("REJECTED", "VALIDATED"))
    }

    # Status filter
    if (!is.null(input$status) && length(input$status) > 0) {
      df <- df %>% filter(submission_status_code %in% input$status)
    }

    df
  })

  # ---- KPIs ----
  output$missing_times <- renderText({
    n <- nrow(base_filtered() %>% filter(is.na(total_time)))
    if (n == 0) {
      return("No")
    }
    if (n > 0) stop("Missing times. Check tables.")
  })

  output$files_total <- renderText({
    nrow(base_filtered()) %>% comma()
  })

  output$users_total <- renderText({
    nrow(base_filtered() %>% dplyr::select(submitter_user_id) %>% unique()) %>% comma()
  })

  output$orgs_total <- renderText({
    nrow(base_filtered() %>% dplyr::select(submitter_agency_name) %>% unique()) %>% comma()
  })

  output$files_validated <- renderText({
    df <- base_filtered()
    if (nrow(df) == 0) {
      return("—")
    }
    pct <- mean(df$submission_status_code == "VALIDATED", na.rm = TRUE) * 100
    sprintf("%.1f%%", pct)
  })

  output$files_rejected <- renderText({
    df <- base_filtered()
    if (nrow(df) == 0) {
      return("—")
    }
    pct <- mean(df$submission_status_code == "REJECTED", na.rm = TRUE) * 100
    sprintf("%.1f%%", pct)
  })

  output$files_submitted <- renderText({
    df <- base_filtered()
    if (nrow(df) == 0) {
      return("—")
    }
    pct <- mean(df$submission_status_code == "SUBMITTED", na.rm = TRUE) * 100
    sprintf("%.1f%%", pct)
  })

  output$kpi_submitted_median_time <- renderText({
    df <- base_filtered() |> dplyr::filter(submission_status_code == "SUBMITTED")
    if (nrow(df) == 0) {
      return("—")
    }

    med_sec <- median(df$total_time, na.rm = TRUE)
    if (!is.finite(med_sec)) {
      return("—")
    }

    sprintf("%.1f min", med_sec / 60)
  })

  output$kpi_submitted_min_per_200 <- renderText({
    df <- base_filtered() %>%
      filter(submission_status_code == "SUBMITTED") %>%
      filter(!is.na(total_time), !is.na(results_count), results_count > 0)

    if (nrow(df) == 0) {
      return("—")
    }

    # Estimate: minutes per 200 results (results_count used as the scaling factor)
    min_per_200 <- median((df$total_time / df$results_count) * 200 / 60, na.rm = TRUE)

    if (!is.finite(min_per_200)) {
      return("—")
    }
    sprintf("%.2f min / 200 results", min_per_200)
  })

  output$kpi_submitted_median_results <- renderText({
    df <- base_filtered() %>% filter(submission_status_code == "SUBMITTED")
    if (nrow(df) == 0) {
      return("—")
    }

    med_n <- median(df$results_count, na.rm = TRUE)
    if (!is.finite(med_n)) {
      return("—")
    }

    format(round(med_n), big.mark = ",")
  })

  output$kpi_submitted_total_results <- renderText({
    df <- base_filtered() %>% filter(submission_status_code == "SUBMITTED")
    if (nrow(df) == 0) {
      return("—")
    }

    med_n <- sum(df$results_count, na.rm = TRUE)
    if (!is.finite(med_n)) {
      return("—")
    }

    format(round(med_n), big.mark = ",")
  })

  output$kpi_submitted_total_samples <- renderText({
    df <- base_filtered() %>% filter(submission_status_code == "SUBMITTED")
    if (nrow(df) == 0) {
      return("—")
    }

    med_n <- sum(df$sample_count, na.rm = TRUE)
    if (!is.finite(med_n)) {
      return("—")
    }

    format(round(med_n), big.mark = ",")
  })

  # ---- Rejected follow-up tracking ----
  # For every REJECTED submission, find the latest *other* submission by the same user
  # whose base file name (extension stripped) either exactly matches or contains
  # the REJECTED row's base name as a substring. Store that match's date and status.
  rejected_followup <- reactive({
    df <- base_filtered()
    if (nrow(df) == 0) {
      return(df)
    }

    # Strip the file-format extension (everything after the last dot)
    df <- df %>%
      dplyr::mutate(
        base_name = stringr::str_replace(original_file_name, "\\.[^.]+$", "")
      )

    # Work user by user
    users <- unique(df$submitter_user_id)

    results <- lapply(users, function(uid) {
      user_df <- df %>% dplyr::filter(submitter_user_id == uid)
      rejected_rows <- user_df %>% dplyr::filter(submission_status_code == "REJECTED")

      if (nrow(rejected_rows) == 0) {
        return(NULL)
      }

      # For each rejected row, scan all *other* rows for this user
      rejected_rows$latest_match_date <- as.POSIXct(NA, tz = "America/Vancouver")
      rejected_rows$latest_match_status <- NA_character_

      for (i in seq_len(nrow(rejected_rows))) {
        target_base <- rejected_rows$base_name[i]
        target_id <- rejected_rows$submission_id[i]

        # Candidates: all other rows for this user where the candidate base name
        # exactly equals OR fully contains the target base name as a substring
        candidates <- user_df %>%
          dplyr::filter(
            submission_id != target_id,
            stringr::str_detect(
              stringr::fixed(base_name), # search within candidate base_name
              stringr::fixed(target_base) # for the rejected row's base_name
            ) |
              base_name == target_base
          )

        if (nrow(candidates) == 0) next

        # Keep only the latest candidate
        best <- candidates %>%
          dplyr::arrange(dplyr::desc(submission_date)) %>%
          dplyr::slice(1)

        rejected_rows$latest_match_date[i] <- best$submission_date
        rejected_rows$latest_match_status[i] <- best$submission_status_code
      }

      rejected_rows
    })

    dplyr::bind_rows(results)
  })

  output$files_rejected_then_validated <- renderText({
    df_rej <- rejected_followup()
    if (is.null(df_rej) || nrow(df_rej) == 0) {
      return("—")
    }
    total_all <- nrow(base_filtered())
    if (total_all == 0) {
      return("—")
    }
    n_validated <- sum(df_rej$latest_match_status == "VALIDATED", na.rm = TRUE)
    sprintf("%.1f%%", n_validated / total_all * 100)
  })

  output$files_rejected_then_submitted <- renderText({
    df_rej <- rejected_followup()
    if (is.null(df_rej) || nrow(df_rej) == 0) {
      return("—")
    }
    total_all <- nrow(base_filtered())
    if (total_all == 0) {
      return("—")
    }
    n_submitted <- sum(df_rej$latest_match_status == "SUBMITTED", na.rm = TRUE)
    sprintf("%.1f%%", n_submitted / total_all * 100)
  })

  output$files_rejected_not_submitted <- renderText({
    df_rej <- rejected_followup()
    if (is.null(df_rej) || nrow(df_rej) == 0) {
      return("—")
    }
    total_all <- nrow(base_filtered())
    if (total_all == 0) {
      return("—")
    }
    # No match found at all, or the latest match is neither SUBMITTED nor VALIDATED
    n_no_sub <- sum(
      is.na(df_rej$latest_match_status) |
        !(df_rej$latest_match_status %in% c("SUBMITTED", "VALIDATED")),
      na.rm = TRUE
    )
    sprintf("%.1f%%", n_no_sub / total_all * 100)
  })

  # ---- Plots ----
  # output$plot_submissions <- renderPlot({
  #   df <- base_filtered()
  #   validate(need(nrow(df) > 0, "No data after filtering."))
  #
  #   # Ensure we have one bar per day (even if there are missing days)
  #   df_day <- df %>%
  #     mutate(day = as.Date(submission_date)) %>%
  #     count(day, submission_status_code, name = "n") %>%
  #     tidyr::complete(
  #       day = seq(min(day, na.rm = TRUE), max(day, na.rm = TRUE), by = "day"),
  #       submission_status_code,
  #       fill = list(n = 0)
  #     )
  #
  #   ggplot(df_day, aes(x = day, y = n, fill = submission_status_code)) +
  #     geom_col(width = 0.9) +
  #     labs(x = NULL, y = "Count", fill = "Status") +
  #
  #     # Daily ticks (one tick per day)
  #     scale_x_date(
  #       breaks = seq(min(df_day$day, na.rm = TRUE), max(df_day$day, na.rm = TRUE), by = "day"),
  #       date_labels = "%b %d"
  #     ) +
  #     coord_cartesian(ylim = c(0, 100)) +
  #     theme_minimal(base_size = 18) +
  #
  #     # Legend font + spacing improvements
  #     theme(
  #       legend.position = "top",
  #       legend.title = element_text(size = 16, margin = margin(b = 6)),
  #       legend.text = element_text(size = 14),
  #       legend.key.size = grid::unit(0.7, "cm"),
  #       legend.spacing.x = grid::unit(0.6, "cm"),
  #       legend.spacing.y = grid::unit(0.25, "cm"),
  #       legend.box.spacing = grid::unit(0.4, "cm"),
  #
  #       # Make daily tick labels readable (daily ticks get crowded fast)
  #       axis.text.x = element_text(angle = 45, hjust = 1, size = 16)
  #     ) +
  #     guides(
  #       fill = guide_legend(
  #         title.position = "top",
  #         title.hjust = 0.5,
  #         nrow = 1, # put legend items in one row
  #         byrow = TRUE
  #       )
  #     )
  #
  #   # ggplot(df_day, aes(x = day, y = n, fill = submission_status_code)) +
  #   #   geom_col() +
  #   #   labs(x = NULL, y = "Count", fill = "Status") +
  #   #   scale_x_date(date_labels = "%b %d") +
  #   #   theme_minimal(base_size = 18)
  # })

  output$plot_submissions <- plotly::renderPlotly({
    # Wait until both uiOutput-rendered inputs exist in the session.
    req(!is.null(input$submissions_status_filter))
    req(!is.null(input$submissions_agency_filter))

    df <- base_filtered()
    validate(need(nrow(df) > 0, "No data after filtering."))

    status_sel <- input$submissions_status_filter
    agency_sel <- input$submissions_agency_filter
    all_statuses <- (status_sel == "All Statuses")
    all_agencies <- (agency_sel == "All Agencies")

    # Apply filters
    if (!all_statuses) {
      df <- df %>% dplyr::filter(submission_status_code == status_sel)
    }
    if (!all_agencies) {
      df <- df %>% dplyr::filter(submitter_agency_name == agency_sel)
    }

    validate(need(nrow(df) > 0, "No data matches the selected filters."))

    # --- Legend visibility rules ---
    # Show agency legend when a specific status is selected (colour distinguishes agencies)
    # Show status legend when a specific agency is selected (alpha distinguishes statuses)
    show_agency_legend <- !all_statuses # specific status chosen → colour = agency
    show_status_legend <- !all_agencies # specific agency chosen → alpha = status

    # --- Classify agencies: those with <10 total submissions become "Others" ---
    # Use the full (unfiltered-by-agency) base data to compute prominence consistently
    agency_counts <- df %>%
      dplyr::count(submitter_agency_name, sort = TRUE)

    prominent_agencies <- agency_counts %>%
      dplyr::filter(n >= 10) %>%
      dplyr::pull(submitter_agency_name)

    df <- df %>%
      dplyr::mutate(
        agency_group = dplyr::if_else(
          submitter_agency_name %in% prominent_agencies,
          submitter_agency_name,
          "Others"
        )
      )

    # --- Daily counts by agency_group x status ---
    df_day <- df %>%
      dplyr::mutate(day = as.Date(with_tz(submission_date, tz = "America/Vancouver"))) %>%
      dplyr::count(day, agency_group, submission_status_code, name = "n") %>%
      tidyr::complete(
        day          = seq(min(day, na.rm = TRUE), max(day, na.rm = TRUE), by = "day"),
        agency_group = unique(df$agency_group),
        submission_status_code,
        fill         = list(n = 0)
      )

    # --- Colour palette for agencies (Others always grey) ---
    agency_levels <- c(sort(setdiff(unique(df$agency_group), "Others")), "Others")
    n_prominent <- length(agency_levels) - 1L

    palette_colors <- if (n_prominent > 0) {
      base_cols <- scales::hue_pal()(n_prominent)
      c(stats::setNames(base_cols, agency_levels[seq_len(n_prominent)]), Others = "#AAAAAA")
    } else {
      c(Others = "#AAAAAA")
    }

    # --- Alpha per status (encodes status visually) ---
    status_levels <- sort(unique(df_day$submission_status_code))
    n_status <- length(status_levels)
    alpha_vals <- seq(1.0, 0.35, length.out = max(n_status, 1))
    alpha_map <- stats::setNames(alpha_vals, status_levels)

    # --- Build plotly traces: one per agency x status combo ---
    fig <- plotly::plot_ly()
    shown_agencies <- character(0)
    shown_statuses <- character(0)

    for (ag in agency_levels) {
      for (st in status_levels) {
        sub <- df_day %>%
          dplyr::filter(agency_group == ag, submission_status_code == st)

        if (nrow(sub) == 0 || all(sub$n == 0)) next

        bar_color <- palette_colors[[ag]]
        bar_alpha <- alpha_map[[st]]
        is_first_for_ag <- !(ag %in% shown_agencies)
        is_first_for_st <- !(st %in% shown_statuses)
        if (is_first_for_ag) shown_agencies <- c(shown_agencies, ag)
        if (is_first_for_st) shown_statuses <- c(shown_statuses, st)

        # When showing agency legend: group/name by agency, show once per agency
        # When showing status legend only: group/name by status, show once per status
        # When showing both: agency traces in agency group, status handled by dummy traces below
        if (show_agency_legend) {
          trace_name <- ag
          trace_group <- ag
          trace_showleg <- is_first_for_ag
        } else if (show_status_legend) {
          trace_name <- st
          trace_group <- st
          trace_showleg <- is_first_for_st
        } else {
          trace_name <- ag
          trace_group <- ag
          trace_showleg <- FALSE
        }

        fig <- fig %>%
          plotly::add_trace(
            data = sub,
            x = ~day,
            y = ~n,
            type = "bar",
            name = trace_name,
            legendgroup = trace_group,
            showlegend = trace_showleg,
            marker = list(
              color = plotly::toRGB(bar_color, alpha = bar_alpha),
              line  = list(color = plotly::toRGB(bar_color, alpha = min(bar_alpha + 0.2, 1)), width = 0.6)
            ),
            hoverlabel = list(font = list(size = 16)), # <-- add this
            hovertemplate = paste0(
              "<b>", ag, " \u2014 ", st, "</b><br>",
              "Date: %{x|%b %d}<br>",
              "Count: %{y}<extra></extra>"
            )
          )
      }
    }

    # --- Dummy traces for the Status legend
    # Only needed when BOTH agency and status are selected (to add the second status legend
    # alongside the agency legend from the real traces above).
    if (show_status_legend && show_agency_legend) {
      for (i in seq_along(status_levels)) {
        st <- status_levels[[i]]
        bar_alpha <- alpha_map[[st]]
        fig <- fig %>%
          plotly::add_trace(
            x = list(NULL),
            y = list(NULL),
            type = "bar",
            name = st,
            legendgroup = paste0("__status_", st),
            legendgrouptitle = if (i == 1L) list(text = "Status") else list(text = ""),
            showlegend = TRUE,
            marker = list(
              color = plotly::toRGB("#555555", alpha = bar_alpha),
              line  = list(color = "transparent", width = 0)
            ),
            hoverinfo = "skip"
          )
      }
    }

    # Right margin: only needed when a legend is showing
    right_margin <- if (show_agency_legend || show_status_legend) 160 else 40

    fig %>%
      plotly::layout(
        barmode = "stack",
        xaxis = list(
          type        = "date",
          title       = "",
          rangeslider = list(visible = TRUE)
        ),
        yaxis = list(title = "Submissions"),
        showlegend = show_agency_legend || show_status_legend,
        legend = list(
          orientation   = "v",
          x             = 1.02,
          xanchor       = "left",
          y             = 1,
          yanchor       = "top",
          tracegroupgap = 12,
          title         = list(text = if (show_agency_legend) "Agency" else if (show_status_legend) "Status" else "")
        ),
        bargap = 0.15,
        margin = list(r = right_margin)
      ) %>%
      plotly::config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToAdd = list(hires_btn))
  })

  output$plot_time_status <- renderPlot({
    # If user has not selected a status yet, keep the plot empty
    if (is.null(input$time200_status) || input$time200_status == "") {
      return(NULL)
    }

    df <- base_filtered()

    if (nrow(df) == 0) {
      stop("No data after filtering.")
      # req(nrow(df) > 0) # , "No valid rows loaded from performance file. Check column names."))
    }

    df <- df %>%
      dplyr::filter(submission_status_code == input$time200_status) %>% # status filter
      dplyr::filter(results_count > 0) %>%
      dplyr::mutate(
        results_count = as.numeric(results_count),
        local_validation_time = dplyr::coalesce(as.numeric(local_validation_time), 0),
        local_import_time = dplyr::coalesce(as.numeric(local_import_time), 0),
        obs_validation_time = dplyr::coalesce(as.numeric(obs_validation_time), 0),
        obs_import_time = dplyr::coalesce(as.numeric(obs_import_time), 0),
        total_time = dplyr::coalesce(as.numeric(total_time), 0),

        # Components requested
        salus_time = local_validation_time + local_import_time,
        aqi_time = obs_validation_time + obs_import_time
      ) %>%
      dplyr::filter(!is.na(results_count), results_count >= 25) %>%
      dplyr::mutate(
        salus_per_200 = (salus_time / results_count) * 200 / 60,
        aqi_per_200   = (aqi_time / results_count) * 200 / 60,
        total_per_200 = (total_time / results_count) * 200 / 60
      ) %>%
      tidyr::pivot_longer(
        cols      = c(salus_per_200, aqi_per_200, total_per_200),
        names_to  = "component",
        values_to = "mins_per_200"
      ) %>%
      dplyr::mutate(
        component = dplyr::recode(
          component,
          salus_per_200 = "Salus time",
          aqi_per_200   = "AQI time",
          total_per_200 = "Total time"
        )
      )

    # If no rows after filtering, keep plot empty
    if (nrow(df) == 0) {
      return(NULL)
    }

    ggplot2::ggplot(df, ggplot2::aes(x = component, y = mins_per_200, fill = component)) +
      ggplot2::geom_boxplot(outlier.alpha = 0.3) +
      ggplot2::geom_jitter(width = 0.12, alpha = 0.35, size = 1) +
      geom_hline(yintercept = 1, linetype = "dotted", color = "black", linewidth = 0.8) +
      ggplot2::labs(
        x = NULL,
        y = "Minutes per 200 observations"
      ) +
      coord_cartesian(ylim = c(0, 10)) +
      ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 16)
      )
  })

  # output$plot_time_status <- renderPlot({
  #   df <- base_filtered()
  #   validate(need(nrow(df) > 0, "No data after filtering."))
  #
  #   df %>%
  #     filter(!is.na(total_time)) %>%
  #     ggplot(aes(x = submission_status_code, y = total_time / results_count * 200 / 60, fill = submission_status_code)) +
  #     geom_hline(yintercept = 1, linetype = "dotted", color = "black", linewidth = 0.8) +
  #     geom_boxplot(outlier.alpha = 0.3) +
  #     labs(x = "Status", y = "Time (min)") +
  #     theme_minimal(base_size = 18) +
  #     theme(legend.position = "none")
  # })

  # ---- Table ----
  output$tbl <- renderDT({
    df <- base_filtered()

    datatable(
      df %>% arrange(desc(submission_date)),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top"
    )
  })

  # ---- Research Only: helper to build daily aggregated data ----
  research_daily_val <- reactive({
    df <- base_filtered()
    df %>%
      dplyr::filter(submission_status_code == "SUBMITTED") %>%
      dplyr::mutate(
        day                   = as.Date(with_tz(submission_date, tz = "America/Vancouver")),
        results_count         = suppressWarnings(as.numeric(results_count)),
        sample_count          = suppressWarnings(as.numeric(sample_count)),
        local_validation_time = suppressWarnings(as.numeric(local_validation_time))
      ) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(
        total_results = sum(results_count, na.rm = TRUE),
        total_samples = sum(sample_count, na.rm = TRUE),
        salus_time = sum(local_validation_time, na.rm = TRUE) / 60,
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(salus_time), salus_time > 0)
  })

  research_daily_imp <- reactive({
    df <- base_filtered()
    df %>%
      dplyr::filter(submission_status_code == "SUBMITTED") %>%
      dplyr::mutate(
        day               = as.Date(with_tz(submission_date, tz = "America/Vancouver")),
        results_count     = suppressWarnings(as.numeric(results_count)),
        sample_count      = suppressWarnings(as.numeric(sample_count)),
        local_import_time = suppressWarnings(as.numeric(local_import_time))
      ) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(
        total_results = sum(results_count, na.rm = TRUE),
        total_samples = sum(sample_count, na.rm = TRUE),
        salus_time = sum(local_import_time, na.rm = TRUE) / 60,
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(salus_time), salus_time > 0)
  })

  # ---- Daily results_count discrepancy tracker (from March 26, 2026) ----
  # Uses base_filtered() so user date-range and zero-time filters apply.
  # A discrepancy is any row where results_count and results_count_old differ
  # (including the case where exactly one of the two is NA).
  # The sequence always starts on March 26 and is zero-filled for days with
  # no submissions, so the line is shown continuously from that date.
  daily_discrepancies <- reactive({
    df <- base_filtered()

    DISC_START <- as.Date("2026-03-26")
    empty <- tibble::tibble(day = as.Date(character()), n_discrepancies = integer())

    if (!"results_count_old" %in% names(df)) {
      return(empty)
    }

    disc <- df %>%
      dplyr::mutate(
        day = as.Date(with_tz(submission_date, tz = "America/Vancouver")),
        discrepancy = dplyr::case_when(
          is.na(results_count) & is.na(results_count_old) ~ FALSE,
          is.na(results_count) | is.na(results_count_old) ~ TRUE,
          TRUE ~ results_count != results_count_old
        )
      ) %>%
      dplyr::filter(day >= DISC_START) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(n_discrepancies = sum(discrepancy, na.rm = TRUE), .groups = "drop")

    # If no submissions at all on or after March 26, return empty
    if (nrow(disc) == 0) {
      return(empty)
    }

    # Zero-fill every day from March 26 to the last day seen in filtered data
    end_day <- max(disc$day)
    disc %>%
      tidyr::complete(
        day = seq.Date(DISC_START, end_day, by = "day"),
        fill = list(n_discrepancies = 0L)
      )
  })

  # ---- Time series plot: Background vs Import job counts ----

  compute_event_daily_stats <- function(df) {
    # df must have: start_date (POSIXct), job_count (numeric)

    df_events <- df %>%
      arrange(start_date) %>%
      mutate(
        is_event = job_count > 0,
        event_start_flag = is_event & !lag(is_event, default = FALSE),
        event_id = cumsum(event_start_flag)
      ) %>%
      filter(is_event)

    # Event-level summary
    event_summary <- df_events %>%
      group_by(event_id) %>%
      summarise(
        event_start = min(start_date),
        event_end = max(start_date),
        duration_mins = as.numeric(difftime(event_end, event_start, units = "mins")) + 1,
        .groups = "drop"
      ) %>%
      mutate(event_day = as.Date(event_start)) # assign to start day

    # Daily aggregation
    daily_summary <- event_summary %>%
      group_by(event_day) %>%
      summarise(
        event_frequency = n(),
        avg_duration_mins = mean(duration_mins),
        max_duration_mins = max(duration_mins),
        .groups = "drop"
      )

    daily_summary
  }

  output$plot_weekly_job_counts <- plotly::renderPlotly({
    # Require the dropdown selection
    req(input$aqstracking_choice)

    # Read the background and import CSV files
    # background_data <- tryCatch(
    background_data <- raw_background() %>%
      mutate(start_date = with_tz(start_date, tz = "America/Vancouver"))
    import_data <- raw_import() %>%
      mutate(start_date = with_tz(start_date, tz = "America/Vancouver"))
    #   {
    #     readr::read_csv("7days-background.csv", show_col_types = FALSE) %>%
    #       dplyr::mutate(
    #         startTime = lubridate::ymd_hms(startTime, quiet = TRUE),
    #         jobCount = as.numeric(jobCount)
    #       )
    #   },
    #   error = function(e) {
    #     return(NULL)
    #   }
    # )
    #
    # import_data <- tryCatch(
    #   {
    #     readr::read_csv("7days-import.csv", show_col_types = FALSE) %>%
    #       dplyr::mutate(
    #         startTime = lubridate::ymd_hms(startTime, quiet = TRUE),
    #         jobCount = as.numeric(jobCount)
    #       )
    #   },
    #   error = function(e) {
    #     return(NULL)
    #   }
    # )

    # Validate that both files were read successfully
    validate(need(
      !is.null(background_data) && !is.null(import_data),
      "Could not read the CSV files. Please ensure '7days-background.csv' and '7days-import.csv' are in the working directory."
    ))

    validate(need(
      nrow(background_data) > 0 && nrow(import_data) > 0,
      "One or both CSV files are empty."
    ))

    background_events_daily <- compute_event_daily_stats(background_data)
    import_events_daily <- compute_event_daily_stats(import_data)

    # Color scheme to match other plots in the dashboard
    BACKGROUND_COLOR <- "#0072B2" # Blue (matching benchmark color)
    IMPORT_COLOR <- "#E07B39" # Orange (matching discrepancy color)

    # Determine which traces should be visible based on the dropdown selection
    choice <- input$aqstracking_choice

    # Trace visibility: [Raw-Bg, Raw-Imp, Bg-Avg, Bg-Max, Bg-Freq, Imp-Avg, Imp-Max, Imp-Freq]
    if (choice == "Raw time series") {
      trace_visible <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
      yaxis_title <- "Job count"
      yaxis2_title <- "Job count"
      yaxis_range <- c(0, 500)
      yaxis2_range <- c(0, 500)
    } else if (choice == "Event summary statistics – Background") {
      trace_visible <- c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
      yaxis_title <- "Duration (mins)"
      yaxis2_title <- "Event frequency"
      yaxis_range <- c(0, 120)
      yaxis2_range <- c(0, 10)
    } else { # "Event summary statistics – Import"
      trace_visible <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
      yaxis_title <- "Duration (mins)"
      yaxis2_title <- "Event frequency"
      yaxis_range <- c(0, 120)
      yaxis2_range <- c(0, 10)
    }


    # Create the dual y-axis plot
    fig <- plotly::plot_ly()

    # ===============================
    # RAW TIME SERIES (unchanged)
    # ===============================

    fig <- fig %>%
      # ---- Primary trace: Background job count ----
      plotly::add_trace(
        data = background_data,
        x = ~start_date,
        y = ~job_count,
        type = "scatter",
        mode = "lines",
        name = "Background jobs",
        yaxis = "y",
        line = list(color = BACKGROUND_COLOR, width = 2),
        visible = trace_visible[1],
        hovertemplate = paste0(
          "<b>%{x|%b %d, %Y %H:%M}</b><br>",
          "Background job count: %{y}<br>",
          "<extra></extra>"
        )
      ) %>%
      # ---- Secondary trace: Import job count ----
      plotly::add_trace(
        data = import_data,
        x = ~start_date,
        y = ~job_count,
        type = "scatter",
        mode = "lines",
        name = "Import jobs",
        yaxis = "y2",
        line = list(color = IMPORT_COLOR, width = 2),
        visible = trace_visible[2],
        hovertemplate = paste0(
          "<b>%{x|%b %d, %Y %H:%M}</b><br>",
          "Import job count: %{y}<br>",
          "<extra></extra>"
        )
      )
    # ===============================
    # EVENT SUMMARY — BACKGROUND
    # ===============================

    fig <- fig %>%
      plotly::add_trace(
        data = background_events_daily,
        x = ~event_day,
        y = ~avg_duration_mins,
        yaxis = "y",
        type = "bar",
        name = "Background – Avg duration",
        marker = list(color = BACKGROUND_COLOR),
        visible = trace_visible[3],
        hovertemplate =
          "<b>%{x}</b><br>Avg duration: %{y:.1f} mins<extra></extra>"
      ) %>%
      plotly::add_trace(
        data = background_events_daily,
        x = ~event_day,
        y = ~max_duration_mins,
        yaxis = "y",
        type = "bar",
        name = "Background – Max duration",
        marker = list(
          color = BACKGROUND_COLOR,
          line = list(dash = "dot")
        ),
        visible = trace_visible[4],
        hovertemplate =
          "<b>%{x}</b><br>Max duration: %{y:.1f} mins<extra></extra>"
      ) %>%
      plotly::add_trace(
        data = background_events_daily,
        x = ~event_day,
        y = ~event_frequency,
        type = "scatter",
        mode = "markers",
        name = "Background – Event frequency",
        yaxis = "y2",
        marker = list(color = BACKGROUND_COLOR, size = 8),
        visible = trace_visible[5],
        hovertemplate =
          "<b>%{x}</b><br>Events: %{y}<extra></extra>"
      )

    # ===============================
    # EVENT SUMMARY — IMPORT
    # ===============================

    fig <- fig %>%
      plotly::add_trace(
        data = import_events_daily,
        x = ~event_day,
        y = ~avg_duration_mins,
        yaxis = "y",
        type = "bar",
        name = "Import – Avg duration",
        marker = list(color = IMPORT_COLOR),
        visible = trace_visible[6],
        hovertemplate =
          "<b>%{x}</b><br>Avg duration: %{y:.1f} mins<extra></extra>"
      ) %>%
      plotly::add_trace(
        data = import_events_daily,
        x = ~event_day,
        y = ~max_duration_mins,
        yaxis = "y",
        type = "bar",
        name = "Import – Max duration",
        marker = list(
          color = IMPORT_COLOR,
          line = list(dash = "dot")
        ),
        visible = trace_visible[7],
        hovertemplate =
          "<b>%{x}</b><br>Max duration: %{y:.1f} mins<extra></extra>"
      ) %>%
      plotly::add_trace(
        data = import_events_daily,
        x = ~event_day,
        y = ~event_frequency,
        type = "scatter",
        mode = "markers",
        name = "Import – Event frequency",
        yaxis = "y2",
        marker = list(color = IMPORT_COLOR, size = 8),
        visible = trace_visible[8],
        hovertemplate =
          "<b>%{x}</b><br>Events: %{y}<extra></extra>"
      )


    # ---- Layout + dropdown ----
    # fig <- fig %>%
    #   layout(
    #     barmode = "group",
    #     xaxis = list(title = "Date", type = "date"),
    #     yaxis = list(title = "Job count / Duration (mins)", rangemode = "tozero"),
    #     yaxis2 = list(
    #       title = "Job count / Event frequency",
    #       overlaying = "y", side = "right",
    #       showgrid = FALSE, rangemode = "tozero"
    #     ), # ✅ layout defines yaxis2 (secondary axis) [1](https://plotly.com/r/multiple-axes/)[2](https://stackoverflow.com/questions/39380227/second-y-axis-in-a-r-plotly-graph)
    #     legend = list(orientation = "h", x = 0, y = -0.2),
    #     hovermode = "x unified",
    #     updatemenus = list(
    #       list(
    #         type = "dropdown",
    #         x = 0.01, y = 1.25,
    #         buttons = list(
    #           list(
    #             label = "Raw time series",
    #             method = "update",
    #             args = list(
    #               list(visible = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    #               list(
    #                 yaxis  = list(title = "Job count", range = c(0, 500)),
    #                 yaxis2 = list(title = "Job count", range = c(0, 500), overlaying = "y", side = "right", showgrid = FALSE)
    #               )
    #             )
    #           ),
    #           list(
    #             label = "Event summary statistics – Background",
    #             method = "update",
    #             args = list(
    #               list(visible = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)),
    #               list(
    #                 yaxis  = list(title = "Duration (mins)", range = c(0, 120)),
    #                 yaxis2 = list(title = "Event frequency", overlaying = "y", side = "right", range = c(0, 10), showgrid = FALSE)
    #               )
    #             )
    #           ),
    #           list(
    #             label = "Event summary statistics – Import",
    #             method = "update",
    #             args = list(
    #               list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)),
    #               list(
    #                 yaxis  = list(title = "Duration (mins)", range = c(0, 120)),
    #                 yaxis2 = list(title = "Event frequency", overlaying = "y", side = "right", range = c(0, 10), showgrid = FALSE)
    #               )
    #             )
    #           )
    #         )
    #       )
    #     ),
    #     margin = list(t = 20, r = 90, b = 80, l = 70)
    #   ) %>%
    #   config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToAdd = list(hires_btn))

    # ---- Layout (no dropdown, controlled by Shiny input) ----
    fig <- fig %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = yaxis_title, rangemode = "tozero", range = yaxis_range),
        yaxis2 = list(
          title = yaxis2_title,
          overlaying = "y", side = "right",
          showgrid = FALSE, rangemode = "tozero",
          range = yaxis2_range
        ),
        legend = list(orientation = "h", x = 0, y = -0.2),
        hovermode = "x unified",
        margin = list(t = 20, r = 90, b = 80, l = 70)
      ) %>%
      config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToAdd = list(hires_btn))

    fig
  })

  # ---- Daily MLR (minimum 3 submissions per day) ----
  get_daily_coefs <- function(data, response_var, model_name) {
    data %>%
      dplyr::filter(!is.na(.data[[response_var]]), .data[[response_var]] > 0) %>%
      dplyr::group_by(day) %>%
      dplyr::filter(dplyr::n() >= 3) %>% # ✅ enforce >= 3 points per day
      dplyr::group_modify(~ {
        fit <- lm(
          as.formula(paste(response_var, "~ 0 + results_count + sample_count")),
          data = .x
        )

        coefs <- coef(fit)

        tibble::tibble(
          obs_coef  = coefs["results_count"],
          samp_coef = coefs["sample_count"]
        )
      }) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(model = model_name)
  }

  # ---- Daily benchmark speed ratio ----
  output$plot_sync_benchmarks <- plotly::renderPlotly({
    df <- base_filtered() %>%
      dplyr::filter(submission_status_code == "SUBMITTED") %>%
      dplyr::mutate(
        day = as.Date(with_tz(submission_date, tz = "America/Vancouver")),
        results_count = as.numeric(results_count),
        sample_count = as.numeric(sample_count),
        val_time = as.numeric(local_validation_time) / 60,
        imp_time = as.numeric(local_import_time) / 60
      )

    val_coefs <- get_daily_coefs(df, "val_time", "Validation")
    imp_coefs <- get_daily_coefs(df, "imp_time", "Import")

    validate(need(
      nrow(val_coefs) > 0 && nrow(imp_coefs) > 0,
      "Not enough SUBMITTED data (need >= 3 submissions on the same day) for both validation and import."
    ))

    daily_template_time <- dplyr::bind_rows(val_coefs, imp_coefs) %>%
      dplyr::group_by(day) %>%
      dplyr::summarise(
        obs_coef  = sum(obs_coef, na.rm = TRUE),
        samp_coef = sum(samp_coef, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(benchmark_time = 100 * obs_coef + 10 * samp_coef) %>%
      dplyr::select(-c(obs_coef, samp_coef))

    validate(need(
      nrow(daily_template_time) > 0,
      "No qualifying days found (>= 3 submissions per day). Widen the date range."
    ))

    # Template file spec
    TMPL_OBS <- 100
    TMPL_SAMPLES <- 10

    # Discrepancy series colour — distinct warm orange vs the blue benchmark line
    DISC_COLOR <- "#E07B39"

    disc <- daily_discrepancies()
    has_disc <- nrow(disc) > 0

    plotly::plot_ly() %>%
      # ---- Primary trace: benchmark time ----
      plotly::add_trace(
        data = daily_template_time,
        x = ~day,
        y = ~benchmark_time,
        type = "scatter",
        mode = "lines+markers",
        name = "Benchmark time",
        yaxis = "y",
        line = list(color = "#0072B2", width = 2.5),
        marker = list(
          color = "#0072B2", size = 9,
          line = list(color = "#ffffff", width = 1.5)
        ),
        hovertemplate = paste0(
          "<b>Day of %{x|%b %d, %Y}</b><br>",
          "Benchmark time (in mins): %{y:.3f}<br>",
          "<extra></extra>"
        )
      ) %>%
      # ---- Secondary traces: discrepancy bars + zero reference line ----
      {
        p <- .
        if (has_disc) {
          p %>%
            plotly::add_trace(
              data = disc,
              x = ~day,
              y = ~n_discrepancies,
              type = "bar",
              name = "Count Discrepancies",
              yaxis = "y2",
              marker = list(
                color = plotly::toRGB(DISC_COLOR, alpha = 0.45),
                line  = list(color = plotly::toRGB(DISC_COLOR, alpha = 0.85), width = 1)
              ),
              hovertemplate = paste0(
                "<b>Day of %{x|%b %d, %Y}</b><br>",
                "Discrepancies: %{y}<br>",
                "<extra></extra>"
              )
            ) %>%
            plotly::add_segments(
              x = min(disc$day), xend = max(disc$day),
              y = 0, yend = 0,
              yaxis = "y2",
              line = list(color = DISC_COLOR, width = 1.5, dash = "dot"),
              showlegend = FALSE, hoverinfo = "skip"
            )
        } else {
          p
        }
      } %>%
      plotly::layout(
        xaxis = list(
          title = "Day",
          type  = "date"
        ),
        yaxis = list(
          title    = "Time taken to run benchmark file (in mins)",
          zeroline = FALSE
        ),
        yaxis2 = list(
          title      = "Daily Results Count Discrepancies",
          overlaying = "y",
          side       = "right",
          showgrid   = FALSE,
          zeroline   = FALSE,
          rangemode  = "tozero"
        ),
        legend = list(orientation = "h", x = 0, y = -0.18),
        annotations = list(list(
          text = paste0(
            "Reference File: ", TMPL_OBS, " obs, ", TMPL_SAMPLES, " samples"
          ),
          x = 0.01, xref = "paper", y = 0.99, yref = "paper",
          xanchor = "left", yanchor = "top", showarrow = FALSE,
          font = list(size = 12, family = "monospace"),
          bgcolor = "rgba(255,255,255,0.75)",
          bordercolor = "#aaaaaa", borderwidth = 1
        )),
        hovermode = "x unified",
        margin = list(t = 20, r = 90, b = 80, l = 70)
      ) %>%
      plotly::config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToAdd = list(hires_btn))
  })

  # output$plot_sync_benchmarks <- plotly::renderPlotly({
  #   val_daily <- research_daily_val()
  #   imp_daily <- research_daily_imp()
  #
  #   validate(need(
  #     nrow(val_daily) >= 3 && nrow(imp_daily) >= 3,
  #     "Not enough SUBMITTED data (need >= 3 days) in both validation and import to compute benchmark ratios."
  #   ))
  #
  #   # Template file spec
  #   TMPL_OBS <- 100
  #   TMPL_SAMPLES <- 10
  #   # Earliest allowed week start
  #   WEEK_CUTOFF <- as.Date("2026-03-09")
  #
  #   # Helper: per-week zero-intercept MLR -> predicted time for template file
  #   weekly_template_time <- function(df_day) {
  #     df_day %>%
  #       dplyr::mutate(
  #         week_start = lubridate::floor_date(day, unit = "week", week_start = 1)
  #       ) %>%
  #       dplyr::filter(week_start >= WEEK_CUTOFF) %>%
  #       dplyr::group_by(week_start) %>%
  #       dplyr::filter(dplyr::n() >= 3) %>%
  #       dplyr::summarise(
  #         n_days = dplyr::n(),
  #         tmpl_time = tryCatch(
  #           {
  #             m <- lm(salus_time ~ 0 + total_results + total_samples, data = dplyr::cur_data())
  #             co <- coef(m)
  #             co["total_results"] * TMPL_OBS + co["total_samples"] * TMPL_SAMPLES
  #           },
  #           error = function(e) NA_real_
  #         ),
  #         .groups = "drop"
  #       ) %>%
  #       dplyr::filter(!is.na(tmpl_time))
  #   }
  #
  #   wt_val <- weekly_template_time(val_daily)
  #   wt_imp <- weekly_template_time(imp_daily)
  #
  #   validate(need(
  #     nrow(wt_val) >= 1 && nrow(wt_imp) >= 1,
  #     "No qualifying weeks found (>= 3 days, from Mar 9 2026). Widen the date range."
  #   ))
  #
  #   # Join val + imp by week; total time = val + imp
  #   wt_total <- dplyr::inner_join(
  #     wt_val %>% dplyr::rename(val_time = tmpl_time, val_days = n_days),
  #     wt_imp %>% dplyr::rename(imp_time = tmpl_time, imp_days = n_days),
  #     by = "week_start"
  #   ) %>%
  #     dplyr::mutate(
  #       total_time = val_time + imp_time,
  #       n_days     = pmin(val_days, imp_days)
  #     ) %>%
  #     dplyr::arrange(week_start)
  #
  #   validate(need(
  #     nrow(wt_total) >= 1,
  #     "No weeks where both validation and import have >= 3 days of data."
  #   ))
  #
  #   # Reference = first qualifying week
  #   ref_time <- wt_total$total_time[1]
  #   ref_week <- wt_total$week_start[1]
  #
  #   wt_total <- wt_total %>%
  #     dplyr::mutate(
  #       ratio     = ref_time / total_time,
  #       hover_val = round(val_time, 3),
  #       hover_imp = round(imp_time, 3),
  #       hover_tot = round(total_time, 3)
  #     )
  #
  #   ref_label <- format(ref_week, "%b %d, %Y")
  #
  #   plotly::plot_ly(
  #     data = wt_total,
  #     x = ~week_start,
  #     y = ~ratio,
  #     customdata = ~ cbind(n_days, hover_val, hover_imp, hover_tot),
  #     type = "scatter",
  #     mode = "lines+markers",
  #     name = "Benchmark ratio",
  #     line = list(color = "#0072B2", width = 2.5),
  #     marker = list(
  #       color = "#0072B2", size = 9,
  #       line = list(color = "#ffffff", width = 1.5)
  #     ),
  #     hovertemplate = paste0(
  #       "<b>Week of %{x|%b %d, %Y}</b><br>",
  #       "Benchmark ratio: %{y:.3f}<br>",
  #       "Val. time (template): %{customdata[1]:.3f} min<br>",
  #       "Imp. time (template): %{customdata[2]:.3f} min<br>",
  #       "Total time (template): %{customdata[3]:.3f} min<br>",
  #       "Days of data: %{customdata[0]:.0f}<extra></extra>"
  #     )
  #   ) %>%
  #     plotly::add_segments(
  #       x = min(wt_total$week_start), xend = max(wt_total$week_start),
  #       y = 1, yend = 1,
  #       line = list(color = "#999999", width = 1.5, dash = "dot"),
  #       showlegend = FALSE, hoverinfo = "skip"
  #     ) %>%
  #     plotly::layout(
  #       xaxis = list(
  #         title = paste0("Week start (Monday) — reference week: ", ref_label),
  #         type  = "date"
  #       ),
  #       yaxis = list(
  #         title    = "Benchmark speed ratio  (week 1 time / week N time)",
  #         zeroline = FALSE
  #       ),
  #       annotations = list(list(
  #         text = paste0(
  #           "Reference: ", ref_label,
  #           "<br>Template: ", TMPL_OBS, " obs, ", TMPL_SAMPLES, " samples"
  #         ),
  #         x = 0.01, xref = "paper", y = 0.99, yref = "paper",
  #         xanchor = "left", yanchor = "top", showarrow = FALSE,
  #         font = list(size = 12, family = "monospace"),
  #         bgcolor = "rgba(255,255,255,0.75)",
  #         bordercolor = "#aaaaaa", borderwidth = 1
  #       )),
  #       hovermode = "x unified",
  #       margin = list(t = 20, r = 20, b = 80, l = 70)
  #     ) %>%
  #     plotly::config(scrollZoom = TRUE, displayModeBar = TRUE, modeBarButtonsToAdd = list(hires_btn))
  # })


  # ---- Downloads ----
  # Submissions-only: filtered to the same date range + zero-time toggle,
  # but only the original submissions columns (no performance timing columns).
  output$download_submissions <- downloadHandler(
    filename = function() paste0("submissions_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- base_filtered() %>%
        dplyr::select(
          submission_id, file_name, original_file_name,
          submission_date, submitter_user_id, submitter_agency_name,
          submission_status_code, sample_count, results_count, source_file
        )
      readr::write_csv(df, file)
    }
  )

  # Performance-only: filtered to the same rows, but only the timing columns.
  output$download_performance <- downloadHandler(
    filename = function() paste0("performance_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- base_filtered() %>%
        dplyr::select(
          submission_id, submission_date,
          local_validation_time, obs_validation_time,
          local_import_time, obs_import_time, total_time
        )
      readr::write_csv(df, file)
    }
  )

  # Joined: all columns from base_filtered() (submissions + performance merged).
  output$download_joined <- downloadHandler(
    filename = function() paste0("joined_filtered_", Sys.Date(), ".csv"),
    content = function(file) {
      readr::write_csv(base_filtered(), file)
    }
  )
}

shinyApp(ui, server)
