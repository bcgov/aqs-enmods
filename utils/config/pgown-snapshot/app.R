library(shiny)
library(yaml)
library(rmarkdown)
library(stringr)
library(here)

ui <- fluidPage(
  titlePanel("PGOWN Report Generator"),
  
  sidebarLayout(
    sidebarPanel(
      
      textInput("report_folder", "Name of new report folder:", value = "/2024July"),
      textInput("snapshot_name", "Name of new snapshot:", value = "July 2024"),
      textInput("date_of_current_report", "Date analysis is being conducted:", value = "2025-02-04"),
      textInput("date_of_previous_report", "Date analysis was last conducted:", value = "2024-07-04"),
      textInput("date_of_data_gap_start", "Starting date to study data gaps:", value = "2024-01-01"),
      textInput("date_of_data_gap_end", "Ending date to study data gaps:", value = "2024-06-30"),
      textInput("target_data_default", "General data performance target:", value = "90"),
      textInput("target_data_approval", "Data approval target:", value = "90"),
      textInput("target_data_approval_year", "Year data approval target was set:", value = "2024"),
      textInput("target_data_graded_year", "Year data grading target was set:", value = "2024"),
      textInput("target_data_gap_percent", "Data gap target (in %):", value = "15"),
      textInput("target_data_gap_year", "Year data gap target was set:", value = "2024"),
      textInput("target_days_telemetry", "Threshold (in days) to define a telemetry station as offline:", value = "30"),
      textInput("target_telemetry_stn_offline", "Telemetry stations offline target (in %):", value = "10"),
      textInput("target_days_telemetry_year", "Year data telemetry target was set:", value = "2024"),
      
      
      actionButton("generate_report_parameter_list", "Generate HTML Report"),
    ),
    
    mainPanel()
  )
)

server <- function(input, output, session) {
  
  yaml_content <- reactiveVal(NULL)
  
  observeEvent(input$generate_report_parameter_list, {
    # Create a list of parameters
    report_parameter_list <- list(
      report_folder = input$report_folder,
      snapshot_name = input$snapshot_name,
      date_of_current_report = input$date_of_current_report,
      date_of_previous_report = input$date_of_previous_report,
      date_of_data_gap_start = input$date_of_data_gap_start,
      date_of_data_gap_end = input$date_of_data_gap_end,
      target_data_default = input$target_data_default,
      target_data_approval = input$target_data_approval,
      target_data_approval_year = input$target_data_approval_year,
      target_data_graded_year = input$target_data_graded_year,
      target_data_gap_percent = input$target_data_gap_percent,
      target_data_gap_year = input$target_data_gap_year,
      target_days_telemetry = input$target_days_telemetry,
      target_telemetry_stn_offline = input$target_telemetry_stn_offline,
      target_days_telemetry_year = input$target_days_telemetry_year
    )
    
    # Save parameters to YAML file
    report_parameter_list_file <- "report_parameter_list.yaml"
    write_yaml(report_parameter_list, report_parameter_list_file)
    
    # Render RMarkdown file with parameters
    output_file <- str_c(here(), input$report_folder, "/PGOWN-Snapshot.html")
    render(str_c(here(), input$report_folder, "/coding_flow/rcode/PGOWN-Snapshot.Rmd"), output_format = "html_document", output_file = output_file, params = list(set_subtitle= str_c("Snapshot Report: ", report_parameter_list$snapshot_name)), envir = new.env(parent = globalenv()), knit_root_dir = here())
    
    stopApp()
    
   })
  
}

# Run the application
shinyApp(ui, server)
