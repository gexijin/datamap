# mod_file_upload.R
# A Shiny module for smart file upload and parsing

library(shiny)
library(readxl)
library(tools)

#' UI function for file upload module
#'
#' @param id The module namespace id
#' @return A tagList of UI elements
#'
file_upload_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), "Data Import",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv", ".txt", ".tsv", ".xls", ".xlsx"
              ))
  )
}

#' Server function for file upload module
#'
#' @param id The module namespace id
#' @return A list of reactive values: data, matrix, and data_loaded
#'
file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing processed data and flags
    rv <- reactiveValues(
      data = NULL,
      matrix = NULL,
      file_extension = NULL,
      data_loaded = FALSE
    )
    
    # Helper function to count delimiters in a text sample
    count_delimiters <- function(text_sample) {
      delimiters <- c(",", "\t", ";", "|", " ")
      counts <- sapply(delimiters, function(d) {
        sum(sapply(strsplit(text_sample, "\n"), function(line) {
          sum(gregexpr(d, line, fixed = TRUE)[[1]] > 0)
        }))
      })
      names(counts) <- delimiters
      return(counts)
    }
    
    # Helper function to check if a column looks like row names
    is_likely_rownames <- function(column) {
      # Check if values are unique
      if(length(unique(column)) != length(column)) {
        return(FALSE)
      }
      
      # Check if values are mostly character-like (not purely numeric)
      numeric_count <- sum(!is.na(suppressWarnings(as.numeric(column))))
      if(numeric_count / length(column) > 0.9) {
        return(FALSE)
      }
      
      return(TRUE)
    }
    
    # Helper function to check if a row looks like column headers
    is_likely_header <- function(row) {
      # Check if row is different from the rest of the data in type
      numeric_count <- sum(!is.na(suppressWarnings(as.numeric(row))))
      if(numeric_count / length(row) < 0.5) {
        return(TRUE)
      }
      
      return(FALSE)
    }
    
    # Watch for file uploads
    observeEvent(input$file, {
      req(input$file)
      
      # Get file extension
      file_ext <- tolower(file_ext(input$file$name))
      rv$file_extension <- file_ext
      
      # Initialize settings based on file type
      has_header <- TRUE
      has_rownames <- FALSE
      delimiter <- ","
      sheet <- 1
      
      # Read a sample of the file to analyze
      if(file_ext %in% c("xls", "xlsx")) {
        # For Excel files, get sheet names
        sheets <- excel_sheets(input$file$datapath)
        
        # Read first few rows to check for headers and row names
        sample_data <- read_excel(input$file$datapath, sheet = 1, n_max = 10)
        
        # Check if first column might be row names
        if(ncol(sample_data) > 1) {
          has_rownames <- is_likely_rownames(sample_data[[1]])
        }
        
        # Default to having headers for Excel
        has_header <- TRUE
        
      } else {
        # For text files, read first few lines to analyze
        file_con <- file(input$file$datapath, "r")
        file_lines <- readLines(file_con, n = 10)
        close(file_con)
        
        # Determine the most likely delimiter by counting occurrences
        if(length(file_lines) > 0) {
          delim_counts <- count_delimiters(paste(file_lines, collapse = "\n"))
          delimiter <- names(which.max(delim_counts))
          
          # If space was detected as the delimiter, double-check if tab is more appropriate
          if(delimiter == " " && grepl("\t", paste(file_lines, collapse = ""))) {
            delimiter <- "\t"
          }
          
          # Try parsing first 10 rows to check for headers and row names
          if(delimiter == "\t") {
            sample_data <- read.delim(input$file$datapath, nrows = 10, sep = delimiter, header = FALSE, stringsAsFactors = FALSE)
          } else {
            sample_data <- read.csv(input$file$datapath, nrows = 10, sep = delimiter, header = FALSE, stringsAsFactors = FALSE)
          }
          
          # Check if first row looks like a header
          if(nrow(sample_data) > 1) {
            has_header <- is_likely_header(as.character(unlist(sample_data[1, ])))
          }
          
          # Check if first column might be row names
          if(ncol(sample_data) > 1) {
            has_rownames <- is_likely_rownames(sample_data[[1]])
          }
        }
      }
      
      # Display a modal dialog with configurable import settings
      showModal(modalDialog(
        title = "Data Import",
        
        # File type-specific controls
        if(file_ext %in% c("xls", "xlsx")) {
          tagList(
            selectInput(ns("import_sheet"), "Sheet:", choices = sheets, selected = sheet),
            fluidRow(
              checkboxInput(ns("import_header"), "First Row as Header", value = has_header),
              checkboxInput(ns("import_rownames"), "First Column as Row Names", value = has_rownames)
            )
          )
        } else {
          fluidRow(
            column(4, 
              div(style = "display: flex; align-items: center;", 
                span("Delimiter:", style = "margin-right: 10px;"),
                selectInput(ns("import_delimiter"), NULL,
                          choices = c(Comma = ",", Tab = "\t", Semicolon = ";", Pipe = "|", Space = " "),
                          selected = delimiter, width = "120px")
              )
            ),
            column(4, checkboxInput(ns("import_header"), "First Row as Header", value = has_header)),
            column(4, checkboxInput(ns("import_rownames"), "First Column as Row Names", value = has_rownames))
          )
        },
        
        # Preview table
        div(style = "overflow-x: auto; max-width: 100%;",
            tableOutput(ns("import_preview"))),
        
        footer = tagList(
          actionButton(ns("import_cancel"), "Cancel"),
          actionButton(ns("import_confirm"), "Import Data", class = "btn-primary")
        ),
        
        size = "l",
        easyClose = FALSE
      ))
    })
    
    # Update import preview based on selected options
    output$import_preview <- renderTable({
      req(input$file)
      
      file_ext <- rv$file_extension
      
      tryCatch({
        # Get preview data based on selected import options
        if(file_ext %in% c("xls", "xlsx")) {
          if(!is.null(input$import_sheet)) {
            preview_data <- read_excel(
              input$file$datapath,
              sheet = input$import_sheet,
              col_names = input$import_header,
              n_max = 10
            )
          } else {
            preview_data <- read_excel(
              input$file$datapath,
              col_names = TRUE,
              n_max = 10
            )
          }
        } else {
          delimiter <- input$import_delimiter
          if(is.null(delimiter)) delimiter <- ","
          
          if(delimiter == "\t") {
            preview_data <- read.delim(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              nrows = 10,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          } else {
            preview_data <- read.csv(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              nrows = 10,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
        
        return(preview_data)
        
      }, error = function(e) {
        return(data.frame(Error = paste("Could not parse file with current settings:", e$message)))
      })
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)
    
    # Cancel import
    observeEvent(input$import_cancel, {
      removeModal()
    })
    
    # Confirm import and load the full dataset
    observeEvent(input$import_confirm, {
      req(input$file)
      
      file_ext <- rv$file_extension
      
      # Import full dataset based on selected options
      tryCatch({
        if(file_ext %in% c("xls", "xlsx")) {
          df <- read_excel(
            input$file$datapath,
            sheet = input$import_sheet,
            col_names = input$import_header
          )
          df <- as.data.frame(df)
        } else {
          delimiter <- input$import_delimiter
          
          if(delimiter == "\t") {
            df <- read.delim(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          } else {
            df <- read.csv(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
        
        # Process row names if selected
        if(input$import_rownames && ncol(df) > 1) {
          row_names <- df[[1]]
          df <- df[, -1, drop = FALSE]
          rownames(df) <- row_names
        }
        
        
        # Store the data
        rv$data <- df
        rv$data_loaded <- TRUE
        
        removeModal()
        
      }, error = function(e) {
        showNotification(
          paste("Error importing data:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # Return a list of reactive values to be used in the main app
    return(list(
      data = reactive({ rv$data }),
      data_loaded = reactive({ rv$data_loaded })
    ))
  })
}