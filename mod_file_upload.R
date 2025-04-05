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
    fileInput(ns("file"), NULL,
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
#' @return A list of reactive values: data and data_loaded
#'
file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing processed data and flags
    rv <- reactiveValues(
      data = NULL,
      file_extension = NULL,
      data_loaded = FALSE
    )
    
    # Helper function to count delimiters in a text sample
    count_delimiters <- function(text_sample) {
      delimiters <- c(",", "\t", ";", "|", " ")
      counts <- sapply(delimiters, function(d) {
        total_count <- 0
        lines <- strsplit(text_sample, "\n")[[1]]
        for (line in lines) {
          # Count actual occurrences of delimiter in each line
          positions <- gregexpr(d, line, fixed = TRUE)[[1]]
          # When delimiter isn't found, gregexpr returns -1
          occurrences <- sum(positions != -1)
          total_count <- total_count + occurrences
        }
        return(total_count)
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
    
    # Reactive value to track if first column is suitable for row names
    can_use_rownames <- reactiveVal(FALSE)
    
    # Re-evaluate first column when delimiter or sheet changes
    observeEvent(list(input$import_delimiter, input$import_sheet), {
      req(input$file)
      
      # Skip if we're just initializing
      if (is.null(input$import_delimiter) && is.null(input$import_sheet)) {
        return()
      }
      
      file_ext <- rv$file_extension
      
      tryCatch({
        # Get current data based on selected options
        if(file_ext %in% c("xls", "xlsx")) {
          if(!is.null(input$import_sheet)) {
            # Read entire dataset to check for duplicate row names
            sample_data <- read_excel(
              input$file$datapath,
              sheet = input$import_sheet,
              col_names = input$import_header
            )
          } else {
            return()
          }
        } else {
          delimiter <- input$import_delimiter
          if(is.null(delimiter)) return()
          
          # Read entire dataset to check for duplicate row names
          if(delimiter == "\t") {
            sample_data <- read.delim(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          } else {
            sample_data <- read.csv(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          }
        }
        
        # Re-evaluate if first column can be used as row names
        if(ncol(sample_data) > 1) {
          can_use_rownames(is_likely_rownames(sample_data[[1]]))
        } else {
          can_use_rownames(FALSE)
        }
        
      }, error = function(e) {
        can_use_rownames(FALSE)
      })
    })
    
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
        
        # Read the entire Excel sheet to check for uniqueness in the first column
        sample_data <- read_excel(input$file$datapath, sheet = 1)
        
        # Check if first column might be row names
        if(ncol(sample_data) > 1) {
          has_rownames <- is_likely_rownames(sample_data[[1]])
          # Store this value in the reactive for later use
          can_use_rownames(has_rownames)
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
          
          # Read the entire file to check for uniqueness in the first column
          if(delimiter == "\t") {
            sample_data <- read.delim(input$file$datapath, sep = delimiter, header = FALSE, stringsAsFactors = FALSE)
          } else {
            sample_data <- read.csv(input$file$datapath, sep = delimiter, header = FALSE, stringsAsFactors = FALSE)
          }
          
          # Check if first row looks like a header
          if(nrow(sample_data) > 1) {
            has_header <- is_likely_header(as.character(unlist(sample_data[1, ])))
          }
          
          # Check if first column might be row names
          if(ncol(sample_data) > 1) {
            has_rownames <- is_likely_rownames(sample_data[[1]])
            # Store this value in the reactive for later use
            can_use_rownames(has_rownames)
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
              column(6, checkboxInput(ns("import_header"), "First Row as Header", value = has_header)),
              column(6, uiOutput(ns("rownames_ui")))
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
            column(4, uiOutput(ns("rownames_ui")))
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
    
    # Dynamic UI for rownames checkbox - only show if first column can be used as row names
    output$rownames_ui <- renderUI({
      if (can_use_rownames()) {
        checkboxInput(ns("import_rownames"), "First Column as Row Names", value = can_use_rownames())
      }
    })
    
    # Update import preview based on selected options
    output$import_preview <- renderTable({
      req(input$file)
      
      file_ext <- rv$file_extension
      # Make sure using_rownames is a logical value, not NULL
      using_rownames <- isTRUE(!is.null(input$import_rownames) && input$import_rownames)
      
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
            # Convert to data.frame to ensure compatibility with rownames
            preview_data <- as.data.frame(preview_data, stringsAsFactors = FALSE)
          } else {
            preview_data <- read_excel(
              input$file$datapath,
              col_names = TRUE,
              n_max = 10
            )
            # Convert to data.frame to ensure compatibility with rownames
            preview_data <- as.data.frame(preview_data, stringsAsFactors = FALSE)
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
        
        # Process row names if selected for preview - with safe logical checks
        if(isTRUE(using_rownames) && !is.null(preview_data) && ncol(preview_data) > 1) {
          row_names <- preview_data[[1]]
          preview_data <- preview_data[, -1, drop = FALSE]
          # Set custom row names for the preview
          rownames(preview_data) <- row_names
        }
        
        return(preview_data)
        
      }, error = function(e) {
        return(data.frame(Error = paste("Could not parse file with current settings:", e$message)))
      })
    }, 
    # Control whether to show row names in the table 
    rownames = function() {
      # Only show row names when using first column as row names
      return(isTRUE(!is.null(input$import_rownames) && input$import_rownames))
    },
    striped = TRUE, 
    bordered = TRUE)
    
    # Cancel import
    observeEvent(input$import_cancel, {
      removeModal()
    })
    
            # Confirm import and load the full dataset
    observeEvent(input$import_confirm, {
      req(input$file)
      
      file_ext <- rv$file_extension
      # Make sure using_rownames is a logical value, not NULL
      using_rownames <- isTRUE(!is.null(input$import_rownames) && input$import_rownames)
      
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
        
        # Process row names if selected - with safe logical checks
        if(isTRUE(using_rownames) && !is.null(df) && ncol(df) > 1) {
          row_names <- df[[1]]
          df <- df[, -1, drop = FALSE]
          rownames(df) <- row_names
        } else {
          # If not using row names, prent the automatic row names 1, 2,
          rownames(df) <- NULL
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