# mod_file_upload.R
# A Shiny module for smart file upload and parsing with reproducible code generation

library(shiny)


file_upload_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), NULL,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv", ".txt", ".tsv", ".xls", ".xlsx"
              )),
    uiOutput(ns("code_ui"))
  )
}

file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for storing processed data and flags
    rv <- reactiveValues(
      data = NULL,
      file_extension = NULL,
      data_loaded = FALSE,
      has_rownames = FALSE,
      code = NULL
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
    
  # Helper function to generate reproducible code
  generate_code <- function(file_path, file_ext, delimiter = NULL,
                                        sheet = NULL, header = TRUE,
                                        rownames = FALSE) {
    # Start with library imports
    code <- "# Reproducible code for data import\n"

    # Add necessary libraries
    if (file_ext %in% c("xls", "xlsx")) {
      code <- paste0(code, "library(readxl)\n")
    }
    code <- paste0(code, "library(utils) # For read.csv/read.delim\n") # Add utils library

    # Add the data import code
    if (file_ext %in% c("xls", "xlsx")) {
      # For Excel files
      # Note: The original app reads Excel sheets directly to data.frame
      # with readxl::read_excel and then as.data.frame. This is replicated.
      code <- paste0(
        code,
        "data <- readxl::read_excel(\n",
        "  path = \"", file_path, "\",\n",
        "  sheet = ", if (is.numeric(sheet)) as.character(sheet) else paste0("\"", sheet, "\""), ",\n", # Handle numeric or named sheets
        "  col_names = ", as.character(header), "\n",
        ")\n",
        "data <- as.data.frame(data, stringsAsFactors = FALSE)\n"
      )
    } else {
      # For CSV and other delimited text files
      delimiter_name <- switch(delimiter,
                              "," = "comma",
                              "\t" = "tab",
                              ";" = "semicolon",
                              "|" = "pipe",
                              " " = "space")

      if (delimiter == "\t") {
        code <- paste0(
          code,
          "data <- utils::read.delim(\n", # Explicitly use utils::
          "  file = \"", file_path, "\",\n",
          "  header = ", as.character(header), ",\n",
          "  sep = \"\\t\",\n",
          "  stringsAsFactors = FALSE,\n",
          "  check.names = TRUE\n", # Changed from FALSE to TRUE to match server logic
          ")\n"
        )
      } else {
        code <- paste0(
          code,
          "data <- utils::read.csv(\n", # Explicitly use utils::
          "  file = \"", file_path, "\",\n",
          "  header = ", as.character(header), ",\n",
          "  sep = \"", gsub("\\", "\\\\", delimiter, fixed = TRUE), "\",\n",
          "  stringsAsFactors = FALSE,\n",
          "  check.names = TRUE\n", # Changed from FALSE to TRUE to match server logic
          ")\n"
        )
      }
    }

    # Add column name sanitization step to match server logic
    code <- paste0(
      code,
      "\n# Sanitize column names (replicate app behavior)\n",
      "colnames(data) <- gsub(\"-\", \"_\", colnames(data))\n",
      "colnames(data) <- gsub(\" \", \"\", colnames(data))\n"
    )

    # Handle row names if applicable
    if (rownames) {
      code <- paste0(
        code,
        "\n# Set row names from first column (replicate app behavior)\n",
        "# Ensure first column exists before setting row names\n",
        "if (ncol(data) > 1) {\n", # Add check for number of columns
        "  row_names <- data[[1]]\n",
        "  data <- data[, -1, drop = FALSE]\n",
        "  rownames(data) <- row_names\n",
        "} else {\n",
        "  warning(\"Cannot set row names: dataset has only one column after import.\")\n", # Add warning
        "}\n"
      )
    }

    return(code)
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
            sample_data <- readxl::read_excel(
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
      file_ext <- tolower(gsub("^.*\\.(.*)$", "\\1", input$file$name))
      rv$file_extension <- file_ext
      
      # Initialize settings based on file type
      has_header <- TRUE
      has_rownames <- FALSE
      delimiter <- ","
      sheet <- 1
      
      # Read a sample of the file to analyze
      if(file_ext %in% c("xls", "xlsx")) {
        # For Excel files, get sheet names
        sheets <- readxl::excel_sheets(input$file$datapath)
        
        # Read the entire Excel sheet to check for uniqueness in the first column
        sample_data <- readxl::read_excel(input$file$datapath, sheet = 1)
        
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
            preview_data <- readxl::read_excel(
              input$file$datapath,
              sheet = input$import_sheet,
              col_names = input$import_header,
              n_max = 10
            )
            # Convert to data.frame to ensure compatibility with rownames
            preview_data <- as.data.frame(preview_data, stringsAsFactors = FALSE)
          } else {
            preview_data <- readxl::read_excel(
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
          df <- readxl::read_excel(
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
              check.names = TRUE # Ensure column names are valid R names
            )
          } else {
            df <- read.csv(
              input$file$datapath,
              header = input$import_header,
              sep = delimiter,
              stringsAsFactors = FALSE,
              check.names = TRUE # Ensure column names are valid R names
            )
          }
        }

        # Remove "-" or "." from sample names ----------
        colnames(df) <- gsub("-", "_", colnames(df))
        colnames(df) <- gsub(" ", "", colnames(df))
        #browser()
        # Process row names if selected - with safe logical checks
        if(isTRUE(using_rownames) && !is.null(df) && ncol(df) > 1) {
          row_names <- df[[1]]
          df <- df[, -1, drop = FALSE]
          rownames(df) <- row_names
          rv$has_rownames <- TRUE
        } else {
          rownames(df) <- NULL # does nothing
          rv$has_rownames <- FALSE
        }
        
        # Store the data
        rv$data <- df
        rv$data_loaded <- TRUE
        
        # Generate reproducible code
        rv$code <- generate_code(
          file_path = input$file$name,
          file_ext = file_ext,
          delimiter = input$import_delimiter,
          sheet = input$import_sheet,
          header = input$import_header,
          rownames = using_rownames
        )
        
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
      data_loaded = reactive({ rv$data_loaded }),
      has_rownames = reactive({ rv$has_rownames }),
      code = reactive({ rv$code })
    ))
  })
}


guess_transform <- function(data_matrix) {
  # sample this meany rows or columns
  n_sample <- 500

  # Check if input is valid
  if (!is.matrix(data_matrix) && !is.data.frame(data_matrix)) {
    return(0)  # Return 0 for invalid input
  }
  
  # Convert to matrix if it's a data frame
  if (is.data.frame(data_matrix)) {
    # Try to convert to numeric matrix, handling potential errors
    tryCatch({
      data_matrix <- as.matrix(data_matrix)
      data_matrix <- matrix(as.numeric(data_matrix), nrow = nrow(data_matrix))
    }, error = function(e) {
      return(0)  # Return 0 if conversion fails
    })
  }
  
  # Check if matrix is empty or contains only NAs
  if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0 || all(is.na(data_matrix))) {
    return(0)
  }
  
  if (nrow(data_matrix) > n_sample) {
    sampled_rows <- sample(1:nrow(data_matrix), n_sample)
    data_matrix <- data_matrix[sampled_rows, , drop = FALSE]
  }
  
  # Sample columns if more than 500
  if (ncol(data_matrix) > n_sample) {
    sampled_cols <- sample(1:ncol(data_matrix), n_sample)
    data_matrix <- data_matrix[, sampled_cols, drop = FALSE]
  }
  
  # Calculate row and column medians, ignoring NA values
  row_medians <- apply(data_matrix, 1, median, na.rm = TRUE)
  col_medians <- apply(data_matrix, 2, median, na.rm = TRUE)
  
  # Check if all medians are NA
  if (all(is.na(row_medians)) || all(is.na(col_medians))) {
    return(0)
  }
  
  # Calculate MAD of medians
  row_mad <- mad(row_medians, na.rm = TRUE)
  col_mad <- mad(col_medians, na.rm = TRUE)
  
  # Handle edge cases where MAD is 0 or NA
  if (is.na(row_mad) || is.na(col_mad) || row_mad == 0 || col_mad == 0) {
    # If MAD is 0, calculate standard deviation instead
    row_mad <- sd(row_medians, na.rm = TRUE)
    col_mad <- sd(col_medians, na.rm = TRUE)
    
    # If still 0 or NA, return 0
    if (is.na(row_mad) || is.na(col_mad) || row_mad == 0 || col_mad == 0) {
      return(0)
    }
  }
  
  # Compare MADs without division
  if (col_mad <= row_mad) {  # Rows are variables, columns are observations
    if (max(row_medians) < 10 * min(row_medians)) {
      return(2)  # Largest row_mad < 10 times smallest row_mad
    } else {
      return(3)  # Otherwise, recommend row scaling
    }
  } else {  # Columns are variables, rows are observations
    if (max(col_medians) < 10 * min(col_medians)) {
      return(4)  # Largest col_mad < 10 times smallest col_mad
    } else {
      return(5)  # Otherwise, recommend column scaling
    }
  }
}