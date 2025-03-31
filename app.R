# app.R
library(shiny)
library(pheatmap)
library(RColorBrewer)
library(readr)
library(DT)
library(readxl)
library(tools)

ui <- fluidPage(
  titlePanel("Interactive Heatmap Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File upload section
      fileInput("file", "Upload Data Matrix",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv", ".txt", ".tsv", ".xls", ".xlsx"
                )),
      
      # Heatmap customization section - only shown after data is loaded
      conditionalPanel(
        condition = "output.dataLoaded",
        hr(),
        h4("Heatmap Customization"),
        
        # Color scheme selection
        selectInput("color", "Color Palette",
                    choices = c("RdBu", "RdYlBu", "YlOrRd", "YlGnBu", "Blues", "Greens", "Purples", "Reds", "OrRd"),
                    selected = "RdYlBu"),
        
        checkboxInput("color_reverse", "Reverse Colors", FALSE),
        
        # Clustering options
        checkboxInput("cluster_rows", "Cluster Rows", TRUE),
        checkboxInput("cluster_cols", "Cluster Columns", TRUE),
        
        # Scaling options
        radioButtons("scale", "Scale Data",
                     choices = c("None" = "none", "By Row" = "row", "By Column" = "column"),
                     selected = "none"),
        
        # Font size
        sliderInput("fontsize", "Cell Font Size", min = 4, max = 14, value = 8),
        
        # Size adjustments
        hr(),
        h4("Heatmap Size"),
        sliderInput("width", "Width (px)", min = 400, max = 1200, value = 600),
        sliderInput("height", "Height (px)", min = 400, max = 1200, value = 600),
        
        # Download buttons
        hr(),
        downloadButton("downloadPDF", "Download PDF"),
        downloadButton("downloadPNG", "Download PNG")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Heatmap", 
                 plotOutput("heatmap", width = "100%", height = "600px"),
                 conditionalPanel(
                   condition = "output.dataLoaded",
                   hr(),
                   h4("Heatmap Settings Summary"),
                   verbatimTextOutput("settings_summary")
                 )
        ),
        tabPanel("Data Preview", 
                 h4("Raw Data Preview"),
                 DTOutput("data_preview")
        ),
        tabPanel("Help",
                 h3("How to Use This App"),
                 p("This Shiny app allows you to upload a data matrix and generate a customized heatmap using the pheatmap package."),
                 tags$ol(
                   tags$li(strong("Upload Data:"), " Use the file upload button to select a CSV, TSV, or Excel file containing your data matrix."),
                   tags$li(strong("Configure Import:"), " A pop-up window will appear with auto-detected settings for your file. You can adjust these settings if needed."),
                   tags$li(strong("Customize Settings:"), " Adjust the various settings in the sidebar to customize your heatmap:"),
                   tags$ul(
                     tags$li(em("Color Palette:"), " Choose from various color schemes"),
                     tags$li(em("Clustering:"), " Enable/disable row and column clustering"),
                     tags$li(em("Scaling:"), " Scale data by row, column, or leave unscaled"),
                     tags$li(em("Font Size:"), " Adjust text size in cells"),
                     tags$li(em("Heatmap Size:"), " Control the dimensions of the heatmap")
                   ),
                   tags$li(strong("Download:"), " Save your heatmap as a PDF or PNG file")
                 ),
                 h4("Data Format Requirements"),
                 p("Your input file can be:"),
                 tags$ul(
                   tags$li("Excel file (.xls, .xlsx)"),
                   tags$li("CSV file (.csv)"),
                   tags$li("TSV file (.tsv) or other delimited text files")
                 ),
                 p("The app will try to automatically detect:"),
                 tags$ul(
                   tags$li("The appropriate delimiter"),
                   tags$li("Whether the first column contains row names"),
                   tags$li("Whether the first row contains column headers")
                 ),
                 p("You can verify and adjust these settings in the pop-up that appears after file upload.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for storing processed data and flags
  rv <- reactiveValues(
    data = NULL,
    matrix = NULL,
    file_extension = NULL,
    data_loaded = FALSE
  )
  
  # Output flag to control UI display
  output$dataLoaded <- reactive({
    return(rv$data_loaded)
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
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
    # For example, headers are often character while data is numeric
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
      title = "Configure Data Import",
      
      # File type-specific controls
      if(file_ext %in% c("xls", "xlsx")) {
        tagList(
          selectInput("import_sheet", "Sheet:", choices = sheets, selected = sheet),
          checkboxInput("import_header", "First Row as Header", value = has_header),
          checkboxInput("import_rownames", "First Column as Row Names", value = has_rownames)
        )
      } else {
        tagList(
          selectInput("import_delimiter", "Delimiter:",
                      choices = c(Comma = ",", Tab = "\t", Semicolon = ";", Pipe = "|", Space = " "),
                      selected = delimiter),
          checkboxInput("import_header", "First Row as Header", value = has_header),
          checkboxInput("import_rownames", "First Column as Row Names", value = has_rownames)
        )
      },
      
      # Preview table
      h4("Data Preview (First 5 Rows)"),
      DTOutput("import_preview"),
      
      footer = tagList(
        actionButton("import_cancel", "Cancel"),
        actionButton("import_confirm", "Import Data", class = "btn-primary")
      ),
      
      size = "l",
      easyClose = FALSE
    ))
  })
  
  # Update import preview based on selected options
  output$import_preview <- renderDT({
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
            n_max = 5
          )
        } else {
          preview_data <- read_excel(
            input$file$datapath,
            col_names = TRUE,
            n_max = 5
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
            nrows = 5,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        } else {
          preview_data <- read.csv(
            input$file$datapath,
            header = input$import_header,
            sep = delimiter,
            nrows = 5,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }
      }
      
      datatable(preview_data, options = list(scrollX = TRUE, dom = 't'))
      
    }, error = function(e) {
      data.frame(Error = paste("Could not parse file with current settings:", e$message))
    })
  })
  
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
      
      # Convert all columns to numeric, handling non-numeric values
      for(i in 1:ncol(df)) {
        df[[i]] <- as.numeric(df[[i]])
      }
      
      # Store the data
      rv$data <- df
      rv$matrix <- as.matrix(df)
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
  
  # Preview of the uploaded data
  output$data_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Generate the heatmap
  output$heatmap <- renderPlot({
    req(rv$matrix)
    
    # Get the color palette and reverse if needed
    colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
    if(input$color_reverse) {
      colors <- rev(colors)
    }
    
    # Create the heatmap
    pheatmap(
      mat = rv$matrix,
      color = colors,
      cluster_rows = input$cluster_rows,
      cluster_cols = input$cluster_cols,
      scale = input$scale,
      fontsize = input$fontsize,
      main = "Customized Heatmap"
    )
  }, width = function() input$width, height = function() input$height)
  
  # Display settings summary
  output$settings_summary <- renderText({
    req(rv$matrix)
    
    paste0(
      "Data Matrix Size: ", nrow(rv$matrix), " rows × ", ncol(rv$matrix), " columns\n",
      "Color Palette: ", input$color, (if(input$color_reverse) " (Reversed)" else ""), "\n",
      "Clustering: ", (if(input$cluster_rows) "Rows, " else ""), (if(input$cluster_cols) "Columns" else ""), "\n",
      "Scaling: ", input$scale, "\n",
      "Display Size: ", input$width, " × ", input$height, " pixels"
    )
  })
  
  # Download handlers
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = input$width/72, height = input$height/72)
      
      # Get the color palette and reverse if needed
      colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
      if(input$color_reverse) {
        colors <- rev(colors)
      }
      
      # Create the heatmap
      pheatmap(
        mat = rv$matrix,
        color = colors,
        cluster_rows = input$cluster_rows,
        cluster_cols = input$cluster_cols,
        scale = input$scale,
        fontsize = input$fontsize,
        main = "Customized Heatmap"
      )
      
      dev.off()
    }
  )
  
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, res = 72)
      
      # Get the color palette and reverse if needed
      colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
      if(input$color_reverse) {
        colors <- rev(colors)
      }
      
      # Create the heatmap
      pheatmap(
        mat = rv$matrix,
        color = colors,
        cluster_rows = input$cluster_rows,
        cluster_cols = input$cluster_cols,
        scale = input$scale,
        fontsize = input$fontsize,
        main = "Customized Heatmap"
      )
      
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)