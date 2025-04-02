# app.R
library(shiny)
library(pheatmap)
library(RColorBrewer)
library(DT)
library(readxl)  # Required for the file upload module
library(e1071)   # Required for the preprocessing module (skewness calculation)
library(ggplot2) # Required for the preprocessing module (histogram)

# Load the custom modules
source("mod_file_upload.R")
source("mod_preprocess.R")
source("utilities.R")
ui <- fluidPage(
  titlePanel("Interactive Heatmap Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File upload module UI
      fileUploadUI("fileUpload"),
      
      # Preprocessing module UI - only shown after data is loaded
      conditionalPanel(
        condition = "output.dataLoaded",
        hr(),
        h4("Data Preprocessing"),
        preprocessButtonUI("preprocess"),
        tags$div(style = "margin-top: 10px;", uiOutput("preprocessing_status"))
      ),
      
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
                 p("This Shiny app allows you to upload a data matrix, preprocess it, and generate a customized heatmap using the pheatmap package."),
                 tags$ol(
                   tags$li(strong("Upload Data:"), " Use the file upload button to select a CSV, TSV, or Excel file containing your data matrix."),
                   tags$li(strong("Configure Import:"), " A pop-up window will appear with auto-detected settings for your file. You can adjust these settings if needed."),
                   tags$li(strong("Preprocess Data:"), " After uploading, a preprocessing dialog will automatically appear (or use the Preprocess Data button to open it later):"),
                   tags$ul(
                     tags$li(em("Missing Values:"), " Options to handle NA values"),
                     tags$li(em("Log Transformation:"), " Apply log10 transformation with customizable constant"),
                     tags$li(em("Centering/Scaling:"), " Apply various normalization methods by row or column"),
                     tags$li(em("Outlier Capping:"), " Cap extreme values based on z-score")
                   ),
                   tags$li(strong("Customize Heatmap:"), " Adjust the various settings in the sidebar to customize your heatmap:"),
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
                 h4("Preprocessing Features"),
                 p("The Preprocess Data button provides powerful options to transform your data:"),
                 tags$ul(
                   tags$li(strong("Log Transformation:"), " Convert data to log10 scale, automatically suggested for skewed data"),
                   tags$li(strong("Missing Value Handling:"), " Multiple options to handle NA values"),
                   tags$li(strong("Centering and Scaling:"), " Normalize data by row or column"),
                   tags$li(strong("Outlier Capping:"), " Control extreme values with z-score thresholds"),
                   tags$li(strong("Visual Feedback:"), " Dynamic histogram showing data distribution")
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Use the file upload module
  fileData <- fileUploadServer("fileUpload")
  
  # Track if data is loaded for UI conditionals
  output$dataLoaded <- reactive({
    return(fileData$data_loaded())
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Create a reactive that provides the data for processing
  # It will first use the file upload data, then switch to preprocessed data if available
  current_data <- reactive({
    # Get uploaded data
    uploaded_data <- fileData$data()
    
    # If preprocessing has been applied, use that data instead
    if (!is.null(preprocessed_data()) && fileData$data_loaded()) {
      return(preprocessed_data())
    } else {
      return(uploaded_data)
    }
  })
  
  # Use the preprocessing module
  preprocessingData <- preprocessServer("preprocess", reactive({
    fileData$data()
  }))
  
  # Get the preprocessed data
  preprocessed_data <- reactive({
    return(preprocessingData$processed_data())
  })
  
  # Show preprocessing status
  output$preprocessing_status <- renderUI({
    if (!is.null(preprocessed_data()) && fileData$data_loaded()) {
      tags$div(
        icon("check-circle"), 
        "Data has been preprocessed", 
        style = "color: green; font-style: italic;"
      )
    } else {
      tags$div(
        icon("info-circle"), 
        "Data uses original values", 
        style = "color: grey; font-style: italic;"
      )
    }
  })
  
  # Preview of the current data (original or preprocessed)
  output$data_preview <- renderDT({
    req(current_data())
    datatable(current_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Helper function to convert data to a numeric matrix before creating the heatmap
  prepare_heatmap_data <- function(data) {
    # Check if data is already a matrix
    if(!is.matrix(data)) {
      # Convert all columns to numeric if possible
      numeric_data <- as.data.frame(lapply(data, function(x) {
        as.numeric(as.character(x))
      }))
      # Convert to matrix
      data_matrix <- as.matrix(numeric_data)
    } else {
      data_matrix <- data
    }
    return(data_matrix)
  }
  
  # Generate the heatmap using current data
  output$heatmap <- renderPlot({
    req(current_data())
    
    # Convert the data to a numeric matrix for the heatmap
    heatmap_data <- prepare_heatmap_data(current_data())
    
    # Get the color palette and reverse if needed
    colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
    if(input$color_reverse) {
      colors <- rev(colors)
    }
    
    # Create the heatmap
    pheatmap(
      mat = heatmap_data,
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
    req(current_data())
    
    # Determine if we're using preprocessed data
    data_status <- if (!is.null(preprocessed_data()) && fileData$data_loaded()) {
      "Data has been preprocessed"
    } else {
      "Using original data"
    }
    
    paste0(
      "Data Matrix Size: ", nrow(current_data()), " rows × ", ncol(current_data()), " columns\n",
      "Status: ", data_status, "\n",
      "Color Palette: ", input$color, (if(input$color_reverse) " (Reversed)" else ""), "\n",
      "Clustering: ", (if(input$cluster_rows) "Rows, " else ""), (if(input$cluster_cols) "Columns" else ""), "\n",
      "Scaling: ", input$scale, "\n",
      "Display Size: ", input$width, " × ", input$height, " pixels"
    )
  })
  
  # Download handlers using current data
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = input$width/72, height = input$height/72)
      
      # Convert the data to a numeric matrix for the heatmap
      heatmap_data <- prepare_heatmap_data(current_data())
      
      # Get the color palette and reverse if needed
      colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
      if(input$color_reverse) {
        colors <- rev(colors)
      }
      
      # Create the heatmap
      pheatmap(
        mat = heatmap_data,
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
      
      # Convert the data to a numeric matrix for the heatmap
      heatmap_data <- prepare_heatmap_data(current_data())
      
      # Get the color palette and reverse if needed
      colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
      if(input$color_reverse) {
        colors <- rev(colors)
      }
      
      # Create the heatmap
      pheatmap(
        mat = heatmap_data,
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