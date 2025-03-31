# app.R
library(shiny)
library(pheatmap)
library(RColorBrewer)
library(DT)
library(readxl)  # Required for the module
# Load the file upload module
source("mod_file_upload.R")

ui <- fluidPage(
  titlePanel("Interactive Heatmap Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Use the file upload module UI
      fileUploadUI("fileUpload"),
      
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
  # Use the file upload module server
  fileData <- fileUploadServer("fileUpload")
  
  # Output flag to control UI display
  output$dataLoaded <- reactive({
    return(fileData$data_loaded())
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Preview of the uploaded data
  output$data_preview <- renderDT({
    req(fileData$data())
    datatable(fileData$data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Generate the heatmap
  output$heatmap <- renderPlot({
    req(fileData$matrix())
    
    # Get the color palette and reverse if needed
    colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
    if(input$color_reverse) {
      colors <- rev(colors)
    }
    
    # Create the heatmap
    pheatmap(
      mat = fileData$matrix(),
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
    req(fileData$matrix())
    
    paste0(
      "Data Matrix Size: ", nrow(fileData$matrix()), " rows × ", ncol(fileData$matrix()), " columns\n",
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
        mat = fileData$matrix(),
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
        mat = fileData$matrix(),
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