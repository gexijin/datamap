library(shiny)
library(pheatmap)
library(RColorBrewer)
library(readxl)  # file upload module
library(DT)
library(e1071)   # transform module (skewness calculation)
library(grid)    # needed for grid.draw

source("mod_file_upload.R")
source("mod_transform.R")
source("utilities.R")

ui <- fluidPage(
  titlePanel("Interactive Heatmap Generator"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # File upload module UI for main data
      file_upload_ui("file_upload"),
      
      # Column annotation file upload widget
      tags$div(
        tags$h4("Column Annotations", style="margin-top: 15px;"),
        file_upload_ui("col_annotation_file_upload")
      ),
      
      # Dynamic UI for selecting column annotation rows
      conditionalPanel(
        condition = "output.col_annotation_uploaded",
        uiOutput("col_annotation_select_ui")
      ),
      
      # Row annotation file upload widget
      tags$div(
        tags$h4("Row Annotations", style="margin-top: 15px;"),
        file_upload_ui("row_annotation_file_upload")
      ),
      
      # Dynamic UI for selecting row annotation columns
      conditionalPanel(
        condition = "output.row_annotation_uploaded",
        uiOutput("row_annotation_select_ui")
      ),
      
      # Transform module UI - only shown after data is loaded
      conditionalPanel(
        condition = "output.data_loaded",
        hr(),
        fluidRow(
          column(8, transform_ui("transform")),
          column(4, uiOutput("transform_status"))
        )
      ),
      
      # Heatmap customization section - only shown after data is loaded
      conditionalPanel(
        condition = "output.data_loaded",
        hr(),
        
        # Clustering options - compact layout
        fluidRow(
          column(6, checkboxInput("cluster_rows", "Cluster Rows", TRUE)),
          column(6, checkboxInput("cluster_cols", "Cluster Columns", TRUE))
        ),
        
        # Linkage method - label to the left
        fluidRow(
          column(3, p("Linkage:", style="padding-top: 7px; text-align: right;")),
          column(9, selectInput("clustering_method", NULL,
                  choices = c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median", "centroid"),
                  selected = "average"))
        ),
                   
        # Distance method - label to the left
        fluidRow(
          column(3, p("Distance:", style="padding-top: 7px; text-align: right;")),
          column(9, selectInput("distance_method", NULL,
                   choices = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
                   selected = "pearson"))
        ),
        
        # Color palette - compact
        hr(),
        fluidRow(
          column(3, p("Colors:", style="padding-top: 7px; text-align: right;")),
          column(9, selectInput("color", NULL,
                   choices = c("GreenBlackRed", "RdBu", "RdYlBu", "YlOrRd", 
                               "YlGnBu", "Blues", "Greens", "Purples", "Reds", "OrRd"),
                   selected = "GreenBlackRed"))
        ),
        
        # Font size - more compact
        fluidRow(
          column(3, p("Font:", style="padding-top: 7px; text-align: right;")),
          column(9, sliderInput("fontsize", NULL, min = 5, max = 25, value = 12))
        ),
        
        # Width & Height in more compact form
        fluidRow(
          column(3, p("Width:", style="padding-top: 7px; text-align: right;")),
          column(9, sliderInput("width", NULL, min = 500, max = 2000, value = 600, step = 100))
        ),
        
        fluidRow(
          column(3, p("Height:", style="padding-top: 7px; text-align: right;")),
          column(9, sliderInput("height", NULL, min = 500, max = 2000, value = 600, step = 100))
        ),
        
        # Download buttons
        hr(),
        downloadButton("download_pdf", "PDF"),
        downloadButton("download_png", "PNG")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Heatmap", 
                plotOutput("heatmap", width = "100%", height = "600px")
        ),
        tabPanel("Data Preview", 
                h4("Raw Data Preview"),
                DTOutput("data_preview")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Use the file upload module for main data
  file_data <- file_upload_server("file_upload")
  
  # Use a separate file upload module for the column annotation file
  col_annotation_file_data <- file_upload_server("col_annotation_file_upload")
  
  # Use a separate file upload module for the row annotation file
  row_annotation_file_data <- file_upload_server("row_annotation_file_upload")
  
  # Track if main data is loaded for UI conditionals
  output$data_loaded <- reactive({
    return(file_data$data_loaded())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Reactive to indicate if column annotation file is uploaded
  output$col_annotation_uploaded <- reactive({
    !is.null(col_annotation_file_data$data())
  })
  outputOptions(output, "col_annotation_uploaded", suspendWhenHidden = FALSE)
  
  # Reactive to indicate if row annotation file is uploaded
  output$row_annotation_uploaded <- reactive({
    !is.null(row_annotation_file_data$data())
  })
  outputOptions(output, "row_annotation_uploaded", suspendWhenHidden = FALSE)
  
  # Render UI for column annotation row selection
  output$col_annotation_select_ui <- renderUI({
    req(col_annotation_file_data$data())
    annot_df <- col_annotation_file_data$data()
    row_choices <- rownames(annot_df)
    if (is.null(row_choices) || length(row_choices) == 0) {
      row_choices <- as.character(seq_len(nrow(annot_df)))
    }
    selectInput("col_annotation_select", "Select column annotation rows", 
                choices = row_choices, selected = row_choices[1], multiple = TRUE)
  })
  
  # Render UI for row annotation column selection
  output$row_annotation_select_ui <- renderUI({
    req(row_annotation_file_data$data())
    annot_df <- row_annotation_file_data$data()
    col_choices <- colnames(annot_df)
    if (is.null(col_choices) || length(col_choices) == 0) {
      col_choices <- as.character(seq_len(ncol(annot_df)))
    }
    selectInput("row_annotation_select", "Select row annotation columns", 
                choices = col_choices, selected = col_choices[1], multiple = TRUE)
  })
  
  # Use the transform module
  transform_data <- transform_server("transform", reactive({
    file_data$data()
  }))
  
  # Create a reactive that provides the main data for processing.
  current_data <- reactive({
    if (!transform_data$modal_closed()) {
      return(NULL)  # Do not generate heatmap if the transformation dialog is still open
    }
    
    # Get uploaded main data
    uploaded_data <- file_data$data()
    
    # If transformation has been applied, use that data instead
    if (!is.null(transform_data$processed_data()) && transform_data$has_transformed()) {
      return(transform_data$processed_data())
    } else {
      return(uploaded_data)
    }
  })
  
  # Show transform status
  output$transform_status <- renderUI({
    if (transform_data$has_transformed()) {
      tags$div(
        icon("check-circle"), 
        "Transformed", 
        style = "color: green; margin-top: 7px;"
      )
    } else {
      tags$div(
        icon("info-circle"), 
        "Original", 
        style = "color: grey; margin-top: 7px;"
      )
    }
  })
  
  # Preview of the current data (original or transformed)
  output$data_preview <- renderDT({
    req(current_data())
    datatable(current_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Helper function to convert data to a numeric matrix before creating the heatmap
  prepare_heatmap_data <- function(data) {
    # Check if data is already a matrix
    if (!is.matrix(data)) {
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
  
  # Column annotation for heatmap
  col_annotation_for_heatmap <- reactive({
    req(current_data())
    # If no column annotation file is uploaded, return NULL
    if (is.null(col_annotation_file_data$data())) {
      return(NULL)
    }
    annot_df <- col_annotation_file_data$data()
    main_cols <- colnames(current_data())
    annot_cols <- colnames(annot_df)
    
    # Tolerate extra samples in the annotation file:
    # Proceed as long as all data matrix columns are present in the annotation file.
    if (!all(main_cols %in% annot_cols)) {
      return(NULL)
    }
    # Subset and reorder annotation file columns to match the main data matrix
    annot_df <- annot_df[, main_cols, drop = FALSE]
    
    # Use the selected annotation rows; default to the first if none selected
    selected <- input$col_annotation_select
    if (is.null(selected)) {
      selected <- if (nrow(annot_df) > 0) rownames(annot_df)[1] else NULL
    }
    if (is.null(selected))
      return(NULL)
    
    annot_selected <- annot_df[selected, , drop = FALSE]
    # Transpose so that each row corresponds to a sample (column in main data)
    as.data.frame(t(annot_selected))
  })
  
  # Row annotation for heatmap
  row_annotation_for_heatmap <- reactive({
    req(current_data())
    # If no row annotation file is uploaded, return NULL
    if (is.null(row_annotation_file_data$data())) {
      return(NULL)
    }
    annot_df <- row_annotation_file_data$data()
    main_rows <- rownames(current_data())
    annot_rows <- rownames(annot_df)
    
    # Tolerate extra samples in the annotation file:
    # Proceed as long as all data matrix rows are present in the annotation file.
    if (!all(main_rows %in% annot_rows)) {
      return(NULL)
    }
    # Subset and reorder annotation file rows to match the main data matrix
    annot_df <- annot_df[main_rows, , drop = FALSE]
    
    # Use the selected annotation columns; default to the first if none selected
    selected <- input$row_annotation_select
    if (is.null(selected)) {
      selected <- if (ncol(annot_df) > 0) colnames(annot_df)[1] else NULL
    }
    if (is.null(selected))
      return(NULL)
    
    annot_selected <- annot_df[, selected, drop = FALSE]
    # No need to transpose as it's already in the correct orientation
    as.data.frame(annot_selected)
  })

  heatmap_obj <- reactive({
    req(current_data())
    # Convert the current data to a numeric matrix for the heatmap
    heatmap_data <- prepare_heatmap_data(current_data())
    
    # Determine whether to show row names: only if row names exist and there are fewer than 100 rows
    show_row_names <- !is.null(rownames(heatmap_data)) && nrow(heatmap_data) < 100
    
    # Select the color palette
    if (input$color == "GreenBlackRed") {
      colors <- colorRampPalette(c("green", "black", "red"))(100)
    } else {
      colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
    }
    
    # Prepare clustering parameters
    distance_method <- if (!is.null(input$distance_method)) input$distance_method else "euclidean"
    correlation_method <- "pearson"
    if(distance_method %in% c("pearson", "spearman", "kendall")) {
      correlation_method <- distance_method
      distance_method <- "correlation"
    }
    
    clustering_method <- if (!is.null(input$clustering_method)) input$clustering_method else "complete"
    # Generate the heatmap with conditional row names display
    pheatmap(
      mat = heatmap_data,
      color = colors,
      cluster_rows = input$cluster_rows,
      cluster_cols = input$cluster_cols,
      clustering_method = clustering_method,
      clustering_distance_rows = distance_method,
      clustering_distance_cols = distance_method,
      clustering_distance_cols_fun = if (distance_method == "correlation") {
        function(x) as.dist(1 - cor(x, method = correlation_method, use = "pairwise.complete.obs"))
      } else NULL,
      clustering_distance_rows_fun = if (distance_method == "correlation") {
        function(x) as.dist(1 - cor(t(x), method = correlation_method, use = "pairwise.complete.obs"))
      } else NULL,
      fontsize = input$fontsize,
      annotation_col = col_annotation_for_heatmap(),
      annotation_row = row_annotation_for_heatmap(),
      show_rownames = show_row_names,  # Conditionally display row names
      silent = TRUE
    )
  })

  
  # Generate the heatmap using current data
  output$heatmap <- renderPlot({
    req(heatmap_obj())
    grid::grid.newpage()
    grid::grid.draw(heatmap_obj())
  }, width = function() input$width, height = function() input$height)
  
  # Download handlers using the same heatmap object
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = input$width/72, height = input$height/72)
      grid::grid.newpage()
      grid::grid.draw(heatmap_obj())
      dev.off()
    }
  )
  
  output$download_png <- downloadHandler(
    filename = function() {
      paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, res = 72)
      grid::grid.newpage()
      grid::grid.draw(heatmap_obj())
      dev.off()
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)