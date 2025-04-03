library(shiny)
library(pheatmap)
library(RColorBrewer)
library(readxl)  # file upload module
library(e1071)   # transform module (skewness calculation)
library(grid)    # needed for grid.draw

source("mod_file_upload.R")
source("mod_transform.R")
source("utilities.R")

ui <- fluidPage(

  titlePanel("DataMap"),  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Button to open file upload modal
      actionButton("show_upload_modal", "Upload Files", 
                  icon = icon("upload"), 
                  style = "width: 100%; margin-bottom: 15px;"),
      
      # Dynamic UI for selecting column annotation rows
      conditionalPanel(
        condition = "output.col_annotation_uploaded",
        uiOutput("col_annotation_select_ui")
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
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Show the modal when the button is clicked
  observeEvent(input$show_upload_modal, {
    showModal(modalDialog(
      title = "Upload Files",
      
      # Main data file upload
      h4("Upload file:"),
      file_upload_ui("file_upload"),
      
      # Column annotation file upload widget
      tags$div(
        tags$h4("Column Annotations", style="margin-top: 15px;"),
        file_upload_ui("col_annotation_file_upload")
      ),
      
      # Row annotation file upload widget
      tags$div(
        tags$h4("Row Annotations", style="margin-top: 15px;"),
        file_upload_ui("row_annotation_file_upload")
      ),
      
      footer = tagList(
        modalButton("Close")
      ),
      size = "s",
      easyClose = TRUE
    ))
  })
  
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
  
  initial_loading <- reactiveVal(TRUE)
  # Create a reactive that provides the main data for processing.
  current_data <- reactive({
    # If there's no data uploaded yet, return NULL
    if (is.null(file_data$data())) {
      return(NULL)
    }
    
    # During initial loading, don't show heatmap if transform dialog is open
    if (initial_loading() && !transform_data$modal_closed()) {
      return(NULL)
    }
    
    # Once transformation is applied for the first time, mark initial loading as complete
    if (transform_data$has_transformed()) {
      initial_loading(FALSE)
    }
    
    # Return appropriate data based on transformation state
    if (!is.null(transform_data$processed_data()) && transform_data$has_transformed()) {
      return(transform_data$processed_data())
    } else {
      return(file_data$data())
    }
  })
  
  # Show transform status
  output$transform_status <- renderUI({
    if (transform_data$has_transformed()) {
      tags$div(
        icon("check-circle"), 
        style = "color: green; margin-top: 7px;"
      )
    } else {
      tags$div(
        icon("info-circle"), 
        style = "color: grey; margin-top: 7px;"
      )
    }
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
  
   auto_row_annotation <- reactive({
    # Return the factor columns identified during data transformation
    transform_data$factor_columns()
  })

  row_annotation_for_heatmap <- reactive({
    req(current_data())
    
    # First try to use uploaded row annotation file
    if (!is.null(row_annotation_file_data$data())) {
      annot_df <- row_annotation_file_data$data()
      main_rows <- rownames(current_data())
      annot_rows <- rownames(annot_df)
      
      # Proceed as long as all data matrix rows are present in the annotation file
      if (!all(main_rows %in% annot_rows)) {
        return(NULL)
      }
      
      # Subset and reorder annotation file rows to match the main data matrix
      annot_df <- annot_df[main_rows, , drop = FALSE]
      
      # Use selected annotation columns
      selected <- input$row_annotation_select
      if (is.null(selected)) {
        selected <- if (ncol(annot_df) > 0) colnames(annot_df)[1] else NULL
      }
      if (is.null(selected))
        return(NULL)
      
      return(as.data.frame(annot_df[, selected, drop = FALSE]))
    }
    
    # If no row annotation file, try auto-detected factor columns
    if (!is.null(auto_row_annotation())) {
      annot_df <- auto_row_annotation()
      main_rows <- rownames(current_data())
      
      # Make sure row names match
      if (!all(main_rows %in% rownames(annot_df))) {
        return(NULL)
      }
      
      # Subset and reorder rows to match main data
      annot_df <- annot_df[main_rows, , drop = FALSE]
      
      # Use all detected factor columns automatically
      return(as.data.frame(annot_df))
    }
    
    # If neither source is available, return NULL
    return(NULL)
  })

  heatmap_obj <- reactive({
    req(current_data())

    withProgress(message = 'Generating heatmap', value = 0, {
      # Convert the current data to a numeric matrix for the heatmap
      incProgress(0.1, detail = "Preparing data")
      heatmap_data <- prepare_heatmap_data(current_data())
      
      # Check for and handle zero standard deviation rows/columns
      incProgress(0.1, detail = "Checking data variance")
      row_sds <- apply(heatmap_data, 1, sd, na.rm = TRUE)
      col_sds <- apply(heatmap_data, 2, sd, na.rm = TRUE)
      
      # Identify rows and columns with zero or NA standard deviation
      zero_sd_rows <- which(row_sds == 0 | is.na(row_sds))
      zero_sd_cols <- which(col_sds == 0 | is.na(col_sds))
      
      # Add small random noise to zero SD rows/columns
      incProgress(0.1, detail = "Handling zero variance data")
      if(length(zero_sd_rows) > 0) {
        for(i in zero_sd_rows) {
          # Add tiny random noise (won't affect visualization but prevents clustering errors)
          heatmap_data[i,] <- heatmap_data[i,] + rnorm(ncol(heatmap_data), 0, 1e-10)
        }
      }
      
      if(length(zero_sd_cols) > 0) {
        for(j in zero_sd_cols) {
          # Add tiny random noise
          heatmap_data[,j] <- heatmap_data[,j] + rnorm(nrow(heatmap_data), 0, 1e-10)
        }
      }
      
      # Determine whether to show row names: only if row names exist and there are fewer than 100 rows
      show_row_names <- !is.null(rownames(heatmap_data)) && nrow(heatmap_data) < 100
      
      # Select the color palette
      incProgress(0.1, detail = "Setting up color palette")
      if (input$color == "GreenBlackRed") {
        colors <- colorRampPalette(c("green", "black", "red"))(100)
      } else {
        colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
      }
      
      # Prepare clustering parameters
      incProgress(0.1, detail = "Configuring clustering parameters")
      distance_method <- if (!is.null(input$distance_method)) input$distance_method else "euclidean"
      correlation_method <- "pearson"
      if(distance_method %in% c("pearson", "spearman", "kendall")) {
        correlation_method <- distance_method
        distance_method <- "correlation"
      }
      
      clustering_method <- if (!is.null(input$clustering_method)) input$clustering_method else "complete"
      
      # Create custom distance functions that handle NAs and zero SDs gracefully
      incProgress(0.1, detail = "Setting up distance functions")
      custom_cor_cols <- function(x) {
        cors <- cor(x, method = correlation_method, use = "pairwise.complete.obs")
        # Replace NAs with 0 correlations
        cors[is.na(cors)] <- 0
        as.dist(1 - cors)
      }
      
      custom_cor_rows <- function(x) {
        cors <- cor(t(x), method = correlation_method, use = "pairwise.complete.obs")
        # Replace NAs with 0 correlations
        cors[is.na(cors)] <- 0
        as.dist(1 - cors)
      }
      # Try to generate the heatmap with error handling
      incProgress(0.3, detail = "Rendering heatmap")
      tryCatch({
        pheatmap(
          mat = heatmap_data,
          color = colors,
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols,
          clustering_method = clustering_method,
          clustering_distance_rows = distance_method,
          clustering_distance_cols = distance_method,
          clustering_distance_cols_fun = if (distance_method == "correlation") custom_cor_cols else NULL,
          clustering_distance_rows_fun = if (distance_method == "correlation") custom_cor_rows else NULL,
          fontsize = input$fontsize,
          annotation_col = col_annotation_for_heatmap(),
          annotation_row = row_annotation_for_heatmap(),
          show_rownames = show_row_names,
          silent = TRUE
        )
      }, error = function(e) {
        # If correlation-based distances fail, fall back to euclidean
        incProgress(0.1, detail = "Clustering error, falling back to euclidean distance")
        message("Clustering error: ", e$message, ". Falling back to euclidean distance.")
        pheatmap(
          mat = heatmap_data,
          color = colors,
          cluster_rows = input$cluster_rows,
          cluster_cols = input$cluster_cols,
          clustering_method = clustering_method,
          clustering_distance_rows = "euclidean",
          clustering_distance_cols = "euclidean",
          fontsize = input$fontsize,
          annotation_col = col_annotation_for_heatmap(),
          annotation_row = row_annotation_for_heatmap(),
          show_rownames = show_row_names,
          silent = TRUE
        )
      })
    })
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