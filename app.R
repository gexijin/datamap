# DataMap: A Shiny app for visualizing data matrices with heatmaps, PCA, and t-SNE
# by Steven Ge 4/5/2025  
library(shiny)

source("R/mod_file_upload.R")
source("R/mod_transform.R")
source("R/utilities.R")

max_rows_to_show <- 1000  # Maximum number of rows to show row names in the heatmap
default_width <- 600
default_height <- 600
# this solves the issue of the download button not working from Chromium when this app is deployed as Shinylive
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}
ui <- fluidPage(

  titlePanel("DataMap: a portable app for visualizing data matrices"),  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Place Upload Files and Reset buttons on the same row
      fluidRow(
        column(7, 
          actionButton("show_upload_modal", "Data Files", 
                      icon = icon("upload"))
        ),
        column(5,
          conditionalPanel(
            condition = "output.data_loaded",
            actionButton("reset_session", "Reset", 
                        icon = icon("refresh"), 
                        style = "background-color: #f8d7da; color: #721c24;")
          )
        )
      ),

      conditionalPanel(
        condition = "output.data_loaded",
        fluidRow(
          column(8, transform_ui("transform")),
          column(4, uiOutput("transform_status"))
        )
      ), 

      # Dynamic UI for selecting row annotation columns
      conditionalPanel(
        condition = "output.row_annotation_available",
        uiOutput("row_annotation_select_ui")
      ),
      conditionalPanel(
        condition = "output.col_annotation_uploaded",
        uiOutput("col_annotation_select_ui")
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
          column(9, sliderInput("width", NULL, min = 200, max = 2000, value = default_width, step = 100))
        ),
        
        fluidRow(
          column(3, p("Height:", style="padding-top: 7px; text-align: right;")),
          column(9, sliderInput("height", NULL, min = 200, max = 2000, value = default_height, step = 100))
        ),
        fluidRow(
          column(6, checkboxInput("label_heatmap", "Label Data", value = FALSE)),
          column(6, checkboxInput("show_row_names", "Row Names", value = FALSE))
        ),
        fluidRow(
          column(6, 
            numericInput("cutree_rows", "Row clusters", value = 1, min = 1, max = 100, step = 1),
          ),
          column(6, 
            numericInput("cutree_cols", "Col. Clusters", value = 1, min = 1, max = 100, step = 1),
          )
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs", selected = "About",
        tabPanel("Heatmap", 
                plotOutput("heatmap", width = "100%", height = "600px"),
                downloadButton("download_pdf", "PDF"),
                downloadButton("download_png", "PNG")
        ),
        tabPanel("PCA",
          selectInput("pca_transpose", "PCA Analysis Mode:",
            choices = c("Row vectors" = "row", 
            "Column vectors" = "column"),
              selected = "row"),
          plotOutput("pca_plot", width = "100%", height = "auto"),
          downloadButton("download_pca_pdf", "PDF"),
          downloadButton("download_pca_png", "PNG")
        ),
        tabPanel("t-SNE",
          # Single row with 4 elements
          fluidRow(
            column(3, selectInput("tsne_transpose", "Analysis Mode:",
              choices = c("Row vectors" = "row", "Column vectors" = "column"),
              selected = "row")),
            column(3, sliderInput("tsne_perplexity", "Perplexity:", 
              min = 5, max = 50, value = 30, step = 5)),
            column(3, sliderInput("tsne_early_exaggeration", "Early Exagg.:", 
              min = 4, max = 20, value = 12, step = 1)),
            column(3, sliderInput("tsne_learning_rate", "Learning Rate:", 
              min = 50, max = 1000, value = 200, step = 50))
          ),
          # Second row with 3 elements and a checkbox
          fluidRow(
            column(3, sliderInput("tsne_iterations", "Max Iterations:", 
              min = 500, max = 2000, value = 1000, step = 100)),
            column(3, checkboxInput("tsne_pca_preprocessing", "Use PCA Preprocessing", value = FALSE)),
            column(6,
              tags$div(
                style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                tags$p(
                  style = "margin-bottom: 0; font-size: 0.85em; color: #495057;",
                  "Increase Early Exaggeration (12-20) for more distinct clusters. Perplexity balances local (5-10) vs global (30-50) structure. Higher Learning Rate can improve separation but may cause instability. More Iterations allow for better optimization and clearer boundaries. PCA preprocessing can reduce noise."
                )
              )
            )
          ),
          plotOutput("tsne_plot", width = "100%", height = "auto"),
          downloadButton("download_tsne_pdf", "PDF"),
          downloadButton("download_tsne_png", "PNG")

        ),
        tabPanel("Code",
                uiOutput("code_display")
        ),
        tabPanel("About",
                img(src = "heatmap.png", width = "400px", height = "400px"),
                #img(src = "pca.png", width = "433px", height = "387px"),
                includeHTML("www/help.html")
        )
      ),
      tags$head(includeHTML("www/google_analytics.html"))
    )
  )
)

server <- function(input, output, session) {

  # Track if main data is loaded for UI conditionals
  output$data_loaded <- reactive({
    return(file_data$data_loaded())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Switch to Heatmap tab when data is loaded
  observe({
    if(file_data$data_loaded()) {
      updateTabsetPanel(session, "main_tabs", selected = "Heatmap")
    }
  })

  # Show the modal when the button is clicked
  output$main_file_upload_ui <- renderUI({
    if (!is.null(file_data$data())) {
      # If data is already uploaded
      tags$div(
        style = "padding: 15px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
        tags$p(
          icon("info-circle"), 
          "Reset the app to upload a new data matrix.", 
          style = "margin: 0; color: #495057;"
        )
      )
    } else {
      tags$div(
        tags$h4("Data file (Excel, CSV, ...)"),
        file_upload_ui("file_upload"),
        downloadButton("download_example_genomics", "Example 1", style = "margin-top: -15px;"),
        downloadButton("download_example", "Example 2", style = "margin-top: -15px;")
      )
    }
  })

  observeEvent(input$show_upload_modal, {
    showModal(modalDialog(
      title = "Upload Files",
      
      uiOutput("main_file_upload_ui"),
      
      hr(),
      tags$div(
        tags$h4("Optional: Column Annotation"),
        file_upload_ui("col_annotation_file_upload"),
        downloadButton("download_example_col_genomics", "Example 1", style = "margin-top: -15px;"),
        downloadButton("download_example_col", "Example 2", style = "margin-top: -15px;")
      ),
      hr(),
      tags$div(
        tags$h4("Optional: Row Annotations"),
        file_upload_ui("row_annotation_file_upload"),
        downloadButton("download_example_row", "Example", style = "margin-top: -15px;")
      ),
      
      footer = tagList(
        modalButton("Close")
      ),
      size = "s",
      easyClose = TRUE
    ))
  })

  output$download_example <- downloadHandler(
    filename = function() {
      "example data.csv"
    },
    content = function(file) {
      file.copy("data/iris.csv", file)
    }
  )
  output$download_example_genomics <- downloadHandler(
    filename = function() {
      "example gene expression.csv"
    },
    content = function(file) {
      file.copy("data/Genomics.csv", file)
    }
  )
  output$download_example_col_genomics <- downloadHandler(
    filename = function() {
      "example experiment design.csv"
    },
    content = function(file) {
      file.copy("data/experiment_design.csv", file)
    }
  )
  output$download_example_col <- downloadHandler(
    filename = function() {
      "example column annotation.csv"
    },
    content = function(file) {
      file.copy("data/iris_column.csv", file)
    }
  )
  output$download_example_row <- downloadHandler(
    filename = function() {
      "example gene info.csv"
    },
    content = function(file) {
      file.copy("data/Gene_info.csv", file)
    }
  )
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
  
  output$row_annotation_available <- reactive({
    has_uploaded <- !is.null(row_annotation_file_data$data())
    has_factors <- !is.null(transform_data$factor_columns()) && 
                  ncol(transform_data$factor_columns()) > 0
    return(has_uploaded || has_factors)
  })
  outputOptions(output, "row_annotation_available", suspendWhenHidden = FALSE)
  
  # Render UI for column annotation row selection
  output$col_annotation_select_ui <- renderUI({
    req(col_annotation_file_data$data())
    annot_df <- col_annotation_file_data$data()
    row_choices <- rownames(annot_df)
    if (is.null(row_choices) || length(row_choices) == 0) {
      row_choices <- as.character(seq_len(nrow(annot_df)))
    }
    selectInput("col_annotation_select", "Column annotation:", 
                choices = row_choices, selected = row_choices[1], multiple = TRUE)
  })

  # Render UI for row annotation column selection (merged from both sources)
  output$row_annotation_select_ui <- renderUI({
    all_choices <- c()
    default_selected <- c()
    
    # Get choices from uploaded row annotation file
    if (!is.null(row_annotation_file_data$data())) {
      uploaded_choices <- colnames(row_annotation_file_data$data())
      if (!is.null(uploaded_choices) && length(uploaded_choices) > 0) {
        all_choices <- c(all_choices, uploaded_choices)
        # Select first uploaded column by default
        default_selected <- c(default_selected, uploaded_choices[1])
      }
    }
    
    # Get choices from auto-detected factor columns
    if (!is.null(transform_data$factor_columns()) && 
        ncol(transform_data$factor_columns()) > 0) {
      factor_choices <- colnames(transform_data$factor_columns())
      if (!is.null(factor_choices) && length(factor_choices) > 0) {
        all_choices <- c(all_choices, factor_choices)
        # Select all factor columns by default
        default_selected <- c(default_selected, factor_choices)
      }
    }
    
    # Remove any duplicates
    all_choices <- unique(all_choices)
    default_selected <- unique(default_selected)
    
    # Create the selectInput if we have any choices
    if (length(all_choices) > 0) {
      selectInput("row_annotation_select", "Row annotations:", 
                  choices = all_choices, selected = default_selected, multiple = TRUE)
    } else {
      return(NULL)
    }
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
    
    # Use the selected annotation rows
    selected <- input$col_annotation_select
    
    # If nothing selected (either NULL or length 0), return NULL
    if (is.null(selected) || length(selected) == 0) {
      return(NULL)
    }
    
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
    
    # Get selected annotations
    selected <- input$row_annotation_select
    if (is.null(selected) || length(selected) == 0) {
      return(NULL)
    }
    
    # Create empty data frame to store combined annotations
    main_rows <- rownames(current_data())
    combined_annot <- data.frame(row.names = main_rows)
    added_columns <- c()
    
    # First try to add columns from uploaded row annotation file
    if (!is.null(row_annotation_file_data$data())) {
      annot_df <- row_annotation_file_data$data()
      
      # Only proceed if all main rows are in the annotation file
      if (all(main_rows %in% rownames(annot_df))) {
        # Subset and reorder to match main data
        annot_df <- annot_df[main_rows, , drop = FALSE]
        
        # Find selected columns that exist in the file
        file_cols <- intersect(selected, colnames(annot_df))
        
        if (length(file_cols) > 0) {
          # Add selected columns from file
          combined_annot <- cbind(combined_annot, annot_df[, file_cols, drop = FALSE])
          added_columns <- c(added_columns, file_cols)
        }
      }
    }
    
    # Next try to add columns from auto-detected factors
    if (!is.null(transform_data$factor_columns()) && 
        ncol(transform_data$factor_columns()) > 0) {
      factor_df <- transform_data$factor_columns()
      
      # Only proceed if all main rows are in the factor data
      if (all(main_rows %in% rownames(factor_df))) {
        # Subset and reorder to match main data
        factor_df <- factor_df[main_rows, , drop = FALSE]
        
        # Find selected columns that exist in factors (and aren't already added)
        factor_cols <- setdiff(intersect(selected, colnames(factor_df)), added_columns)
        
        if (length(factor_cols) > 0) {
          # Add selected columns from factors
          combined_annot <- cbind(combined_annot, factor_df[, factor_cols, drop = FALSE])
        }
      }
    }
    
    # Return NULL if no annotations were added
    if (ncol(combined_annot) == 0) {
      return(NULL)
    }
    
    return(combined_annot)
  })

  # Create a reactive to store and access the actual pheatmap parameters
  pheatmap_params_used <- reactiveVal(NULL)
  
  # Modify the heatmap_obj reactive function to correctly handle distance objects:
  heatmap_obj <- reactive({
    req(transform_data$processed_data())

    withProgress(message = 'Generating heatmap', value = 0, {
      # Convert the current data to a numeric matrix for the heatmap
      incProgress(0.1, detail = "Preparing data")
      heatmap_data <- transform_data$processed_data()
     
      show_row_names <- file_data$has_rownames() && !is.null(rownames(heatmap_data))
      # Use the user's input if available; otherwise, use the default
      if(show_row_names && !is.null(input$show_row_names)) {
        show_row_names <- input$show_row_names
      }

      # Select the color palette
      if (input$color == "GreenBlackRed") {
        colors <- colorRampPalette(c("green", "black", "red"))(100)
      } else {
        colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, input$color)))(100)
      }
      
      # Prepare clustering parameters
      distance_method <- if (!is.null(input$distance_method)) input$distance_method else "euclidean"
      correlation_method <- "pearson"
      
      # Flag to determine if we're using correlation-based distance
      using_correlation <- FALSE
      
      if(distance_method %in% c("pearson", "spearman", "kendall")) {
        correlation_method <- distance_method
        using_correlation <- TRUE
      }
      
      clustering_method <- if (!is.null(input$clustering_method)) input$clustering_method else "complete"
      
      # x is a matrix of data
      # Returns a distance object for columns
      # to get row distances, transpose the matrix before calling this function
      custom_cor <- function(x) {
        cors <- withCallingHandlers(
          tryCatch(
            cor(x, method = correlation_method, use = "pairwise.complete.obs"),
            error = function(e) {
              showNotification(paste("Error in cor:", conditionMessage(e)), type = "error")
              return(diag(nrow(x)))
            }
          ),
          warning = function(w) {
            showNotification(paste("Warning in cor:", conditionMessage(w)), type = "warning")
            invokeRestart("muffleWarning")
          }
        )
        # Replace NAs with 0 correlations
        cors[is.na(cors)] <- 0
        as.dist(1 - cors)
      }

      # Try to generate the heatmap with error handling
      incProgress(0.3, detail = "Rendering heatmap")
      tryCatch({
        if (using_correlation) {
            # Calculate the distance matrices for rows and columns
            dist_rows <- NULL
            if (input$cluster_rows) {
              dist_rows <- custom_cor(t(heatmap_data))
            }
            dist_cols <- NULL
            if (input$cluster_cols) {
              dist_cols <- custom_cor(heatmap_data)
            }
            
            # IMPORTANT FIX: Create a copy of the parameters for display without the distance objects
            display_params <- list(
              mat = heatmap_data,
              color = colors,
              cluster_rows = input$cluster_rows,
              cluster_cols = input$cluster_cols,
              clustering_method = clustering_method,
              fontsize = input$fontsize,
              annotation_col = col_annotation_for_heatmap(),
              annotation_row = row_annotation_for_heatmap(),
              show_rownames = show_row_names,
              silent = TRUE,
              display_numbers = if (input$label_heatmap) round(as.matrix(current_data()), 2) else FALSE
            )
            
            # Store parameters for code generation without the distance objects
            # Instead, store the method name to recreate them later
            if (input$cluster_rows) {
              display_params$clustering_distance_rows = correlation_method
            }
            if (input$cluster_cols) {
              display_params$clustering_distance_cols = correlation_method
            }

            # Only add cutree parameters if clustering is enabled and value > 0
            if(!is.na(input$cutree_rows)) { # when user delete the number in the input box, it will be NA
              if (input$cluster_rows && input$cutree_rows > 1 && input$cutree_rows <= nrow(heatmap_data)) {
                display_params$cutree_rows <- input$cutree_rows
              }
            }
            if(!is.na(input$cutree_cols)) {
              if (input$cluster_cols && input$cutree_cols > 1 && input$cutree_cols <= ncol(heatmap_data)) {
                display_params$cutree_cols <- input$cutree_cols
              }
            }

            # Store the parameters for code generation
            pheatmap_params_used(display_params)
            
            # Create the actual parameters with the distance objects for rendering
            render_params <- display_params
            if (input$cluster_rows) {
              render_params$clustering_distance_rows <- dist_rows
            }
            if (input$cluster_cols) {
              render_params$clustering_distance_cols <- dist_cols
            }
            
            # Call pheatmap with the parameter list that includes the distance objects
            do.call(pheatmap::pheatmap, render_params)
        } else {
          # For non-correlation methods, use the standard distance_method
            pheatmap_params <- list(
            mat = heatmap_data,
            color = colors,
            cluster_rows = input$cluster_rows,
            cluster_cols = input$cluster_cols,
            clustering_method = clustering_method,
            clustering_distance_rows = distance_method,
            clustering_distance_cols = distance_method,
            fontsize = input$fontsize,
            annotation_col = col_annotation_for_heatmap(),
            annotation_row = row_annotation_for_heatmap(),
            show_rownames = show_row_names,
            silent = TRUE,
            display_numbers = if (input$label_heatmap) round(as.matrix(current_data()), 2) else FALSE
            )
            
            # Only add cutree parameters if clustering is enabled and value > 0
            if(!is.null(input$cutree_rows)) {
              if (input$cluster_rows && input$cutree_rows > 1 && input$cutree_rows <= nrow(heatmap_data)) {
                pheatmap_params$cutree_rows <- input$cutree_rows
              }
            }

            if(!is.null(input$cutree_cols)) {
              if (input$cluster_cols && input$cutree_cols > 1 && input$cutree_cols <= ncol(heatmap_data)) {
                pheatmap_params$cutree_cols <- input$cutree_cols
              }
            }
            
            # Store the parameters for code generation
            pheatmap_params_used(pheatmap_params)
            
            do.call(pheatmap::pheatmap, pheatmap_params)
        }
      }, error = function(e) {
        # If any clustering fails, fall back to euclidean
        incProgress(0.1, detail = "Clustering error, falling back to euclidean distance")
        message("Clustering error: ", e$message, ". Falling back to euclidean distance.")
        fallback_params <- list(
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
        
        # Store the fallback parameters
        pheatmap_params_used(fallback_params)
        
        pheatmap::pheatmap(
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

  observe({
    req(current_data())
    heatmap_data <- current_data()
    # Compute default: TRUE if the data has row names and row count is less than max_rows_to_show
    default_show <- file_data$has_rownames() && !is.null(rownames(heatmap_data)) && nrow(heatmap_data) < max_rows_to_show
    updateCheckboxInput(session, "show_row_names", value = default_show)
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

  observeEvent(input$reset_session, {
    # Create a modal dialog asking for confirmation
    showModal(modalDialog(
      title = "Confirm Reset",
      "This will clear all loaded data and reset the application to its initial state. Continue?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Reset", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  # Handle confirmation
  observeEvent(input$confirm_reset, {
    # Close the confirmation dialog
    removeModal()
    
    # Reload the page to reset the session
    session$reload()
  })



  # Create a reactive expression for generating the full code
  full_code <- reactive({
    # Initialize an empty vector to store code parts
    code_parts <- c()
    
    # Add file upload code if available
    if (!is.null(file_data$code())) {
      code_parts <- c(code_parts, "# Data Import Code", file_data$code(), "")
    }
    
    # Add transform code if available
    if (!is.null(transform_data$code())) {
      code_parts <- c(code_parts, "# Data Transformation Code", transform_data$code(), "")
    }
    
    # Generate heatmap code using the actual parameters that were used
    if (!is.null(current_data()) && !is.null(pheatmap_params_used())) {
      # Extract the params that were used
      params <- pheatmap_params_used()
      
      heatmap_code <- c(
        "# Heatmap Generation Code",
        "library(pheatmap)",
        "library(RColorBrewer)",
        "library(grid)",
        "",
        "# Save the processed data (adjust this path as needed)",
        "processed_data <- transformed_data$numeric_data",
        ""
      )
      
      # Add color palette code
      if (!is.null(params$color)) {
        if (identical(params$color, colorRampPalette(c("green", "black", "red"))(100))) {
          heatmap_code <- c(heatmap_code, "# Define color palette",
                            "colors <- colorRampPalette(c(\"green\", \"black\", \"red\"))(100)")
        } else {
          # Determine which palette was used
          for (palette_name in c("RdBu", "RdYlBu", "YlOrRd", "YlGnBu", "Blues", "Greens", "Purples", "Reds", "OrRd")) {
            if (identical(params$color, colorRampPalette(rev(RColorBrewer::brewer.pal(11, palette_name)))(100))) {
              heatmap_code <- c(heatmap_code, "# Define color palette",
                                paste0("colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, \"", palette_name, "\")))(100)"))
              break
            }
          }
        }
      }
      params$silent <- FALSE # otherwise pheatmap will not show
      
      # Check if we need to add custom correlation functions
      needs_custom_cor <- FALSE
      
      if (!is.null(params$clustering_distance_rows)) {
        if (params$clustering_distance_rows %in% c("pearson", "spearman", "kendall")) {
          needs_custom_cor <- TRUE
        }
      }
      
      if (!is.null(params$clustering_distance_cols)) {
        if (params$clustering_distance_cols %in% c("pearson", "spearman", "kendall")) {
          needs_custom_cor <- TRUE
        }
      }
      
      # If correlation methods are used, add the custom correlation function
      if (needs_custom_cor) {
        heatmap_code <- c(heatmap_code, "", 
                        "# Custom correlation function for distance calculation",
                        "custom_cor <- function(x, method = \"pearson\") {",
                        "  cors <- cor(x, method = method, use = \"pairwise.complete.obs\")",
                        "  # Replace NAs with 0 correlations",
                        "  cors[is.na(cors)] <- 0",
                        "  as.dist(1 - cors)",
                        "}")
      }
      
      # Start building pheatmap code
      pheatmap_call <- "pheatmap(\n  processed_data"
      
      # Add all the non-complex parameters
      simple_params <- c(
        "cluster_rows", "cluster_cols", "clustering_method", 
        "fontsize", "show_rownames", "silent", "cutree_rows", "cutree_cols"
      )
      
      for (param in simple_params) {
        if (!is.null(params[[param]])) {
          # Format the value based on its type
          if (is.logical(params[[param]])) {
            value <- ifelse(params[[param]], "TRUE", "FALSE")
          } else if (is.character(params[[param]])) {
            value <- paste0("\"", params[[param]], "\"")
          } else {
            value <- as.character(params[[param]])
          }
          
          pheatmap_call <- paste0(pheatmap_call, ",\n  ", param, " = ", value)
        }
      }
      
      # Handle distance methods correctly
      if (!is.null(params$clustering_distance_rows)) {
        if (params$clustering_distance_rows %in% c("pearson", "spearman", "kendall")) {
          # For correlation methods, use the custom_cor function
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = custom_cor(t(processed_data), method = \"", 
                                params$clustering_distance_rows, "\")")
        } else {
          # For standard distance methods, use the string
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = \"", params$clustering_distance_rows, "\"")
        }
      }
      
      if (!is.null(params$clustering_distance_cols)) {
        if (params$clustering_distance_cols %in% c("pearson", "spearman", "kendall")) {
          # For correlation methods, use the custom_cor function
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = custom_cor(processed_data, method = \"", 
                                params$clustering_distance_cols, "\")")
        } else {
          # For standard distance methods, use the string
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = \"", params$clustering_distance_cols, "\"")
        }
      }
      
      # Handle annotation data
      if (!is.null(params$annotation_col)) {
        heatmap_code <- c(heatmap_code, "",
                        "# Load column annotation data (adjust this path as needed)",
                        "col_annotation <- read.csv('your_column_annotation.csv')",
                        "# Make sure rownames match column names in the main data",
                        "rownames(col_annotation) <- col_annotation$column_id",
                        "col_annotation$column_id <- NULL")
        pheatmap_call <- paste0(pheatmap_call, ",\n  annotation_col = col_annotation")
      }
      
      if (!is.null(params$annotation_row)) {
        heatmap_code <- c(heatmap_code, "\n",
                        "row_annotation <- NULL")
        pheatmap_call <- paste0(pheatmap_call, ",\n  annotation_row = row_annotation")
      }
      
      # Handle display_numbers if it was used
      if (!is.null(params$display_numbers)) {
        if (is.logical(params$display_numbers)) {
          if (params$display_numbers) {
            pheatmap_call <- paste0(pheatmap_call, ",\n  display_numbers = TRUE")
          }
        } else {
          # If display_numbers contains a matrix of values, use round() on the data matrix
          pheatmap_call <- paste0(pheatmap_call, ",\n  display_numbers = round(as.matrix(processed_data), 2)")
        }
      }
      
      # Add colors
      pheatmap_call <- paste0(pheatmap_call, ",\n  color = colors")
      
      # Close the pheatmap call
      pheatmap_call <- paste0(pheatmap_call, "\n)")
      
      # Complete the heatmap code
      heatmap_code <- c(heatmap_code, "", "# Generate the heatmap", pheatmap_call)

      
      code_parts <- c(code_parts, heatmap_code)


      # Add PCA code if used
      pca_code <- c(
        "# PCA Analysis Code",
        "library(stats)",
        "",
        "# Calculate PCA",
        "pca_result <- prcomp(processed_data, center = TRUE, scale. = TRUE)",
        "",
        "# Extract the first two principal components",
        "pc_data <- as.data.frame(pca_result$x[, 1:2])",
        ""
      )
      
      # Add basic plot code
      pca_code <- c(pca_code,
        "# Create a PCA plot",
        "# Set margins to make room for legend",
        "par(mar = c(5, 5, 2, 8) + 0.1)",
        "plot(pc_data$PC1, pc_data$PC2,",
        "     xlab = paste0(\"PC1 (\", round(summary(pca_result)$importance[2, 1] * 100, 1), \"%)\"),",
        "     ylab = paste0(\"PC2 (\", round(summary(pca_result)$importance[2, 2] * 100, 1), \"%)\"),",
        "     main = \"\",",
        "     pch = 16, col = \"black\")",
        ""
      )
      
      # Add row annotation code if used
      if (!is.null(row_annotation_for_heatmap())) {
        pca_code <- c(pca_code,
        "",
        "# Use row annotations for point colors and shapes",
        "if (!is.null(row_annotation)) {",
        "  # Color by first annotation column",
        "  color_col <- names(row_annotation)[1]",
        "  color_factor <- as.factor(row_annotation[[color_col]])",
        "  color_palette <- rainbow(length(levels(color_factor)))",
        "  point_colors <- color_palette[as.numeric(color_factor)]",
        "",
        "  # If there's a second annotation column, use for shapes",
        "  if (ncol(row_annotation) >= 2) {",
        "    shape_col <- names(row_annotation)[2]",
        "    shape_factor <- as.factor(row_annotation[[shape_col]])",
        "    available_shapes <- c(16, 17, 15, 18, 19, 1, 2, 5, 6, 8)",
        "    shape_numbers <- available_shapes[1:min(length(available_shapes), length(levels(shape_factor)))]",
        "    point_shapes <- shape_numbers[as.numeric(shape_factor)]",
        "  } else {",
        "    point_shapes <- 16  # Default circle",
        "  }",
        "",
        "  # Replot with annotations",
        "  plot(pc_data$PC1, pc_data$PC2,",
        "       xlab = paste0(\"PC1 (\", round(summary(pca_result)$importance[2, 1] * 100, 1), \"%)\"),",
        "       ylab = paste0(\"PC2 (\", round(summary(pca_result)$importance[2, 2] * 100, 1), \"%)\"),",
        "       main = \"\",",
        "       pch = point_shapes,",
        "       col = point_colors,",
        "       cex = fontsize/12,",     
        "       cex.lab = fontsize/12,", 
        "       cex.axis = fontsize/12)",
        "",
        "  # Add reference lines",
        "  abline(h = 0, lty = 2, col = \"gray50\")",
        "  abline(v = 0, lty = 2, col = \"gray50\")",
        "",
        "  # Add legends",
        "  par(xpd = TRUE)  # Allow plotting outside plot region",
        "  # Color legend",
        "  legend(\"topright\", ",
        "         legend = levels(color_factor),",
        "         fill = color_palette,",
        "         title = color_col,",
        "         cex = fontsize/15,",
        "         inset = c(-0.15, 0),",
        "         bty = \"n\")",
        "",
        "  # Shape legend if applicable",
        "  if (ncol(row_annotation) >= 2) {",
        "    legend(\"topright\", ",
        "           legend = levels(shape_factor),",
        "           pch = shape_numbers,",
        "           title = shape_col,",
        "           cex = 0.8,",
        "           inset = c(-0.15, 0.3),",
        "           bty = \"n\")",
        "  }",
        "  par(xpd = FALSE)",
        "}"
        )
      }

    }
    
    # Combine all parts and return
    paste(code_parts, collapse = "\n")
  })

  output$code_display <- renderUI({
    req(full_code())
    
    code_text <- full_code()
    
    # Split the code into sections for better display
    sections <- strsplit(code_text, "# ")[[1]]
    
    # Initialize HTML output
    html_output <- tagList()
    
    # Process each section
    current_section <- NULL
    section_content <- NULL
    
    for (section in sections) {
      if (nchar(section) == 0) next
      
      # First line is likely a section title
      lines <- strsplit(section, "\n")[[1]]
      section_title <- lines[1]
      
      # If it's a proper section title
      if (section_title %in% c("Data Import Code", "Data Transformation Code", "Heatmap Generation Code")) {
        # If we were building a previous section, add it to output
        if (!is.null(current_section) && !is.null(section_content)) {
          html_output <- tagAppendChild(html_output, 
                                      tags$div(
                                        tags$h3(current_section, class = "code-section-title"),
                                        tags$pre(tags$code(class = "r", section_content))
                                      ))
        }
        
        # Start new section
        current_section <- section_title
        section_content <- paste(lines[-1], collapse = "\n")
      } else {
        # Just continuation of content
        section_content <- paste(section_content, "# ", section, sep = "")
      }
    }
    
    # Add the last section
    if (!is.null(current_section) && !is.null(section_content)) {
      html_output <- tagAppendChild(html_output, 
                                  tags$div(
                                    tags$h3(current_section, class = "code-section-title"),
                                    tags$pre(tags$code(class = "r", section_content))
                                  ))
    }
    
    # Add some CSS for styling
    css <- tags$style(HTML("
      .code-section-title {
        color: #2c3e50;
        border-bottom: 1px solid #eee;
        padding-bottom: 10px;
        margin-top: 20px;
      }
      pre {
        background-color: #f5f5f5;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 10px;
        margin-bottom: 20px;
        overflow: auto;
      }
      code.r {
        font-family: 'Courier New', Courier, monospace;
        white-space: pre;
      }
    "))
    
    # Return the complete UI
    tagList(
      css,
      tags$div(
        tags$h2("Generated R Code"),
        tags$p("This code can be used to reproduce the analysis:"),
        html_output,
        tags$hr(),
        downloadButton("download_combined_code", "Download Code as R Script")
      )
    )
  })

  output$download_combined_code <- downloadHandler(
    filename = function() {
      paste0("data_analysis_code-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".R")
    },
    content = function(file) {
      # Get the code as text
      code_text <- full_code()
      # Write it to the file
      writeLines(code_text, file)
    }
  )


  # Modify the pca_data reactive to handle transposition
  pca_data <- reactive({
    req(current_data())
    
    # Get the data and handle transposition based on user selection
    data_mat <- as.matrix(current_data())
    
    # Transpose if column PCA is selected
    if(input$pca_transpose == "column") {
      data_mat <- t(data_mat)
    }
    
    # Use prcomp for PCA calculation, scaling the data
    tryCatch({
      # Handle missing values
      if(any(is.na(data_mat))) {
        showNotification("Warning: Missing values found. Using pairwise complete observations.", type = "warning")
        data_mat <- na.omit(data_mat)
      }
      
      # Perform PCA
      pca_result <- prcomp(data_mat, center = TRUE, scale. = TRUE)
      
      # Store the transposition information with the result
      attr(pca_result, "transposed") <- (input$pca_transpose == "column")
      
      return(pca_result)
    }, error = function(e) {
      showNotification(paste("Error in PCA calculation:", e$message), type = "error")
      return(NULL)
    })
  })

  # Create a reactive for the PCA plot object
  create_pca_plot <- reactive({
    req(pca_data())
    req(current_data())
    
    # Get PCA results and extract the first two principal components
    pca_result <- pca_data()
    pc_data <- as.data.frame(pca_result$x[, 1:2])
    
    # Check if we're in transposed mode
    transposed <- attr(pca_result, "transposed")
    
    # Get appropriate annotations based on transposition mode
    if (transposed) {
      # For column PCA mode (columns as points), use column annotation data
      point_annot <- col_annotation_for_heatmap()
    } else {
      # For row PCA mode (rows as points), use row annotation data
      point_annot <- row_annotation_for_heatmap()
    }
    
    # PC variances for axis labels
    pc1_var <- round(summary(pca_result)$importance[2, 1] * 100, 1)
    pc2_var <- round(summary(pca_result)$importance[2, 2] * 100, 1)
    
    # Default plot settings
    point_colors <- "black"
    point_shapes <- 16
    point_sizes <- rep(input$fontsize/12, nrow(pc_data))
    
    # If annotations are available, use them for colors and shapes
    legend_items <- list()
    
    if (!is.null(point_annot) && ncol(point_annot) > 0) {
      # Get all annotation columns
      selected_cols <- names(point_annot)
      
      # Use first column for colors
      if (length(selected_cols) >= 1) {
        color_col <- selected_cols[1]
        color_factor <- as.factor(point_annot[[color_col]])
        color_levels <- levels(color_factor)
        color_palette <- rainbow(length(color_levels))
        point_colors <- color_palette[as.numeric(color_factor)]
        
        # Store color legend info
        legend_items$colors <- list(
          title = color_col,
          labels = color_levels,
          palette = color_palette
        )
      }
      
      # Use second column for shapes (ONLY if we have multiple annotations)
      if (length(selected_cols) >= 2) {
        shape_col <- selected_cols[2]
        shape_factor <- as.factor(point_annot[[shape_col]])
        shape_levels <- levels(shape_factor)
        
        available_shapes <- c(16, 17, 15, 18, 19, 1, 2, 5, 6, 8)
        shape_numbers <- available_shapes[1:min(length(available_shapes), length(shape_levels))]
        point_shapes <- shape_numbers[as.numeric(shape_factor)]
        
        # Store shape legend info
        legend_items$shapes <- list(
          title = shape_col,
          labels = shape_levels,
          shapes = shape_numbers
        )
      } else {
        # If only one annotation, use circle shape for all points
        point_shapes <- 16
      }
    }
    
    # Create a new plotting environment
    plot_env <- new.env()
    
    # Create and record the plot
    p <- with(plot_env, {
      # Set margins
      par(mar = c(5, 5, 2, 10) + 0.1)
      
      # Create the points plot
      plot(pc_data$PC1, pc_data$PC2, 
          xlab = paste0("PC1 (", pc1_var, "%)"),
          ylab = paste0("PC2 (", pc2_var, "%)"),
          main = "",
          pch = point_shapes,
          col = point_colors,
          cex = point_sizes,
          cex.lab = input$fontsize/12,
          cex.axis = input$fontsize/12)
      
      # Add legend if using annotations
      if (length(legend_items) > 0) {
        # Allow plotting outside the plot region
        par(xpd = TRUE)
        
        # Color legend
        if (!is.null(legend_items$colors)) {
          color_info <- legend_items$colors
          
          legend("topright", 
                legend = color_info$labels,
                fill = color_info$palette,
                title = color_info$title,
                cex = input$fontsize/15,
                inset = c(-0.25, 0),
                bty = "n")
        }
        
        # Shape legend (if available)
        if (!is.null(legend_items$shapes)) {
          shape_info <- legend_items$shapes
          
          legend("topright", 
                legend = shape_info$labels,
                pch = shape_info$shapes,
                title = shape_info$title,
                cex = input$fontsize/15,
                inset = c(-0.25, 0.3),
                bty = "n")
        }
        
        par(xpd = FALSE)
      }
      
      # Return the recorded plot
      recordPlot()
    })
    
    return(p)
  })

  output$pca_plot <- renderPlot({
    replayPlot(create_pca_plot())
  }, width = function() input$width, height = function() input$height)

  # Download handlers for PCA plots
  output$download_pca_pdf <- downloadHandler(
    filename = function() {
      paste0("pca-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = input$width/72, height = input$height/72)
      replayPlot(create_pca_plot())
      dev.off()
    }
  )

  output$download_pca_png <- downloadHandler(
    filename = function() {
      paste0("pca-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, res = 72)
      replayPlot(create_pca_plot())
      dev.off()
    }
  )



  # t-SNE data reactive
  tsne_data <- reactive({
    req(current_data())
    
    # Get the data and handle transposition based on user selection
    data_mat <- as.matrix(current_data())
    
    # Transpose if column t-SNE is selected
    if(input$tsne_transpose == "column") {
      data_mat <- t(data_mat)
    }
    
  # Use Rtsne for t-SNE calculation with progress indicator
  withProgress(message = 'Computing t-SNE', value = 0, {
    tryCatch({
      # Handle missing values
      if(any(is.na(data_mat))) {
        showNotification("Warning: Missing values found in t-SNE calculation. Using complete cases only.", type = "warning")
        data_mat <- na.omit(data_mat)
      }
      
      # Check if we have enough data points for the perplexity
      # t-SNE requires at least perplexity*3 + 1 points
      perplexity <- min(input$tsne_perplexity, floor(nrow(data_mat)/3) - 1)
      if(perplexity < 5) {
        perplexity <- 5
        showNotification(paste("Perplexity adjusted to", perplexity, "due to small sample size"), type = "warning")
      }
      
      # Double-check we have enough data
      if(nrow(data_mat) < perplexity * 3 + 1) {
        showNotification("Not enough samples for t-SNE with current perplexity. Try reducing perplexity.", type = "error")
        return(NULL)
      }
      
      # Scale data to have mean=0 and sd=1 (important for t-SNE)
      incProgress(0.2, detail = "Scaling data")
      scaled_data <- scale(data_mat)
      
      # Set seed for reproducibility
      set.seed(42)
      
      # Run t-SNE with progress updates
      incProgress(0.2, detail = "Running t-SNE optimization (this may take a while)")
      
      # Apply Rtsne with all user-defined parameters
      tsne_result <- Rtsne::Rtsne(
        scaled_data, 
        dims = 2,                                     # Always use 2D for visualization
        perplexity = perplexity,                      # From UI slider with validation
        check_duplicates = FALSE,                     # Skip duplicate checking for performance
        pca = input$tsne_pca_preprocessing,           # From UI checkbox
        normalize = FALSE,                            # Already normalized above
        max_iter = input$tsne_iterations,             # From UI slider
        eta = input$tsne_learning_rate,               # From UI slider
        exaggeration_factor = input$tsne_early_exaggeration,  # From UI slider
        verbose = FALSE                               # Disable verbose output
      )
      
      # Store the transposition information for the plotting function
      attr(tsne_result, "transposed") <- (input$tsne_transpose == "column")
      
      return(tsne_result)
    }, error = function(e) {
      # Handle any errors during t-SNE computation
      showNotification(paste("Error in t-SNE calculation:", e$message), type = "error")
      return(NULL)
    })
  })
  })


  # Create a reactive for the t-SNE plot object
  create_tsne_plot <- reactive({
    req(tsne_data())
    req(current_data())
    
    # Get t-SNE results and extract the two dimensions
    tsne_result <- tsne_data()
    tsne_coords <- as.data.frame(tsne_result$Y)
    colnames(tsne_coords) <- c("tSNE1", "tSNE2")
    
    # Check if we're in transposed mode
    transposed <- attr(tsne_result, "transposed")
    
    # Get appropriate annotations based on transposition mode
    if (transposed) {
      # For column t-SNE mode (columns as points), use column annotation data
      point_annot <- col_annotation_for_heatmap()
    } else {
      # For row t-SNE mode (rows as points), use row annotation data
      point_annot <- row_annotation_for_heatmap()
    }
    
    # Default plot settings
    point_colors <- "black"
    point_shapes <- 16
    point_sizes <- rep(input$fontsize/12, nrow(tsne_coords))
    
    # If annotations are available, use them for colors and shapes
    legend_items <- list()
    
    if (!is.null(point_annot) && ncol(point_annot) > 0) {
      # Get all annotation columns
      selected_cols <- names(point_annot)
      
      # Use first column for colors
      if (length(selected_cols) >= 1) {
        color_col <- selected_cols[1]
        color_factor <- as.factor(point_annot[[color_col]])
        color_levels <- levels(color_factor)
        color_palette <- rainbow(length(color_levels))
        point_colors <- color_palette[as.numeric(color_factor)]
        
        # Store color legend info
        legend_items$colors <- list(
          title = color_col,
          labels = color_levels,
          palette = color_palette
        )
      }
      
      # Use second column for shapes (ONLY if we have multiple annotations)
      if (length(selected_cols) >= 2) {
        shape_col <- selected_cols[2]
        shape_factor <- as.factor(point_annot[[shape_col]])
        shape_levels <- levels(shape_factor)
        
        available_shapes <- c(16, 17, 15, 18, 19, 1, 2, 5, 6, 8)
        shape_numbers <- available_shapes[1:min(length(available_shapes), length(shape_levels))]
        point_shapes <- shape_numbers[as.numeric(shape_factor)]
        
        # Store shape legend info
        legend_items$shapes <- list(
          title = shape_col,
          labels = shape_levels,
          shapes = shape_numbers
        )
      } else {
        # If only one annotation, use circle shape for all points
        point_shapes <- 16
      }
    }
    
    # Create a new plotting environment
    plot_env <- new.env()
    
    # Create and record the plot
    p <- with(plot_env, {
      # Set margins
      par(mar = c(5, 5, 2, 10) + 0.1)
      
      # Create the points plot
      plot(tsne_coords$tSNE1, tsne_coords$tSNE2, 
          xlab = "t-SNE Dimension 1",
          ylab = "t-SNE Dimension 2",
          main = "",
          pch = point_shapes,
          col = point_colors,
          cex = point_sizes,
          cex.lab = input$fontsize/12,
          cex.axis = input$fontsize/12)
      
      # Add legend if using annotations
      if (length(legend_items) > 0) {
        # Allow plotting outside the plot region
        par(xpd = TRUE)
        
        # Color legend
        if (!is.null(legend_items$colors)) {
          color_info <- legend_items$colors
          
          legend("topright", 
                legend = color_info$labels,
                fill = color_info$palette,
                title = color_info$title,
                cex = input$fontsize/15,
                inset = c(-0.25, 0),
                bty = "n")
        }
        
        # Shape legend (if available)
        if (!is.null(legend_items$shapes)) {
          shape_info <- legend_items$shapes
          
          legend("topright", 
                legend = shape_info$labels,
                pch = shape_info$shapes,
                title = shape_info$title,
                cex = input$fontsize/15,
                inset = c(-0.25, 0.3),
                bty = "n")
        }
        
        par(xpd = FALSE)
      }
      
      # Return the recorded plot
      recordPlot()
    })
    
    return(p)
  })

  # Use the reactive plot in the renderPlot function
  output$tsne_plot <- renderPlot({
    replayPlot(create_tsne_plot())
  }, width = function() input$width, height = function() input$height)

  # Use the same reactive plot in the download handlers
  output$download_tsne_pdf <- downloadHandler(
    filename = function() {
      paste0("tsne-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
    },
    content = function(file) {
      pdf(file, width = input$width/72, height = input$height/72)
      replayPlot(create_tsne_plot())
      dev.off()
    }
  )

  output$download_tsne_png <- downloadHandler(
    filename = function() {
      paste0("tsne-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = input$width, height = input$height, res = 72)
      replayPlot(create_tsne_plot())
      dev.off()
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)