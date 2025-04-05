library(shiny)
library(pheatmap)
library(RColorBrewer)
library(readxl)  # file upload module
library(e1071)   # transform module (skewness calculation)
library(grid)    # needed for grid.draw
library(gplots) 

source("mod_file_upload.R")
source("mod_transform.R")
source("utilities.R")

max_rows_to_show <- 1000  # Maximum number of rows to show row names in the heatmap
default_width <- 600
default_height <- 600
ui <- fluidPage(

  titlePanel("DataMap"),  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = "output.data_loaded",
        actionButton("reset_session", "Reset All", 
                    icon = icon("refresh"), 
                    style = "width: 100%; margin-bottom: 15px; background-color: #f8d7da; color: #721c24;")
      ),
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
          column(9, sliderInput("width", NULL, min = 500, max = 2000, value = default_width, step = 100))
        ),
        
        fluidRow(
          column(3, p("Height:", style="padding-top: 7px; text-align: right;")),
          column(9, sliderInput("height", NULL, min = 500, max = 2000, value = default_height, step = 100))
        ),
        fluidRow(
          column(6, checkboxInput("label_heatmap", "Label", value = FALSE)),
          column(6, checkboxInput("show_row_names", "Row Names", value = FALSE))
        ),
        hr(),
        fluidRow(
          column(6, 
            numericInput("cutree_rows", "Row clusters", value = 1, min = 1, max = 100, step = 1),
          ),
          column(6, 
            numericInput("cutree_cols", "Col. Clusters", value = 1, min = 1, max = 100, step = 1),
          )
        ),
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
        tabPanel("Code",
                downloadButton("download_combined_code", "Download All Code")
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
      tags$div(
        tags$h4("Main data file"),
        file_upload_ui("file_upload"),
        downloadButton("download_example", "Eample", style = "margin-top: -15px;")
      ),
      hr(),
      # Column annotation file upload widget
      tags$div(
        tags$h4("Optional: Column Annotation"),
        file_upload_ui("col_annotation_file_upload"),
        downloadButton("download_example_col", "Eample", style = "margin-top: -15px;")
      ),
      hr(),
      # Row annotation file upload widget
      tags$div(
        tags$h4("Optional: Row Annotations"),
        file_upload_ui("row_annotation_file_upload"),
        downloadButton("download_example_row", "Eample", style = "margin-top: -15px;")
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
      "example row annotation.csv"
    },
    content = function(file) {
      file.copy("data/iris_row.csv", file)
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
    selectInput("col_annotation_select", "Column annotation:", 
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
    selectInput("row_annotation_select", "Row annotation:", 
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

  # Replace the heatmap_obj reactive function with this corrected version:

  # Create a reactive to store and access the actual pheatmap parameters
  pheatmap_params_used <- reactiveVal(NULL)
  
  # Modify the heatmap_obj reactive function to save the parameters:
  heatmap_obj <- reactive({
    req(current_data())

    withProgress(message = 'Generating heatmap', value = 0, {
      # Convert the current data to a numeric matrix for the heatmap
      incProgress(0.1, detail = "Preparing data")
      heatmap_data <- current_data()
     
      show_row_names <- file_data$has_rownames() && !is.null(rownames(heatmap_data))
      # Use the user's input if available; otherwise, use the default
      if(show_row_names && !is.null(input$show_row_names)) {
        show_row_names <- input$show_row_names
      }

      # Select the color palette
      if (input$color == "GreenBlackRed") {
        colors <- colorRampPalette(c("green", "black", "red"))(100)
      } else {
        colors <- colorRampPalette(rev(brewer.pal(11, input$color)))(100)
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
            pheatmap_params <- list(
              mat = heatmap_data,
              color = colors,
              cluster_rows = input$cluster_rows,
              cluster_cols = input$cluster_cols,
              clustering_method = clustering_method,
              clustering_distance_rows = dist_rows,
              clustering_distance_cols = dist_cols,
              fontsize = input$fontsize,
              annotation_col = col_annotation_for_heatmap(),
              annotation_row = row_annotation_for_heatmap(),
              show_rownames = show_row_names,
              silent = TRUE,
              display_numbers = if (input$label_heatmap) round(as.matrix(current_data()), 2) else FALSE
            )

            # Only add cutree parameters if clustering is enabled and value > 0
            if(!is.na(input$cutree_rows)) { # when user delete the number in the input box, it will be NA
              if (input$cluster_rows && input$cutree_rows > 1 && input$cutree_rows <= nrow(heatmap_data)) {
                pheatmap_params$cutree_rows <- input$cutree_rows
              }
            }
            if(!is.na(input$cutree_cols)) {
              if (input$cluster_cols && input$cutree_cols > 1 && input$cutree_cols <= ncol(heatmap_data)) {
                pheatmap_params$cutree_cols <- input$cutree_cols
              }
            }

            # Store the parameters for code generation
            pheatmap_params_used(pheatmap_params)
            
            # Call pheatmap with the parameter list
            do.call(pheatmap, pheatmap_params)
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
            
            do.call(pheatmap, pheatmap_params)
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
          if (identical(params$color, colorRampPalette(rev(brewer.pal(11, palette_name)))(100))) {
            heatmap_code <- c(heatmap_code, "# Define color palette",
                              paste0("colors <- colorRampPalette(rev(brewer.pal(11, \"", palette_name, "\")))(100)"))
            break
          }
        }
      }
    }
    params$silent <- FALSE # otherwise pheatmap will not show
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
    
    # Handle distance methods
    if (!is.null(params$clustering_distance_rows)) {
      if (is.character(params$clustering_distance_rows)) {
        pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = \"", params$clustering_distance_rows, "\"")
      } else {
        # If it's a custom distance object, include the custom_cor function
        heatmap_code <- c(heatmap_code, "", 
                        "# Custom correlation function for distance calculation",
                        "custom_cor <- function(x, method = \"pearson\") {",
                        "  cors <- tryCatch({",
                        "    cor(x, method = method, use = \"pairwise.complete.obs\")",
                        "  }, error = function(e) {",
                        "    warning(\"Error calculating correlation: \", e$message)",
                        "    return(diag(nrow(x)))",
                        "  })",
                        "  # Replace NAs with 0 correlations",
                        "  cors[is.na(cors)] <- 0",
                        "  as.dist(1 - cors)",
                        "}")
                        
        # Determine correlation method used
        corr_method <- input$distance_method
        if (corr_method %in% c("pearson", "spearman", "kendall")) {
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = custom_cor(t(processed_data), method = \"", corr_method, "\")")
        } else {
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = custom_cor(t(processed_data))")
        }
      }
    }
    
    if (!is.null(params$clustering_distance_cols)) {
      if (is.character(params$clustering_distance_cols)) {
        pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = \"", params$clustering_distance_cols, "\"")
      } else {
        # If the custom_cor function was already added, don't add it again
        corr_method <- input$distance_method
        if (corr_method %in% c("pearson", "spearman", "kendall")) {
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = custom_cor(processed_data, method = \"", corr_method, "\")")
        } else {
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = custom_cor(processed_data)")
        }
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
  }
  
  # Combine all parts and return
  paste(code_parts, collapse = "\n")
})

# Fixed download handler
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
  
}

# Run the application
shinyApp(ui = ui, server = server)