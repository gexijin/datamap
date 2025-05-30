# this solves the issue of the download button not working from Chromium when this app is deployed as Shinylive
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Module UI function
heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12, 
        downloadButton(ns("download_pdf"), "PDF"),
        downloadButton(ns("download_png"), "PNG"), 
        align = "right")
    ),
    plotOutput(ns("heatmap"), width = "100%", height = "600px"),
  )
}

# Module server function
heatmap_server <- function(id, current_data, 
                         file_data,
                         unprocessed_data,
                         col_annotation_for_heatmap, 
                         row_annotation_for_heatmap, 
                         transform_data,
                         max_rows_to_show = 1000,
                         default_width = 600,
                         default_height = 600) {
                          
  moduleServer(id, function(input, output, session) {
    # Initialize internal reactive values for width/height
    width <- reactiveVal(default_width)
    height <- reactiveVal(default_height)
    
    # Update width/height reactives when inputs change
    observe({
      if(!is.null(input$width)) width(input$width)
      if(!is.null(input$height)) height(input$height)
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
              display_numbers = if (input$label_heatmap) round(as.matrix(unprocessed_data()), 2) else FALSE
            )
            
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
              display_numbers = if (input$label_heatmap) round(as.matrix(unprocessed_data()), 2) else FALSE
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
    }, width = function() width(), height = function() height())
    
    # Download handlers using the same heatmap object
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("heatmap-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file, width = width()/72, height = height()/72)
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
        png(file, width = width(), height = height(), res = 72)
        grid::grid.newpage()
        grid::grid.draw(heatmap_obj())
        dev.off()
      }
    )
    
    # Generate heatmap code using the actual parameters that were used
    heatmap_code <- reactive({
      req(current_data(), pheatmap_params_used())
      # Extract the params that were used
      params <- pheatmap_params_used()
      
      code_parts <- c(
        "# Heatmap Generation Code",
        "library(pheatmap)",
        "library(RColorBrewer)",
        "library(grid)",
        "",
        "processed_data <- transformed_data$numeric_data",
        ""
      )
      
      # Add color palette code
      if (!is.null(params$color)) {
        if (identical(params$color, colorRampPalette(c("green", "black", "red"))(100))) {
          code_parts <- c(code_parts, "# Define color palette",
                          "colors <- colorRampPalette(c(\"green\", \"black\", \"red\"))(100)")
        } else {
          # Determine which palette was used
          for (palette_name in c("RdBu", "RdYlBu", "YlOrRd", "YlGnBu", "Blues", "Greens", "Purples", "Reds", "OrRd")) {
            if (identical(params$color, colorRampPalette(rev(RColorBrewer::brewer.pal(11, palette_name)))(100))) {
              code_parts <- c(code_parts, "# Define color palette",
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
        code_parts <- c(code_parts, "", 
                        "# Custom correlation function for distance calculation",
                        "custom_cor <- function(x, method = \"pearson\") {",
                        "  cors <- cor(x, method = method, use = \"pairwise.complete.obs\")",
                        "  # Replace NAs with 0 correlations",
                        "  cors[is.na(cors)] <- 0",
                        "  as.dist(1 - cors)",
                        "}")
      }
      
      # Add annotation handling code if annotations are used
      if (!is.null(params$annotation_col)) {
        # Extract the selected column annotation rows
        if (!is.null(col_annotation_for_heatmap())) {
          selected_col_anno_rows <- colnames(col_annotation_for_heatmap())
          if (length(selected_col_anno_rows) > 0) {
            selected_rows_code <- paste0("c(", paste0("\"", selected_col_anno_rows, "\"", collapse = ", "), ")")
            code_parts <- c(code_parts, "",
                          "# Column annotation code",
                          paste0("selected_col_anno_rows <- ", selected_rows_code),
                          "main_cols <- colnames(processed_data)",
                          "col_annotation <- NULL",
                          "if (exists(\"col_annotation_raw\") && !is.null(col_annotation_raw)) {",
                          "  col_annotation <- process_column_annotations(",
                          "    main_data_cols = main_cols,",
                          "    annotation_df = col_annotation_raw,",
                          "    selected_annotations = selected_col_anno_rows",
                          "  )",
                          "}")
          }
        }
      }
      
      if (!is.null(params$annotation_row)) {
        # Extract the selected row annotation rows
        if (!is.null(row_annotation_for_heatmap())) {
          selected_row_anno_rows <- colnames(row_annotation_for_heatmap())
          if (length(selected_row_anno_rows) > 0) {
            selected_rows_code <- paste0("c(", paste0("\"", selected_row_anno_rows, "\"", collapse = ", "), ")")
            code_parts <- c(code_parts, "",
                          "# Row annotation code",
                          paste0("selected_row_anno_rows <- ", selected_rows_code),
                          "main_rows <- rownames(processed_data)",
                          "row_annotation <- NULL",
                          "if (exists(\"row_annotation_raw\") && !is.null(row_annotation_raw)) {",
                          "  factor_annotation_df <- NULL",
                          "  # Factor columns might come from transformed_data if available",
                          "  if (exists(\"transformed_data\") && !is.null(transformed_data$factor_columns)) {",
                          "    factor_annotation_df <- transformed_data$factor_columns",
                          "  }",
                          "  row_annotation <- process_row_annotations(",
                          "    main_data_rows = main_rows,",
                          "    file_annotation_df = row_annotation_raw,",
                          "    factor_annotation_df = factor_annotation_df,",
                          "    selected_annotations = selected_row_anno_rows",
                          "  )",
                          "}")
          }
        }
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
          # Add code to create the distance object before the pheatmap call
          code_parts <- c(code_parts, "",
                        "# Calculate distance matrices for clustering",
                        paste0("row_dist <- custom_cor(t(processed_data), method = \"", params$clustering_distance_rows, "\")"))
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = row_dist")
        } else {
          # For standard distance methods, use the string
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_rows = \"", params$clustering_distance_rows, "\"")
        }
      }

      if (!is.null(params$clustering_distance_cols)) {
        if (params$clustering_distance_cols %in% c("pearson", "spearman", "kendall")) {
          # Add code to create the distance object before the pheatmap call
          code_parts <- c(code_parts, "",
                        paste0("col_dist <- custom_cor(processed_data, method = \"", params$clustering_distance_cols, "\")"))
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = col_dist")
        } else {
          # For standard distance methods, use the string
          pheatmap_call <- paste0(pheatmap_call, ",\n  clustering_distance_cols = \"", params$clustering_distance_cols, "\"")
        }
      }
      
      # Handle annotation parameters
      if (!is.null(params$annotation_col)) {
        pheatmap_call <- paste0(pheatmap_call, ",\n  annotation_col = col_annotation")
      }
      
      if (!is.null(params$annotation_row)) {
        pheatmap_call <- paste0(pheatmap_call, ",\n  annotation_row = row_annotation")
      }
      
      # Handle display_numbers if it was used
      if (!is.null(params$display_numbers)) {
        if (is.logical(params$display_numbers)) {
          if (params$display_numbers) {
            pheatmap_call <- paste0(pheatmap_call, ",\n  display_numbers = TRUE")
          }
        } else {
          # If display_numbers contains a matrix of values, use the raw data
          code_parts <- c(code_parts, "",
                        "# Create display numbers matrix from raw data",
                        "# In standalone RStudio, we'll just use the raw_data directly",
                        "display_numbers <- round(as.matrix(raw_data), 2)")
          pheatmap_call <- paste0(pheatmap_call, ",\n  display_numbers = display_numbers")
        }
      }
      
      # Add colors
      pheatmap_call <- paste0(pheatmap_call, ",\n  color = colors")
      
      # Close the pheatmap call
      pheatmap_call <- paste0(pheatmap_call, "\n)")
      
      # Complete the heatmap code
      code_parts <- c(code_parts, "", "# Generate the heatmap", pheatmap_call)
      
      paste(code_parts, collapse = "\n")
    })

    
    # Return reactives that will be needed by the parent module
    return(list(
      heatmap_code = heatmap_code,
      params_used = pheatmap_params_used
    ))
  })
}

# Helper function to generate heatmap UI elements for the sidebar
heatmap_control_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Clustering options - compact layout
    fluidRow(
      column(6, checkboxInput(ns("cluster_rows"), "Cluster Rows", TRUE)),
      column(6, checkboxInput(ns("cluster_cols"), "Cluster Columns", TRUE))
    ),
    
    # Linkage method - label to the left
    fluidRow(
      column(3, p("Linkage:", style="padding-top: 7px; text-align: right;")),
      column(9, selectInput(ns("clustering_method"), NULL,
              choices = c("complete", "average", "single", "ward.D", "ward.D2", "mcquitty", "median", "centroid"),
              selected = "average"))
    ),

    # Distance method - label to the left
    fluidRow(
      column(3, p("Distance:", style="padding-top: 7px; text-align: right;")),
      column(9, selectInput(ns("distance_method"), NULL,
               choices = c("euclidean", "manhattan", "maximum", "canberra", "binary", "minkowski", "pearson", "spearman", "kendall"),
               selected = "euclidean"))
    ),
    
    # Color palette - compact
    hr(),
    fluidRow(
      column(3, p("Colors:", style="padding-top: 7px; text-align: right;")),
      column(9, selectInput(ns("color"), NULL,
               choices = c("Green Black Red" = "GreenBlackRed", "Red yellow blue" ="RdYlBu", "Red Blue" = "RdBu", "Yellow orange red" = "YlOrRd", 
                           "Yellow Green Blue" = "YlGnBu", "Blues", "Greens", "Purples", "Reds", "OrRd"),
               selected = "RdYlBu"))
    ),
    
    # Font size - more compact
    fluidRow(
      column(3, p("Font:", style="padding-top: 7px; text-align: right;")),
      column(9, sliderInput(ns("fontsize"), NULL, min = 5, max = 25, value = 12))
    ),
    
    # Width & Height in more compact form
    fluidRow(
      column(3, p("Width:", style="padding-top: 7px; text-align: right;")),
      column(9, sliderInput(ns("width"), NULL, min = 200, max = 4000, value = 600, step = 20))
    ),
    
    fluidRow(
      column(3, p("Height:", style="padding-top: 7px; text-align: right;")),
      column(9, sliderInput(ns("height"), NULL, min = 200, max = 6000, value = 600, step = 20))
    ),
    fluidRow(
      column(6, checkboxInput(ns("label_heatmap"), "Label Data", value = FALSE)),
      column(6, checkboxInput(ns("show_row_names"), "Row Names", value = FALSE))
    ),
    fluidRow(
      column(6, numericInput(ns("cutree_rows"), "Row clusters", value = 1, min = 1, max = 100, step = 1)),
      column(6, numericInput(ns("cutree_cols"), "Col. Clusters", value = 1, min = 1, max = 100, step = 1))
    )
  )
}