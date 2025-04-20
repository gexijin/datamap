# t-SNE module

# this solves the issue of the download button not working from Chromium when this app is deployed as Shinylive
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}

# UI function
tsne_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Single row with 4 elements
    fluidRow(
      column(3, selectInput(ns("tsne_transpose"), "Analysis Mode:",
        choices = c("Row vectors" = "row", "Column vectors" = "column"),
        selected = "row")),
      column(3, sliderInput(ns("tsne_perplexity"), "Perplexity:", 
        min = 5, max = 50, value = 30, step = 5)),
      column(3, sliderInput(ns("tsne_early_exaggeration"), "Early Exagg.:", 
        min = 4, max = 20, value = 12, step = 1)),
      column(3, sliderInput(ns("tsne_learning_rate"), "Learning Rate:", 
        min = 50, max = 1000, value = 200, step = 50))
    ),
    # Second row with 3 elements and a checkbox
    fluidRow(
      column(3, sliderInput(ns("tsne_iterations"), "Max Iterations:", 
        min = 500, max = 2000, value = 1000, step = 100)),
      column(3, checkboxInput(ns("tsne_pca_preprocessing"), "Use PCA Preprocessing", value = FALSE)),
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
    checkboxInput(ns("show_point_labels"), "Show point labels", value = FALSE),
    plotOutput(ns("tsne_plot"), width = "100%", height = "auto"),
    downloadButton(ns("download_tsne_pdf"), "PDF"),
    downloadButton(ns("download_tsne_png"), "PNG")
  )
}

# Server function
tsne_plot_server <- function(id, current_data, col_annotation_for_heatmap, row_annotation_for_heatmap,
                             fontsize, width, height) {
  moduleServer(id, function(input, output, session) {
    
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

    # Implementation for t-SNE plot using the refactored function
    create_tsne_plot <- function() {
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
      
      # Create x and y labels
      x_label <- "tSNE 1"
      y_label <- "tSNE 2"
      
      # Get point labels if checkbox is selected and enabled
      point_labels <- NULL
      show_labels <- FALSE
      
      # Check if the checkbox is available and checked
      if (!is.null(input$show_point_labels) && input$show_point_labels) {
        show_labels <- TRUE
        data_mat <- as.matrix(current_data())
        
        if (transposed) {
          # Column names as labels in column mode
          point_labels <- colnames(data_mat)
        } else {
          # Row names as labels in row mode
          point_labels <- rownames(data_mat)
        }
      }
      
      # Use the generic function to create the plot
      create_dr_plot(tsne_coords, x_label, y_label, point_annot, fontsize(),
                    show_labels = show_labels, point_labels = point_labels)
    }

    # Use the reactive plot in the renderPlot function
    output$tsne_plot <- renderPlot({
      replayPlot(create_tsne_plot())
    }, width = function() width(), height = function() height())

    # Use the same reactive plot in the download handlers
    output$download_tsne_pdf <- downloadHandler(
      filename = function() {
        paste0("tsne-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file, width = width()/72, height = height()/72)
        replayPlot(create_tsne_plot())
        dev.off()
      }
    )

    output$download_tsne_png <- downloadHandler(
      filename = function() {
        paste0("tsne-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
      },
      content = function(file) {
        png(file, width = width(), height = height(), res = 72)
        replayPlot(create_tsne_plot())
        dev.off()
      }
    )
    
    # Generate the t-SNE code (matching the PCA module's approach)
    tsne_code <- function() {
      req(tsne_data())
      
      # Capture the actual adjusted perplexity value from the tsne_data reactive
      # to ensure reproducibility
      tsne_result <- tsne_data()
      data_mat <- as.matrix(current_data())
      if(input$tsne_transpose == "column") {
        data_mat <- t(data_mat)
      }
      actual_perplexity <- min(input$tsne_perplexity, floor(nrow(data_mat)/3) - 1)
      if(actual_perplexity < 5) actual_perplexity <- 5
      
      # Start with required libraries
      code <- c(
        "library(Rtsne)",
        ""
      )
      
      # Convert to matrix and handle transposition
      code <- c(code,
        "# Get the data and handle transposition based on user selection",
        "data_mat <- as.matrix(processed_data)",
        ""
      )
      
      # Add the transpose code only if needed
      if (input$tsne_transpose == "column") {
        code <- c(code,
          "# Transpose matrix for column-based analysis",
          "data_mat <- t(data_mat)",
          ""
        )
      }
      
      # Add error handling for missing values
      code <- c(code,
        "# Handle missing values",
        "if (any(is.na(data_mat))) {",
        "  print(\"Warning: Missing values found. Using complete cases only.\")",
        "  data_mat <- na.omit(data_mat)",
        "}",
        ""
      )
      
      # Check perplexity and data size - use the ACTUAL value being used in the app
      code <- c(code,
        "# Check perplexity against data size",
        paste0("perplexity <- ", actual_perplexity, " # Adjusted perplexity value"),
        "",
        "# Verify we have enough data",
        "if(nrow(data_mat) < perplexity * 3 + 1) {",
        "  stop(\"Not enough samples for t-SNE with current perplexity. Try reducing perplexity.\")",
        "}",
        ""
      )
      
      # Scale data
      code <- c(code,
        "# Scale data to have mean=0 and sd=1 (important for t-SNE)",
        "scaled_data <- scale(data_mat)",
        "",
        "# Set seed for reproducibility",
        "set.seed(42)",
        ""
      )
      
      # Perform t-SNE (use the exact parameters from the UI)
      code <- c(code,
        "# Perform t-SNE",
        paste0("tsne_result <- Rtsne::Rtsne(",
               "\n  scaled_data,", 
               "\n  dims = 2,", 
               "\n  perplexity = perplexity,", 
               "\n  check_duplicates = FALSE,", 
               sprintf("\n  pca = %s,", ifelse(input$tsne_pca_preprocessing, "TRUE", "FALSE")),
               "\n  normalize = FALSE,", 
               sprintf("\n  max_iter = %d,", input$tsne_iterations),
               sprintf("\n  eta = %d,", input$tsne_learning_rate),
               sprintf("\n  exaggeration_factor = %d,", input$tsne_early_exaggeration),
               "\n  verbose = TRUE",
               "\n)"),
        "",
        "# Extract t-SNE coordinates",
        "tsne_coords <- as.data.frame(tsne_result$Y)",
        "colnames(tsne_coords) <- c(\"tSNE1\", \"tSNE2\")",
        ""
      )
      
      # Prepare for plotting (matching the logic in create_tsne_plot)
      code <- c(code,
        "# Define transposed setting based on analysis mode",
        sprintf("transposed <- %s", ifelse(input$tsne_transpose == "column", "TRUE", "FALSE")),
        "",
        "# Get appropriate annotations based on transposition mode",
        "if (transposed) {",
        "  # For column t-SNE mode (columns as points), use column annotation data",
        "  point_annot <- col_annotation",
        "} else {",
        "  # For row t-SNE mode (rows as points), use row annotation data",
        "  point_annot <- row_annotation",
        "}",
        "",
        "# Create axis labels",
        "x_label <- \"tSNE 1\"",
        "y_label <- \"tSNE 2\"",
        ""
      )
      
      # Handle point labels
      code <- c(code,
        "# Point label settings",
        sprintf("show_labels <- %s", ifelse(input$show_point_labels, "TRUE", "FALSE")),
        "point_labels <- NULL",
        "",
        "# Set up point labels if needed",
        "if (show_labels) {",
        "  if (transposed) {",
        "    # Use column names as labels in column mode",
        "    point_labels <- colnames(processed_data)",
        "  } else {",
        "    # Use row names as labels in row mode",
        "    point_labels <- rownames(processed_data)",
        "  }",
        "}",
        ""
      )
      
      # Use the generic function to create the plot
      code <- c(code,
        "# Plot the t-SNE results",
        sprintf("create_dr_plot(tsne_coords, x_label, y_label, point_annot, %d,", fontsize()),
        "               show_labels = show_labels, point_labels = point_labels)"
      )
      
      return(code)
    }
    
    # Return the tsne_data and code generation function for use outside the module
    return(list(
      tsne_data = tsne_data,
      tsne_code = tsne_code
    ))
  })
}