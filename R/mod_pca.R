# PCA Plot Module
# This module encapsulates the PCA plot functionality

# this solves the issue of the download button not working from Chromium when this app is deployed as Shinylive
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}

# UI Function
pca_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3, selectInput(ns("pca_transpose"), NULL,
        choices = c("Row vectors" = "row", 
              "Column vectors" = "column"),
        selected = "row"), style = "margin-top:5px;"),
      column(9, checkboxInput(ns("show_point_labels"), "Show names", value = FALSE), style = "margin-top:5px;", align = "left")
    ),
    plotOutput(ns("pca_plot"), width = "100%", height = "auto"),
    downloadButton(ns("download_pca_pdf"), "PDF"),
    downloadButton(ns("download_pca_png"), "PNG")
  )
}

# Server Function
pca_plot_server <- function(id, current_data, col_annotation_for_heatmap, row_annotation_for_heatmap, input_fontsize, input_width, input_height) {
  moduleServer(id, function(input, output, session) {
    
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
    
    # Implementation for PCA plot using the refactored function
    create_pca_plot <- function() {
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
      
      # Create x and y labels
      x_label <- paste0("PC1 (", pc1_var, "%)")
      y_label <- paste0("PC2 (", pc2_var, "%)")
      
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
      create_dr_plot(pc_data, x_label, y_label, point_annot, input_fontsize(), 
                     show_labels = show_labels, point_labels = point_labels)
    }
    
    output$pca_plot <- renderPlot({
      replayPlot(create_pca_plot())
    }, width = function() input_width(), height = function() input_height())
    
    # Download handlers for PCA plots
    output$download_pca_pdf <- downloadHandler(
      filename = function() {
        paste0("pca-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf")
      },
      content = function(file) {
        pdf(file, width = input_width()/72, height = input_height()/72)
        replayPlot(create_pca_plot())
        dev.off()
      }
    )
    
    output$download_pca_png <- downloadHandler(
      filename = function() {
        paste0("pca-plot-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".png")
      },
      content = function(file) {
        png(file, width = input_width(), height = input_height(), res = 72)
        replayPlot(create_pca_plot())
        dev.off()
      }
    )
    
    # Generate the PCA code
    pca_code <- function() {
      req(pca_data())
      
      # Capture the current user settings as values
      transpose_selection <- input$pca_transpose
      show_labels <- !is.null(input$show_point_labels) && input$show_point_labels
      font_size <- input_fontsize()
      
      # Start with required inputs documentation
      code <- c(
        "library(stats)",
        ""
      )
      
      # Convert to matrix and handle transposition (matching app's pca_data reactive)
      code <- c(code,
        "# Get the data and handle transposition based on user selection",
        "data_mat <- as.matrix(processed_data)",
        "",
        "# Transpose if column PCA is selected",
        if (transpose_selection == "column") {
          "data_mat <- t(data_mat)"
        },
        ""
      )
      
      # Add error handling for missing values (matching app exactly)
      code <- c(code,
        "# Handle missing values",
        "if (any(is.na(data_mat))) {",
        "  print(\"Warning: Missing values found. Using pairwise complete observations.\")",
        "  data_mat <- na.omit(data_mat)",
        "}",
        ""
      )
      
      # Perform PCA (matching app exactly)
      code <- c(code,
        "# Perform PCA",
        "pca_result <- prcomp(data_mat, center = TRUE, scale. = TRUE)",
        "",
        "# Store the transposition information with the result",
        sprintf("attr(pca_result, \"transposed\") <- %s", if(transpose_selection == "column") "TRUE" else "FALSE"),
        ""
      )
      
      # Now match the app's create_pca_plot function exactly
      code <- c(code,
        "# Get PCA results and extract the first two principal components",
        "pc_data <- as.data.frame(pca_result$x[, 1:2])",
        "",
        "# Check if we're in transposed mode",
        "transposed <- attr(pca_result, \"transposed\")",
        "",
        "# Get appropriate annotations based on transposition mode",
        "if (transposed) {",
        "  # For column PCA mode (columns as points), use column annotation data",
        "  point_annot <- col_annotation",
        "} else {",
        "  # For row PCA mode (rows as points), use row annotation data",
        "  point_annot <- row_annotation",
        "}",
        "",
        "# PC variances for axis labels",
        "pc1_var <- round(summary(pca_result)$importance[2, 1] * 100, 1)",
        "pc2_var <- round(summary(pca_result)$importance[2, 2] * 100, 1)",
        "",
        "# Create x and y labels",
        "x_label <- paste0(\"PC1 (\", pc1_var, \"%)\")",
        "y_label <- paste0(\"PC2 (\", pc2_var, \"%)\")",
        "",
        "# Get point labels if checkbox is selected",
        "point_labels <- NULL",
        sprintf("show_labels <- %s", if(show_labels) "TRUE" else "FALSE"),
        "",
        "# Check if the checkbox is available and checked",
        "if (show_labels) {",
        "  data_mat <- as.matrix(processed_data)",  # matching the app's logic exactly
        "  ",
        "  if (transposed) {",
        "    # Column names as labels in column mode",
        "    point_labels <- colnames(data_mat)",
        "  } else {",
        "    # Row names as labels in row mode",
        "    point_labels <- rownames(data_mat)",
        "  }",
        "}",
        ""
      )
      
      # Use the generic function to create the plot (matching the app's call exactly)
      code <- c(code,
        "# Use the generic function to create the plot",
        sprintf("create_dr_plot(pc_data, x_label, y_label, point_annot, %d,", font_size),
        "               show_labels = show_labels, point_labels = point_labels)"
      )
      
      return(code)
    }
    
    # Return the pca_data and code generation function for use outside the module
    return(list(
      pca_data = pca_data,
      pca_code = pca_code
    ))
  })
}