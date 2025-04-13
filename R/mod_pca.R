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
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    selectInput(ns("pca_transpose"), "PCA Analysis Mode:",
                choices = c("Row vectors" = "row", 
                            "Column vectors" = "column"),
                selected = "row"),
    # Add uiOutput for dynamic checkbox display
    uiOutput(ns("labels_checkbox_ui")),
    plotOutput(ns("pca_plot"), width = "100%", height = "auto"),
    downloadButton(ns("download_pca_pdf"), "PDF"),
    downloadButton(ns("download_pca_png"), "PNG")
  )
}

# Server Function
pca_plot_server <- function(id, current_data, col_annotation_for_heatmap, row_annotation_for_heatmap, input_fontsize, input_width, input_height) {
  moduleServer(id, function(input, output, session) {
    
    # Create the dynamic UI for the labels checkbox
    output$labels_checkbox_ui <- renderUI({
      req(current_data())
      data_mat <- as.matrix(current_data())
      
      # Determine if we should show the checkbox based on data dimensions
      show_checkbox <- if(input$pca_transpose == "column") {
        ncol(data_mat) <= 200
      } else {
        nrow(data_mat) <= 200
      }
      
      # Only render the checkbox if we have a reasonable number of points
      if(show_checkbox) {
        checkboxInput(session$ns("show_point_labels"), "Show names", value = FALSE)
      }
    })
    
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
      
      code <- c(
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
      code <- c(code,
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
      
      # Add point label code if checkbox is selected
      if (!is.null(input$show_point_labels) && input$show_point_labels) {
        code <- c(code,
          "",
          "# Add point labels",
          "text(pc_data$PC1, pc_data$PC2, labels = rownames(processed_data),",
          "     pos = 4, offset = 0.5, cex = fontsize/15)",
          ""
        )
      }
      
      # Add row annotation code if used
      if (!is.null(row_annotation_for_heatmap())) {
        code <- c(code,
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
          ""
        )
        
        # Add point label code if checkbox is selected (for annotated plot)
        if (!is.null(input$show_point_labels) && input$show_point_labels) {
          code <- c(code,
            "  # Add point labels",
            "  text(pc_data$PC1, pc_data$PC2, labels = rownames(processed_data),",
            "       pos = 4, offset = 0.5, cex = fontsize/15)",
            ""
          )
        }
        
        code <- c(code,
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
      
      return(code)
    }
    
    # Return the pca_data and code generation function for use outside the module
    return(list(
      pca_data = pca_data,
      pca_code = pca_code
    ))
  })
}