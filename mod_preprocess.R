# mod_preprocess.R
# A Shiny module for data preprocessing with visualization

library(shiny)
library(ggplot2)
library(DT)
library(e1071)  # For skewness calculation

#' UI function for preprocessing module button
#'
#' @param id The module namespace id
#' @return A button that triggers the preprocessing modal
#'
preprocessButtonUI <- function(id) {
  ns <- NS(id)
  
  actionButton(ns("show_preprocess"), "Preprocess Data", 
               icon = icon("filter"),
               class = "btn-primary")
}

#' Server function for preprocessing module
#'
#' @param id The module namespace id
#' @param data Reactive data matrix to process
#' @return A list with the processed data reactive
#'
preprocessServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store processed data and state
    rv <- reactiveValues(
      processed_data = NULL,
      has_missing = FALSE,
      has_negative = FALSE,
      skewness = 0,
      original_data = NULL
    )
    
    # Function to analyze data and compute statistics
    analyze_data <- function(data_matrix) {
      if (is.null(data_matrix)) return()
      
      # Store original data
      rv$original_data <- data_matrix
      
      # Detect missing values
      rv$has_missing <- any(is.na(data_matrix))
      
      # Check for negative values
      rv$has_negative <- any(data_matrix < 0, na.rm = TRUE)
      
      # Calculate skewness
      flat_data <- as.numeric(data_matrix)
      flat_data <- flat_data[!is.na(flat_data)]
      rv$skewness <- skewness(flat_data)
      
      # Initial processed data is the input data
      rv$processed_data <- data_matrix
    }
    
    # Function to apply preprocessing based on current settings
    apply_preprocessing <- function() {
      if (is.null(rv$original_data)) return()
      
      # Start with the original data
      processed <- rv$original_data
      
      # 1. Handle missing values
      if (rv$has_missing) {
        # Remove rows that are entirely NA
        row_all_na <- apply(processed, 1, function(x) all(is.na(x)))
        if (any(row_all_na)) {
          processed <- processed[!row_all_na, , drop = FALSE]
        }
        
        # Remove columns that are entirely NA
        col_all_na <- apply(processed, 2, function(x) all(is.na(x)))
        if (any(col_all_na)) {
          processed <- processed[, !col_all_na, drop = FALSE]
        }
        
        # Handle remaining missing values according to user selection
        if (input$na_method != "leave") {
          if (input$na_method == "zero") {
            processed[is.na(processed)] <- 0
          } else if (input$na_method == "mean") {
            # Replace NA with mean of respective column
            for (j in 1:ncol(processed)) {
              col_mean <- mean(processed[, j], na.rm = TRUE)
              processed[is.na(processed[, j]), j] <- col_mean
            }
          } else if (input$na_method == "median") {
            # Replace NA with median of respective column
            for (j in 1:ncol(processed)) {
              col_median <- median(processed[, j], na.rm = TRUE)
              processed[is.na(processed[, j]), j] <- col_median
            }
          }
        }
      }
      
      # 2. Apply log transformation if selected
      if (input$do_log_transform) {
        # Set negative values to zero if they exist
        if (any(processed < 0, na.rm = TRUE)) {
          processed[processed < 0] <- 0
        }
        
        # Get constant c for log(x + c)
        const <- as.numeric(input$log_constant)
        
        # Apply log transformation
        processed <- log10(processed + const)
      }
      
      # 3. Apply centering and scaling
      if (input$center_scale != "none") {
        if (input$center_scale == "center_row") {
          # Center by row
          row_means <- rowMeans(processed, na.rm = TRUE)
          processed <- t(t(processed) - row_means)
        } else if (input$center_scale == "scale_row") {
          # Scale by row (z-score)
          row_means <- rowMeans(processed, na.rm = TRUE)
          row_sds <- apply(processed, 1, sd, na.rm = TRUE)
          processed <- t((t(processed) - row_means) / ifelse(row_sds == 0, 1, row_sds))
          # If SD is zero, set row to zero
          zero_sd_rows <- which(row_sds == 0)
          if (length(zero_sd_rows) > 0) {
            processed[zero_sd_rows, ] <- 0
          }
        } else if (input$center_scale == "center_col") {
          # Center by column
          col_means <- colMeans(processed, na.rm = TRUE)
          processed <- t(t(processed) - col_means)
        } else if (input$center_scale == "scale_col") {
          # Scale by column (z-score)
          col_means <- colMeans(processed, na.rm = TRUE)
          col_sds <- apply(processed, 2, sd, na.rm = TRUE)
          processed <- sweep(processed, 2, col_means, "-")
          processed <- sweep(processed, 2, ifelse(col_sds == 0, 1, col_sds), "/")
          # If SD is zero, set column to zero
          zero_sd_cols <- which(col_sds == 0)
          if (length(zero_sd_cols) > 0) {
            processed[, zero_sd_cols] <- 0
          }
        }
      }
      
      # 4. Apply Z-score cutoff for outlier capping
      if (input$do_zscore_cap) {
        z_cutoff <- as.numeric(input$zscore_cutoff)
        
        if (input$center_scale %in% c("center_row", "scale_row")) {
          # Cap by row if row-wise centering/scaling was applied
          for (i in 1:nrow(processed)) {
            row_data <- processed[i, ]
            row_mean <- mean(row_data, na.rm = TRUE)
            row_sd <- sd(row_data, na.rm = TRUE)
            if (row_sd > 0) {
              upper_bound <- row_mean + z_cutoff * row_sd
              lower_bound <- row_mean - z_cutoff * row_sd
              processed[i, row_data > upper_bound] <- upper_bound
              processed[i, row_data < lower_bound] <- lower_bound
            }
          }
        } else if (input$center_scale %in% c("center_col", "scale_col", "none")) {
          # Cap by column if column-wise centering/scaling was applied or none
          for (j in 1:ncol(processed)) {
            col_data <- processed[, j]
            col_mean <- mean(col_data, na.rm = TRUE)
            col_sd <- sd(col_data, na.rm = TRUE)
            if (col_sd > 0) {
              upper_bound <- col_mean + z_cutoff * col_sd
              lower_bound <- col_mean - z_cutoff * col_sd
              processed[col_data > upper_bound, j] <- upper_bound
              processed[col_data < lower_bound, j] <- lower_bound
            }
          }
        }
      }
      
      rv$processed_data <- processed
    }
    
    # Watch for data changes from the main app
    observe({
      data_matrix <- data()
      if (!is.null(data_matrix)) {
        analyze_data(data_matrix)
        # If this is the first data loading, automatically show preprocessing
        if (is.null(rv$original_data)) {
          showPreprocessingDialog()
        }
      }
    })
    
    # Trigger preprocessing modal from button
    observeEvent(input$show_preprocess, {
      if (!is.null(rv$original_data)) {
        showPreprocessingDialog()
      } else {
        showNotification("No data available to preprocess.", type = "error")
      }
    })
    
    # Show the preprocessing dialog
    showPreprocessingDialog <- function() {
      # Calculate initial values for log constant if needed
      log_constant_default <- 0
      if (rv$has_negative) {
        log_constant_default <- 0
      } else if (!is.null(rv$original_data)) {
        flat_data <- as.numeric(rv$original_data)
        flat_data <- flat_data[!is.na(flat_data) & flat_data > 0]
        if (length(flat_data) > 0) {
          log_constant_default <- 0  # Default is 0 if all data > 0
        }
      }
      
      # Determine if log transform should be on by default based on skewness
      auto_log_transform <- rv$skewness > 10
      
      showModal(modalDialog(
        title = "Preprocess Data",
        
        fluidRow(
          column(6,
                 # Display data statistics
                 strong("Data Statistics:"),
                 tags$ul(
                   tags$li(paste("Matrix Size:", nrow(rv$original_data), "rows ×", ncol(rv$original_data), "columns")),
                   tags$li(paste("Missing Values:", ifelse(rv$has_missing, "Yes", "No"))),
                   tags$li(paste("Negative Values:", ifelse(rv$has_negative, "Yes", "No"))),
                   tags$li(paste("Skewness:", round(rv$skewness, 2)))
                 ),
                 
                 # Missing value handling (only show if missing values are detected)
                 conditionalPanel(
                   condition = "output.has_missing", ns = ns,
                   wellPanel(
                     h4("Missing Value Handling"),
                     radioButtons(ns("na_method"), "How to handle missing values:",
                                  choices = c("Leave as missing" = "leave",
                                              "Replace with zero" = "zero",
                                              "Replace with mean" = "mean",
                                              "Replace with median" = "median"),
                                  selected = "leave")
                   )
                 ),
                 
                 # Log transformation
                 wellPanel(
                   h4("Log Transformation"),
                   checkboxInput(ns("do_log_transform"), "Apply log10 transformation", 
                                 value = auto_log_transform),
                   conditionalPanel(
                     condition = "input.do_log_transform", ns = ns,
                     numericInput(ns("log_constant"), "Constant to add (c in log10(x + c)):",
                                  value = log_constant_default, min = 0)
                   )
                 ),
                 
                 # Centering and scaling
                 wellPanel(
                   h4("Data Centering and Scaling"),
                   radioButtons(ns("center_scale"), "Method:",
                                choices = c("None" = "none",
                                            "Center by row" = "center_row",
                                            "Scale by row (Z-score)" = "scale_row",
                                            "Center by column" = "center_col",
                                            "Scale by column (Z-score)" = "scale_col"),
                                selected = "center_row")
                 ),
                 
                 # Z-score cutoff for outlier capping
                 wellPanel(
                   h4("Outlier Capping"),
                   checkboxInput(ns("do_zscore_cap"), "Cap outliers based on Z-score", value = FALSE),
                   conditionalPanel(
                     condition = "input.do_zscore_cap", ns = ns,
                     numericInput(ns("zscore_cutoff"), "Z-score cutoff value:",
                                  value = 2, min = 0.1, step = 0.1)
                   )
                 )
          ),
          
          column(6,
                 # Dynamic histogram visualization
                 h4("Data Distribution"),
                 plotOutput(ns("data_histogram"), height = "280px"),
                 
                 # Data preview (before/after transformation)
                 h4("Data Preview (First 5 rows and columns)"),
                 DTOutput(ns("data_preview"))
          )
        ),
        
        footer = tagList(
          actionButton(ns("reset_all"), "Reset to Original", class = "btn-warning"),
          actionButton(ns("done"), "Apply Changes", class = "btn-success")
        ),
        
        size = "l",
        easyClose = FALSE
      ))
    }
    
    # Boolean output for conditional UI
    output$has_missing <- reactive({
      return(rv$has_missing)
    })
    outputOptions(output, "has_missing", suspendWhenHidden = FALSE)
    
    # Create histogram of current data values
    output$data_histogram <- renderPlot({
      req(rv$processed_data)
      
      # Flatten the matrix and remove NAs
      flat_data <- as.numeric(rv$processed_data)
      flat_data <- flat_data[!is.na(flat_data) & is.finite(flat_data)]
      
      if (length(flat_data) == 0) {
        # Handle case with no valid data
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No valid data to display") + 
                 theme_void())
      }
      
      # Create histogram
      ggplot(data.frame(value = flat_data), aes(x = value)) +
        geom_histogram(bins = 30, fill = "steelblue", color = "white") +
        labs(x = "Value", y = "Frequency", title = "Current Data Distribution") +
        theme_minimal()
    })
    
    # Show data preview in table
    output$data_preview <- renderDT({
      req(rv$processed_data)
      
      # Get preview of data (first 5 rows and columns)
      preview_rows <- min(5, nrow(rv$processed_data))
      preview_cols <- min(5, ncol(rv$processed_data))
      
      preview_data <- rv$processed_data[1:preview_rows, 1:preview_cols, drop = FALSE]
      
      datatable(
        preview_data,
        options = list(dom = 't', pageLength = 5),
        rownames = TRUE
      )
    })
    
    # Observe changes to controls and update preprocessing
    observeEvent(input$na_method, { apply_preprocessing() })
    observeEvent(input$do_log_transform, { apply_preprocessing() })
    observeEvent(input$log_constant, { apply_preprocessing() })
    observeEvent(input$center_scale, { apply_preprocessing() })
    observeEvent(input$do_zscore_cap, { apply_preprocessing() })
    observeEvent(input$zscore_cutoff, { apply_preprocessing() })
    
    # Reset to original data
    observeEvent(input$reset_all, {
      rv$processed_data <- rv$original_data
    })
    
    # Close modal and return processed data
    observeEvent(input$done, {
      removeModal()
    })
    
    # Return processed data
    return(list(
      processed_data = reactive({ rv$processed_data })
    ))
  })
}