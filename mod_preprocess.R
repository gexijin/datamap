# mod_preprocess.R
# A Shiny module for data preprocessing with visualization

library(shiny)
library(ggplot2)
library(DT)
library(e1071)  # For skewness calculation

#' UI function for preprocessing module
#'
#' @param id The module namespace id
#' @return A UI element containing the preprocessing button
#'
preprocessUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("show_preprocess"), "Preprocess Data", 
                 icon = icon("filter"),
                 class = "btn-primary"),
    downloadButton(ns("download_data"), "Download Data", 
                   class = "btn-info")
  )
}

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
#' @param data Reactive data frame to process
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
      has_zeros = FALSE,
      skewness = 0,
      original_data = NULL,
      original_data_matrix = NULL,  # Store the numeric matrix version
      data_range = NULL,
      log_constant_default = 1e-6,
      min_constant = 1e-6,
      top_n_default = 3000
    )
    
    analyze_data <- function(data_frame) {
      if (is.null(data_frame)) return()
      
      # Store the original data frame
      rv$original_data <- data_frame
      
      # Create a numeric matrix version for analysis and processing
      # Convert all columns to numeric
      numeric_data <- as.data.frame(lapply(data_frame, function(x) {
        as.numeric(as.character(x))
      }))
      
      # Convert to matrix for easier processing
      data_matrix <- as.matrix(numeric_data)
      rv$original_data_matrix <- data_matrix
      
      # Remove columns that are entirely NA (i.e. non-numeric) and notify user
      col_all_na <- apply(data_matrix, 2, function(x) all(is.na(x)))
      if (any(col_all_na)) {
        removed_cols <- colnames(data_matrix)[col_all_na]
        showNotification(
          paste("The following columns are non-numeric and have been removed:", 
                paste(removed_cols, collapse = ", ")),
          type = "warning"
        )
        data_matrix <- data_matrix[, !col_all_na, drop = FALSE]
      }
      
      # Compute statistics based on numeric data
      rv$has_missing <- any(is.na(data_matrix))
      rv$has_negative <- any(data_matrix < 0, na.rm = TRUE)
      rv$has_zeros <- any(data_matrix == 0, na.rm = TRUE)
      
      flat_data <- as.numeric(data_matrix)
      flat_data <- flat_data[!is.na(flat_data)]
      rv$data_range <- c(min(flat_data), max(flat_data))
      
      rv$skewness <- skewness(flat_data)
      
      # Set log constant based on data
      if (rv$has_zeros) {
        positive_values <- flat_data[flat_data > 0]
        if (length(positive_values) > 0) {
          percentile_10 <- quantile(positive_values, 0.1)
          rv$log_constant_default <- max(percentile_10, rv$min_constant)
        } else {
          rv$log_constant_default <- rv$min_constant
        }
      } else if (!rv$has_negative && min(flat_data, na.rm=TRUE) > 0) {
        rv$log_constant_default <- 0
      } else {
        rv$log_constant_default <- rv$min_constant
      }
      
      # Initial processed data is the cleaned input matrix
      rv$processed_data <- data_matrix
    }
    
    # Function to apply preprocessing based on current settings
    apply_preprocessing <- function() {
      if (is.null(rv$original_data_matrix)) return()
      
      # Start with the original data matrix
      processed <- rv$original_data_matrix
      
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
        if (rv$has_zeros || rv$has_negative) {
          const <- as.numeric(input$log_constant)
        } else {
          # If all values are positive (no zeros), use 0 as constant
          const <- 0
        }
        
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
        
        # To fix the hclust error, ensure we're not passing NAs or Infs
        processed <- pmin(pmax(processed, -1e300), 1e300)  # Cap extreme values
        
        if (input$center_scale %in% c("center_row", "scale_row")) {
          # Cap by row if row-wise centering/scaling was applied
          for (i in 1:nrow(processed)) {
            row_data <- processed[i, ]
            row_data <- row_data[is.finite(row_data)]  # Remove Inf/NaN
            if (length(row_data) > 0) {
              row_mean <- mean(row_data, na.rm = TRUE)
              row_sd <- sd(row_data, na.rm = TRUE)
              if (!is.na(row_sd) && row_sd > 0) {
                upper_bound <- row_mean + z_cutoff * row_sd
                lower_bound <- row_mean - z_cutoff * row_sd
                processed[i, processed[i,] > upper_bound] <- upper_bound
                processed[i, processed[i,] < lower_bound] <- lower_bound
              }
            }
          }
        } else if (input$center_scale %in% c("center_col", "scale_col", "none")) {
          # Cap by column if column-wise centering/scaling was applied or none
          for (j in 1:ncol(processed)) {
            col_data <- processed[, j]
            col_data <- col_data[is.finite(col_data)]  # Remove Inf/NaN
            if (length(col_data) > 0) {
              col_mean <- mean(col_data, na.rm = TRUE)
              col_sd <- sd(col_data, na.rm = TRUE)
              if (!is.na(col_sd) && col_sd > 0) {
                upper_bound <- col_mean + z_cutoff * col_sd
                lower_bound <- col_mean - z_cutoff * col_sd
                processed[processed[,j] > upper_bound, j] <- upper_bound
                processed[processed[,j] < lower_bound, j] <- lower_bound
              }
            }
          }
        }
      }
      
      # 5. Filter to keep only top N most variable rows (by SD)
      if (input$do_filter_rows) {
        # Calculate row standard deviations
        row_sds <- apply(processed, 1, sd, na.rm = TRUE)
        
        # Determine how many rows to keep
        top_n <- min(as.numeric(input$top_n_rows), nrow(processed))
        
        # Sort by SD and keep top rows
        if (top_n < nrow(processed)) {
          # Get indices of rows with highest SDs
          top_indices <- order(row_sds, decreasing = TRUE)[1:top_n]
          processed <- processed[top_indices, , drop = FALSE]
        }
      }
      
      rv$processed_data <- processed
    }
    
    # Update log constant when log transform is turned on
    observeEvent(input$do_log_transform, {
      if(input$do_log_transform && (rv$has_zeros || rv$has_negative)) {
        updateNumericInput(session, "log_constant", value = rv$log_constant_default)
      }
    })
    
    # Watch for data changes from the main app
    observe({
      data_frame <- data()
      if (!is.null(data_frame)) {
        analyze_data(data_frame)
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
      # Show modal dialog with controls
      showModal(modalDialog(
        title = "Preprocess Data",
        
        fluidRow(
          column(6,
                 # Missing value handling (only show if missing values are detected)
                 conditionalPanel(
                   condition = "output.has_missing", ns = ns,
                   wellPanel(
                     selectInput(ns("na_method"), "Missing value handling:",
                                 choices = c("Leave as missing" = "leave",
                                             "Replace with zero" = "zero",
                                             "Replace with mean" = "mean",
                                             "Replace with median" = "median"),
                                 selected = "leave")
                   )
                 ),
                 
                 # Log transformation
                 wellPanel(
                   checkboxInput(ns("do_log_transform"), "Apply log10 transformation", 
                                 value = !rv$has_negative && rv$skewness > 10),
                   conditionalPanel(
                     condition = "input.do_log_transform && output.needs_constant", ns = ns,
                     numericInput(ns("log_constant"), "Constant to add (c in log10(x + c)):",
                                  value = rv$log_constant_default, min = rv$min_constant)
                   )
                 ),
                 
                 # Centering and scaling - changed to selectInput
                 wellPanel(
                   selectInput(ns("center_scale"), "Centering and Scaling:",
                               choices = c("None" = "none",
                                           "Center by row" = "center_row",
                                           "Scale by row (Z-score)" = "scale_row",
                                           "Center by column" = "center_col",
                                           "Scale by column (Z-score)" = "scale_col"),
                               selected = "center_row")
                 ),
                 
                 # Z-score cutoff for outlier capping - now on by default
                 wellPanel(
                   checkboxInput(ns("do_zscore_cap"), "Cap outliers based on Z-score", value = TRUE),
                   conditionalPanel(
                     condition = "input.do_zscore_cap", ns = ns,
                     numericInput(ns("zscore_cutoff"), "Z-score cutoff value:",
                                  value = 2, min = 0.1, step = 0.1)
                   )
                 ),
                 
                 # Variable row filtering
                 wellPanel(
                   checkboxInput(ns("do_filter_rows"), "Keep top most variable rows", value = FALSE),
                   conditionalPanel(
                     condition = "input.do_filter_rows", ns = ns,
                     numericInput(ns("top_n_rows"), "Number of rows to keep:",
                                  value = rv$top_n_default, min = 1, max = 10000000, step = 100)
                   )
                 )
          ),
          
          column(6,
                 # Display data statistics moved to right side
                 wellPanel(
                   strong("Data Statistics:"),
                   tags$ul(
                     tags$li(paste("Matrix Size:", nrow(rv$original_data_matrix), "rows ×", ncol(rv$original_data_matrix), "columns")),
                     tags$li(paste("Missing Values:", ifelse(rv$has_missing, "Yes", "No"))),
                     tags$li(paste("Negative Values:", ifelse(rv$has_negative, "Yes", "No"))),
                     tags$li(paste("Skewness:", round(rv$skewness, 2))),
                     tags$li(paste("Data Range:", round(rv$data_range[1], 2), "to", round(rv$data_range[2], 2)))
                   )
                 ),
                 
                 # Dynamic histogram visualization
                 plotOutput(ns("data_histogram"), height = "280px"),
                 
                 # Data preview (before/after transformation)
                 h4("Data Preview (First 5 rows and columns)"),
                 DTOutput(ns("data_preview")),
                 
                 # Download button for transformed data
                 tags$div(
                   style = "margin-top: 15px;",
                   downloadButton(ns("download_data"), "Download Transformed Data", class = "btn-info")
                 )
          )
        ),
        
        footer = tagList(
          actionButton(ns("cancel"), "Cancel", class = "btn-default"),
          actionButton(ns("reset_all"), "Reset to Original", class = "btn-warning"),
          actionButton(ns("done"), "Apply Changes", class = "btn-success")
        ),
        
        size = "l",
        easyClose = TRUE
      ))
    }
    
    # Boolean outputs for conditional UI
    output$has_missing <- reactive({
      return(rv$has_missing)
    })
    outputOptions(output, "has_missing", suspendWhenHidden = FALSE)
    
    output$needs_constant <- reactive({
      return(rv$has_zeros || rv$has_negative)
    })
    outputOptions(output, "needs_constant", suspendWhenHidden = FALSE)
    
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
    observeEvent(input$do_filter_rows, { apply_preprocessing() })
    observeEvent(input$top_n_rows, { apply_preprocessing() })
    
    # Reset to original data and reset all controls to default "do nothing" state
    observeEvent(input$reset_all, {
      # Reset processed data to original
      rv$processed_data <- rv$original_data_matrix
      
      # Reset all controls to default values that do no processing
      if(rv$has_missing) {
        updateRadioButtons(session, "na_method", selected = "leave")
      }
      
      updateCheckboxInput(session, "do_log_transform", value = FALSE)
      
      updateSelectInput(session, "center_scale", selected = "none")
      
      updateCheckboxInput(session, "do_zscore_cap", value = FALSE)
      updateNumericInput(session, "zscore_cutoff", value = 2)
      
      updateCheckboxInput(session, "do_filter_rows", value = FALSE)
      updateNumericInput(session, "top_n_rows", value = rv$top_n_default)
    })
    
    # Cancel button closes the modal without applying changes
    observeEvent(input$cancel, {
      removeModal()
    })
    
    # Close modal and return processed data
    observeEvent(input$done, {
      removeModal()
    })
    
    # Download handler for transformed data
    output$download_data <- downloadHandler(
      filename = function() {
        paste("transformed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
      },
      content = function(file) {
        if (is.null(rv$processed_data)) {
          # If no processed data exists, use original data
          data_to_save <- rv$original_data_matrix
        } else {
          data_to_save <- rv$processed_data
        }
        
        # Convert to data frame for easier writing
        data_df <- as.data.frame(data_to_save)
        
        # Write to CSV
        write.csv(data_df, file, row.names = TRUE)
      }
    )
    
    # Return processed data
    return(list(
      processed_data = reactive({ rv$processed_data })
    ))
  })
}