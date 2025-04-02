# mod_transform.R
library(shiny)
library(e1071)  # For skewness calculation

data_transforms <- c(
  "None" = "none",
  "Center by row" = "center_row",
  "Scale by row (Z-score)" = "scale_row",
  "Center by column" = "center_col",
  "Scale by column (Z-score)" = "scale_col"
)

# Helper function to map the integer code from guess_transform to the corresponding transformation string
map_transform_code <- function(code) {
  switch(as.character(code),
         "0" = "none",
         "2" = "center_row",
         "3" = "scale_row",
         "4" = "center_col",
         "5" = "scale_col",
         "none")  # Default if code doesn't match
}

#' UI function for preprocessing module button
#'
#' @param id The module namespace id
#' @return A button that triggers the preprocessing modal
#'
transform_ui <- function(id) {
  ns <- NS(id)
  actionButton(ns("show_preprocess"), "Transform Data", 
               icon = icon("filter"),
               class = "btn-primary")
}

#' Server function for preprocessing module
#'
#' @param id The module namespace id
#' @param data Reactive data frame to process
#' @return A list with the processed data reactive
#'
transform_server <- function(id, data) {
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
      top_n_default = 3000,
      current_settings = NULL,      # Track current transformation settings
      applied_settings = NULL,      # Track applied transformation settings
      dialog_shown = FALSE,         # Track if dialog has been shown
      changes_applied = FALSE,      # Track if changes have been applied
      modal_closed = TRUE,  
      ui_settings = list(           # Store UI input values to preserve between sessions
        na_method = "leave",
        do_log_transform = FALSE,
        log_constant = NULL,
        center_scale = "none",
        do_zscore_cap = TRUE,
        zscore_cutoff = 2,
        do_filter_rows = TRUE,      # Changed to TRUE as default
        top_n_rows = NULL
      )
    )
    
    analyze_data <- function(data_frame) {
      if (is.null(data_frame)) return()
      
      # Store the original data frame
      rv$original_data <- data_frame
      
      # Create a numeric matrix version for analysis and processing
      numeric_data <- as.data.frame(lapply(data_frame, function(x) {
        as.numeric(as.character(x))
      }))
      
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
        rv$original_data_matrix <- data_matrix
      }
      
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
      } else if (!rv$has_negative && min(flat_data, na.rm = TRUE) > 0) {
        rv$log_constant_default <- 0
      } else {
        rv$log_constant_default <- rv$min_constant
      }
      
      if (is.null(rv$ui_settings$log_constant)) {
        rv$ui_settings$log_constant <- rv$log_constant_default
      }
      
      if (is.null(rv$ui_settings$top_n_rows)) {
        rv$ui_settings$top_n_rows <- rv$top_n_default
      }
      
      rv$processed_data <- data_matrix
    }
    
    capture_current_settings <- function() {
      settings <- list()
      if (rv$has_missing && !is.null(input$na_method)) {
        settings$na_method <- input$na_method
      } else {
        settings$na_method <- "leave"
      }
      
      if (!is.null(input$do_log_transform)) {
        settings$do_log_transform <- input$do_log_transform
        if (settings$do_log_transform && !is.null(input$log_constant)) {
          settings$log_constant <- input$log_constant
        }
      } else {
        settings$do_log_transform <- FALSE
      }
      
      if (!is.null(input$center_scale)) {
        settings$center_scale <- input$center_scale
      } else {
        settings$center_scale <- "none"
      }
      
      if (!is.null(input$do_zscore_cap)) {
        settings$do_zscore_cap <- input$do_zscore_cap
        if (settings$do_zscore_cap && !is.null(input$zscore_cutoff)) {
          settings$zscore_cutoff <- input$zscore_cutoff
        }
      } else {
        settings$do_zscore_cap <- FALSE
      }
      
      if (!is.null(input$do_filter_rows)) {
        settings$do_filter_rows <- input$do_filter_rows
        if (settings$do_filter_rows && !is.null(input$top_n_rows)) {
          settings$top_n_rows <- input$top_n_rows
        }
      } else {
        settings$do_filter_rows <- TRUE
      }
      
      return(settings)
    }
    
    update_ui_settings <- function() {
      if (!is.null(input$na_method)) {
        rv$ui_settings$na_method <- input$na_method
      }
      
      if (!is.null(input$do_log_transform)) {
        rv$ui_settings$do_log_transform <- input$do_log_transform
        if (!is.null(input$log_constant)) {
          rv$ui_settings$log_constant <- input$log_constant
        }
      }
      
      if (!is.null(input$center_scale)) {
        rv$ui_settings$center_scale <- input$center_scale
      }
      
      if (!is.null(input$do_zscore_cap)) {
        rv$ui_settings$do_zscore_cap <- input$do_zscore_cap
        if (!is.null(input$zscore_cutoff)) {
          rv$ui_settings$zscore_cutoff <- input$zscore_cutoff
        }
      }
      
      if (!is.null(input$do_filter_rows)) {
        rv$ui_settings$do_filter_rows <- input$do_filter_rows
        if (!is.null(input$top_n_rows)) {
          rv$ui_settings$top_n_rows <- input$top_n_rows
        }
      }
    }
    
    settings_have_changed <- function(settings1, settings2) {
      if (is.null(settings1) || is.null(settings2)) return(TRUE)
      (!identical(settings1$na_method, settings2$na_method)) ||
      (!identical(settings1$do_log_transform, settings2$do_log_transform)) ||
      (!identical(settings1$log_constant, settings2$log_constant)) ||
      (!identical(settings1$center_scale, settings2$center_scale)) ||
      (!identical(settings1$do_zscore_cap, settings2$do_zscore_cap)) ||
      (!identical(settings1$zscore_cutoff, settings2$zscore_cutoff)) ||
      (!identical(settings1$do_filter_rows, settings2$do_filter_rows)) ||
      (!identical(settings1$top_n_rows, settings2$top_n_rows))
    }
    
    apply_preprocessing <- function() {
      if (is.null(rv$original_data_matrix)) return()
      processed <- rv$original_data_matrix
      
      # 1. Handle missing values
      if (rv$has_missing) {
        row_all_na <- apply(processed, 1, function(x) all(is.na(x)))
        if (any(row_all_na)) {
          processed <- processed[!row_all_na, , drop = FALSE]
        }
        col_all_na <- apply(processed, 2, function(x) all(is.na(x)))
        if (any(col_all_na)) {
          processed <- processed[, !col_all_na, drop = FALSE]
        }
        if (!is.null(input$na_method) && input$na_method != "leave") {
          if (input$na_method == "zero") {
            processed[is.na(processed)] <- 0
          } else if (input$na_method == "mean") {
            for (j in 1:ncol(processed)) {
              col_mean <- mean(processed[, j], na.rm = TRUE)
              processed[is.na(processed[, j]), j] <- col_mean
            }
          } else if (input$na_method == "median") {
            for (j in 1:ncol(processed)) {
              col_median <- median(processed[, j], na.rm = TRUE)
              processed[is.na(processed[, j]), j] <- col_median
            }
          }
        }
      }
      
      # 2. Apply log transformation if selected
      if (!is.null(input$do_log_transform) && input$do_log_transform) {
        if (any(processed < 0, na.rm = TRUE)) {
          processed[processed < 0] <- 0
        }
        if (rv$has_zeros || rv$has_negative) {
          const <- as.numeric(input$log_constant)
        } else {
          const <- 0
        }
        processed <- log10(processed + const)
      }
      
      # 3. Apply centering and scaling
      if (!is.null(input$center_scale) && input$center_scale != "none") {
        if (input$center_scale == "center_row") {
          row_means <- rowMeans(processed, na.rm = TRUE)
          processed <- t(t(processed) - row_means)
        } else if (input$center_scale == "scale_row") {
          row_means <- rowMeans(processed, na.rm = TRUE)
          row_sds <- apply(processed, 1, sd, na.rm = TRUE)
          processed <- t((t(processed) - row_means) / ifelse(row_sds == 0, 1, row_sds))
          zero_sd_rows <- which(row_sds == 0)
          if (length(zero_sd_rows) > 0) {
            processed[zero_sd_rows, ] <- 0
          }
        } else if (input$center_scale == "center_col") {
          col_means <- colMeans(processed, na.rm = TRUE)
          processed <- t(t(processed) - col_means)
        } else if (input$center_scale == "scale_col") {
          col_means <- colMeans(processed, na.rm = TRUE)
          col_sds <- apply(processed, 2, sd, na.rm = TRUE)
          processed <- sweep(processed, 2, col_means, "-")
          processed <- sweep(processed, 2, ifelse(col_sds == 0, 1, col_sds), "/")
          zero_sd_cols <- which(col_sds == 0)
          if (length(zero_sd_cols) > 0) {
            processed[, zero_sd_cols] <- 0
          }
        }
      }
      
      # 4. Apply Z-score cutoff for outlier capping (applied to the entire matrix)
      if (!is.null(input$do_zscore_cap) && input$do_zscore_cap) {
        z_cutoff <- as.numeric(input$zscore_cutoff)
        processed <- pmin(pmax(processed, -1e300), 1e300)
        flat_data <- as.numeric(processed)
        flat_data <- flat_data[is.finite(flat_data)]
        if (length(flat_data) > 0) {
          overall_mean <- mean(flat_data, na.rm = TRUE)
          overall_sd <- sd(flat_data, na.rm = TRUE)
          if (!is.na(overall_sd) && overall_sd > 0) {
            upper_bound <- overall_mean + z_cutoff * overall_sd
            lower_bound <- overall_mean - z_cutoff * overall_sd
            processed[processed > upper_bound] <- upper_bound
            processed[processed < lower_bound] <- lower_bound
          }
        }
      }
      
      # 5. Filter to keep only top N most variable rows (by SD)
      if (!is.null(input$do_filter_rows) && input$do_filter_rows) {
        row_sds <- apply(processed, 1, sd, na.rm = TRUE)
        top_n <- min(as.numeric(input$top_n_rows), nrow(processed))
        if (top_n < nrow(processed)) {
          top_indices <- order(row_sds, decreasing = TRUE)[1:top_n]
          processed <- processed[top_indices, , drop = FALSE]
        }
      }
      
      rv$processed_data <- processed
    }
    
    observeEvent(input$do_log_transform, {
      if(input$do_log_transform && (rv$has_zeros || rv$has_negative)) {
        updateNumericInput(session, "log_constant", value = rv$log_constant_default)
      }
    })
    
    observe({
      data_frame <- data()
      if (!is.null(data_frame) && !identical(data_frame, rv$original_data)) {
        analyze_data(data_frame)
        if (!rv$dialog_shown) {
          rv$ui_settings$do_log_transform <- !rv$has_negative && rv$skewness > 1
          rv$ui_settings$center_scale <- map_transform_code(guess_transform(rv$processed_data))
          showPreprocessingDialog()
          rv$dialog_shown <- TRUE
        }
      }
    })
    
    observeEvent(input$show_preprocess, {
      if (!is.null(rv$original_data)) {
        showPreprocessingDialog()
      } else {
        showNotification("No data available to preprocess.", type = "error")
      }
    })
    
    # Show the preprocessing dialog. Use guess_transform only the first time.
    showPreprocessingDialog <- function() {
     rv$modal_closed <- FALSE  # Modal is now open

      if (!rv$dialog_shown) {
        rv$ui_settings$center_scale <- map_transform_code(guess_transform(rv$processed_data))
      }
      
      showModal(modalDialog(
        title = "Transform Data",
        fluidRow(
          column(6,
                 conditionalPanel(
                   condition = "output.has_missing", ns = ns,
                   wellPanel(
                     selectInput(ns("na_method"), "Missing value handling:",
                                 choices = c("Leave as missing" = "leave",
                                             "Replace with zero" = "zero",
                                             "Replace with mean" = "mean",
                                             "Replace with median" = "median"),
                                 selected = rv$ui_settings$na_method)
                   )
                 ),
                 
                 wellPanel(
                   checkboxInput(ns("do_log_transform"), "Apply log10 transformation", 
                                 value = rv$ui_settings$do_log_transform),
                   conditionalPanel(
                     condition = "input.do_log_transform && output.needs_constant", ns = ns,
                     numericInput(ns("log_constant"), "Constant to add (c in log10(x + c)):",
                                  value = rv$ui_settings$log_constant, min = rv$min_constant)
                   )
                 ),
                 
                 # Use the recommended setting (or last user setting) in the select input below
                 wellPanel(
                   selectInput(ns("center_scale"), "Centering and Scaling:",
                               choices = data_transforms,
                               selected = rv$ui_settings$center_scale)
                 ),
                 
                 wellPanel(
                   checkboxInput(ns("do_zscore_cap"), "Cap outliers based on Z-score", 
                                 value = rv$ui_settings$do_zscore_cap),
                   conditionalPanel(
                     condition = "input.do_zscore_cap", ns = ns,
                     numericInput(ns("zscore_cutoff"), "Z-score cutoff value:",
                                  value = rv$ui_settings$zscore_cutoff, min = 0.1, step = 0.1)
                   )
                 ),
                 
                 wellPanel(
                   checkboxInput(ns("do_filter_rows"), "Keep top most variable rows", 
                                 value = rv$ui_settings$do_filter_rows),
                   conditionalPanel(
                     condition = "input.do_filter_rows", ns = ns,
                     numericInput(ns("top_n_rows"), "Number of rows to keep:",
                                  value = rv$ui_settings$top_n_rows, min = 1, max = 10000000, step = 100)
                   )
                 )
          ),
          
          column(6,
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
                 
                 plotOutput(ns("data_histogram"), height = "400px")
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
      
      rv$current_settings <- capture_current_settings()
    }
    
    output$has_missing <- reactive({
      return(rv$has_missing)
    })
    outputOptions(output, "has_missing", suspendWhenHidden = FALSE)
    
    output$needs_constant <- reactive({
      return(rv$has_zeros || rv$has_negative)
    })
    outputOptions(output, "needs_constant", suspendWhenHidden = FALSE)
    
    output$data_histogram <- renderPlot({
      req(rv$processed_data)
      flat_data <- as.numeric(rv$processed_data)
      flat_data <- flat_data[!is.na(flat_data) & is.finite(flat_data)]
      
      if (length(flat_data) == 0) {
        plot.new()
        text(0.5, 0.5, "No valid data to display", cex = 1.2)
        return()
      }
      
      hist(flat_data, breaks = 30, col = "steelblue", border = "white",
          main = "Current Data Distribution", xlab = "Value")

    })
    
    observeEvent(input$na_method, { apply_preprocessing() })
    observeEvent(input$do_log_transform, { apply_preprocessing() })
    observeEvent(input$log_constant, { apply_preprocessing() })
    observeEvent(input$center_scale, { apply_preprocessing() })
    observeEvent(input$do_zscore_cap, { apply_preprocessing() })
    observeEvent(input$zscore_cutoff, { apply_preprocessing() })
    observeEvent(input$do_filter_rows, { apply_preprocessing() })
    observeEvent(input$top_n_rows, { apply_preprocessing() })
    
    observe({
      if (!is.null(input$na_method)) {
        update_ui_settings()
      }
    })
    
    observeEvent(input$reset_all, {
      rv$processed_data <- rv$original_data_matrix
      
      if(rv$has_missing) {
        updateSelectInput(session, "na_method", selected = "leave")
        rv$ui_settings$na_method <- "leave"
      }
      
      updateCheckboxInput(session, "do_log_transform", value = FALSE)
      rv$ui_settings$do_log_transform <- FALSE
      
      updateSelectInput(session, "center_scale", selected = "none")
      rv$ui_settings$center_scale <- "none"
      
      updateCheckboxInput(session, "do_zscore_cap", value = FALSE)
      rv$ui_settings$do_zscore_cap <- FALSE
      updateNumericInput(session, "zscore_cutoff", value = 2)
      rv$ui_settings$zscore_cutoff <- 2
      
      updateCheckboxInput(session, "do_filter_rows", value = TRUE)
      rv$ui_settings$do_filter_rows <- TRUE
      updateNumericInput(session, "top_n_rows", value = rv$top_n_default)
      rv$ui_settings$top_n_rows <- rv$top_n_default
    })
    
    observeEvent(input$cancel, {
      update_ui_settings()
      removeModal()
      rv$modal_closed <- TRUE 
    })
    
    observeEvent(input$done, {
      update_ui_settings()
      final_settings <- capture_current_settings()
      
      if (settings_have_changed(final_settings, rv$applied_settings)) {
        apply_preprocessing()
        rv$applied_settings <- final_settings
        rv$changes_applied <- TRUE
        showNotification("Transformations applied successfully", type = "message")
      } else {
        showNotification("No changes detected in transformation settings", type = "message")
      }
      
      removeModal()
      rv$modal_closed <- TRUE
    })
    
    return(list(
      processed_data = reactive({ 
        if (rv$changes_applied) {
          rv$processed_data 
        } else {
          NULL
        }
      }),
      has_transformed = reactive({ rv$changes_applied }),
      modal_closed = reactive({ rv$modal_closed }) 
    ))
  })
}
