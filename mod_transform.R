library(shiny)

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
#' @return A list with the processed data reactive and reproducible code
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
      factor_columns = NULL,
      code = NULL,     # NEW: String to store reproducible R code
      ui_settings = list(           # Store UI input values to preserve between sessions
        na_method = "zero",
        do_log_transform = FALSE,
        log_constant = NULL,
        center_scale = "none",
        do_zscore_cap = TRUE,
        zscore_cutoff = 3,
        do_filter_rows = TRUE,      # Changed to TRUE as default
        top_n_rows = NULL
      )
    )
    
    analyze_data <- function(data_frame) {
      if (is.null(data_frame)) return()
      
      # Store the original data frame
      rv$original_data <- data_frame
      original_row_names <- rownames(data_frame)
      
      factor_like_cols <- list()
      for (col_name in names(data_frame)) {
        col_data <- data_frame[[col_name]]
        
        # Check if column is character or factor
        if ((is.character(col_data) || is.factor(col_data)) && 
            length(unique(col_data)) < min(50, nrow(data_frame) * 0.5)) {
          factor_like_cols[[col_name]] <- col_data
        }
      }
      
      # If factor-like columns were found, store them
      if (length(factor_like_cols) > 0) {
        rv$factor_columns <- as.data.frame(factor_like_cols, check.names = FALSE)
        rownames(rv$factor_columns) <- rownames(data_frame)
        
        # Remove factor columns from the data_frame before numeric conversion
        data_frame <- data_frame[, !names(data_frame) %in% names(factor_like_cols), drop = FALSE]
      } else {
        rv$factor_columns <- NULL
      }
      
      # Create a numeric matrix version for analysis and processing
      numeric_data <- as.data.frame(
        lapply(data_frame, function(x) as.numeric(as.character(x))),
        row.names = original_row_names
      )
      
      data_matrix <- as.matrix(numeric_data)
      rownames(data_matrix) <- original_row_names
      
      # Remove rows that are completely missing
      row_na_count <- rowSums(is.na(data_matrix))
      row_all_na <- row_na_count == ncol(data_matrix)
      if (any(row_all_na)) {
        data_matrix <- data_matrix[!row_all_na, , drop = FALSE]
      }
      
      # Remove columns that are entirely NA (i.e. non-numeric) and notify user
      col_na_count <- colSums(is.na(data_matrix))
      col_all_na <- col_na_count == nrow(data_matrix)
      rv$original_data_matrix <- data_matrix

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
      
      rv$skewness <- e1071::skewness(flat_data)
      
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
    
    # Function to generate reproducible R code
    generate_code <- function(settings) {
      code_lines <- c(
        "# Reproducible Data Transformation Code",
        "# This code will transform your data using the same settings applied in the app",
        "",
        "# Load required package for skewness calculation if needed",
        "if (!requireNamespace(\"e1071\", quietly = TRUE)) {",
        "  install.packages(\"e1071\")",
        "}",
        "library(e1071)",
        "",
        "transform_data <- function(data) {",
        "  # Create a copy of the data to avoid modifying the original",
        "  processed <- data",
        "",
        "  # Step 1: Convert data frame to numeric matrix and handle factor columns",
        "  original_row_names <- rownames(processed)",
        "  factor_like_cols <- list()",
        "  ",
        "  for (col_name in names(processed)) {",
        "    col_data <- processed[[col_name]]",
        "    if ((is.character(col_data) || is.factor(col_data)) && ",
        "        length(unique(col_data)) < min(50, nrow(processed) * 0.5)) {",
        "      factor_like_cols[[col_name]] <- col_data",
        "    }",
        "  }",
        "  ",
        "  # Remove factor columns before numeric conversion",
        "  if (length(factor_like_cols) > 0) {",
        "    factor_columns <- as.data.frame(factor_like_cols, check.names = FALSE)",
        "    rownames(factor_columns) <- rownames(processed)",
        "    processed <- processed[, !names(processed) %in% names(factor_like_cols), drop = FALSE]",
        "  }",
        "  ",
        "  # Convert to numeric matrix",
        "  numeric_data <- as.data.frame(",
        "    lapply(processed, function(x) as.numeric(as.character(x))),",
        "    row.names = original_row_names",
        "  )",
        "  ",
        "  processed <- as.matrix(numeric_data)",
        "  rownames(processed) <- original_row_names",
        "  ",
        "  # Remove rows that are completely missing",
        "  row_na_count <- rowSums(is.na(processed))",
        "  row_all_na <- row_na_count == ncol(processed)",
        "  if (any(row_all_na)) {",
        "    processed <- processed[!row_all_na, , drop = FALSE]",
        "  }",
        "  ",
        "  # Remove columns that are entirely NA",
        "  col_na_count <- colSums(is.na(processed))",
        "  col_all_na <- col_na_count == nrow(processed)",
        "  if (any(col_all_na)) {",
        "    removed_cols <- colnames(processed)[col_all_na]",
        "    cat(\"Removed non-numeric columns:\", paste(removed_cols, collapse = \", \"), \"\\n\")",
        "    processed <- processed[, !col_all_na, drop = FALSE]",
        "  }"
      )
      
      # Handle missing values
      if (settings$na_method != "leave") {
        code_lines <- c(code_lines, "", "  # Handle missing values")
        if (settings$na_method == "zero") {
          code_lines <- c(code_lines, "  processed[is.na(processed)] <- 0")
        } else if (settings$na_method == "mean") {
          code_lines <- c(code_lines, 
                          "  means <- colMeans(processed, na.rm = TRUE)",
                          "  idx <- which(is.na(processed), arr.ind = TRUE)",
                          "  processed[idx] <- means[idx[, 2]]")
        } else if (settings$na_method == "median") {
          code_lines <- c(code_lines,
                          "  medians <- apply(processed, 2, median, na.rm = TRUE)",
                          "  idx <- which(is.na(processed), arr.ind = TRUE)",
                          "  processed[idx] <- medians[idx[, 2]]")
        }
      }
      
      # Apply log transformation
      if (settings$do_log_transform) {
        code_lines <- c(code_lines, "", "  # Apply log10 transformation")
        code_lines <- c(code_lines, "  if (any(processed < 0, na.rm = TRUE)) {",
                        "    processed[processed < 0] <- 0",
                        "  }")
        code_lines <- c(code_lines, paste0("  const <- ", settings$log_constant))
        code_lines <- c(code_lines, "  processed <- log10(processed + const)")
      }
      
      # Filter to keep only top N most variable rows
      if (settings$do_filter_rows) {
        code_lines <- c(code_lines, "", "  # Keep top most variable rows")
        code_lines <- c(code_lines, "  row_sds <- apply(processed, 1, sd, na.rm = TRUE)")
        code_lines <- c(code_lines, paste0("  top_n <- ", settings$top_n_rows))
        code_lines <- c(code_lines, 
                        "  if (top_n < nrow(processed)) {",
                        "    top_indices <- order(row_sds, decreasing = TRUE)[1:top_n]",
                        "    processed <- processed[top_indices, , drop = FALSE]",
                        "  }")
      }
      
      # Apply centering and scaling
      if (settings$center_scale != "none") {
        code_lines <- c(code_lines, "", paste0("  # Apply ", settings$center_scale, " transformation"))
        
        if (settings$center_scale == "center_row") {
          code_lines <- c(code_lines,
                          "  row_means <- rowMeans(processed, na.rm = TRUE)",
                          "  processed <- sweep(processed, 1, row_means, \"-\")")
        } else if (settings$center_scale == "scale_row") {
          code_lines <- c(code_lines,
                          "  # Scale each row (Z-score normalization)",
                          "  processed <- t(scale(t(processed)))",
                          "  # Replace any NA values (from constant rows) with 0",
                          "  processed[is.na(processed)] <- 0")
        } else if (settings$center_scale == "center_col") {
          code_lines <- c(code_lines,
                          "  col_means <- colMeans(processed, na.rm = TRUE)",
                          "  processed <- t(t(processed) - col_means)")
        } else if (settings$center_scale == "scale_col") {
          code_lines <- c(code_lines,
                          "  processed <- scale(processed)",
                          "  processed[is.na(processed)] <- 0")
        }
      }
      
      # Apply Z-score cutoff for outlier capping
      if (settings$do_zscore_cap) {
        code_lines <- c(code_lines, "", "  # Cap outliers based on Z-score")
        code_lines <- c(code_lines, paste0("  z_cutoff <- ", settings$zscore_cutoff))
        code_lines <- c(code_lines,
                        "  flat_data <- as.numeric(processed)",
                        "  flat_data <- flat_data[is.finite(flat_data)]",
                        "  if (length(flat_data) > 0) {",
                        "    overall_mean <- mean(flat_data, na.rm = TRUE)",
                        "    overall_sd <- sd(flat_data, na.rm = TRUE)",
                        "    if (!is.na(overall_sd) && overall_sd > 0) {",
                        "      upper_bound <- overall_mean + z_cutoff * overall_sd",
                        "      lower_bound <- overall_mean - z_cutoff * overall_sd",
                        "      processed[processed > upper_bound] <- upper_bound",
                        "      processed[processed < lower_bound] <- lower_bound",
                        "    }",
                        "  }")
      }
      
      # Finish the function and add usage example
      code_lines <- c(code_lines,
                      "",
                      "  # Merge back factor columns if any were extracted",
                      "  if (exists(\"factor_columns\")) {",
                      "    result_list <- list(numeric_data = processed, factor_columns = factor_columns)",
                      "    return(result_list)",
                      "  } else {",
                      "    return(processed)",
                      "  }",
                      "}",
                      "",
                      "transformed_data <- transform_data(data)")
      
      return(paste(code_lines, collapse = "\n"))
    }
    
    # Consolidated function to update and capture UI settings
    update_and_capture_ui_settings <- function() {
      settings <- list(
        na_method = if (!is.null(input$na_method)) input$na_method else "zero",
        do_log_transform = if (!is.null(input$do_log_transform)) input$do_log_transform else FALSE,
        log_constant = if (!is.null(input$log_constant)) input$log_constant else rv$ui_settings$log_constant,
        center_scale = if (!is.null(input$center_scale)) input$center_scale else "none",
        do_zscore_cap = if (!is.null(input$do_zscore_cap)) input$do_zscore_cap else FALSE,
        zscore_cutoff = if (!is.null(input$zscore_cutoff)) input$zscore_cutoff else rv$ui_settings$zscore_cutoff,
        do_filter_rows = if (!is.null(input$do_filter_rows)) input$do_filter_rows else TRUE,
        top_n_rows = if (!is.null(input$top_n_rows)) input$top_n_rows else rv$ui_settings$top_n_rows
      )
      rv$ui_settings <- settings
      return(settings)
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
      
      # Create a progress object
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      
      # Set up progress
      progress$set(message = "Applying transformations", value = 0)
      
      processed <- rv$original_data_matrix
      
      # 1. Handle missing values
      if (rv$has_missing) {
        progress$set(value = 0.1, detail = "Handling missing values")
        if (!is.null(input$na_method) && input$na_method != "leave") {
          if (input$na_method == "zero") {
            processed[is.na(processed)] <- 0
          } else if (input$na_method == "mean") {
            means <- colMeans(processed, na.rm = TRUE)
            idx <- which(is.na(processed), arr.ind = TRUE)
            processed[idx] <- means[idx[, 2]]
          } else if (input$na_method == "median") {
            medians <- apply(processed, 2, median, na.rm = TRUE)
            idx <- which(is.na(processed), arr.ind = TRUE)
            processed[idx] <- medians[idx[, 2]]
          }
        }
      }

      # 2. Apply log transformation if selected
      if (!is.null(input$do_log_transform) && input$do_log_transform) {
        progress$set(value = 0.3, detail = "Applying log transformation")
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

      # 3. Filter to keep only top N most variable rows (by SD)
      if (!is.null(input$do_filter_rows) && input$do_filter_rows) {
        progress$set(value = 0.9, detail = "Filtering rows by variability")
        row_sds <- apply(processed, 1, sd, na.rm = TRUE)
        top_n_input <- input$top_n_rows
        if (is.null(top_n_input) || is.na(top_n_input) || !is.numeric(top_n_input)) {
          top_n <- 2  # Default to 2 if the input is invalid
        } else {
          top_n <- max(2, min(as.numeric(top_n_input), nrow(processed)))
        }
        if (top_n < nrow(processed)) {
          top_indices <- order(row_sds, decreasing = TRUE)[1:top_n]
          processed <- processed[top_indices, , drop = FALSE]
        }
      }

      # 4. Apply centering and scaling
      if (!is.null(input$center_scale) && input$center_scale != "none") {
        progress$set(value = 0.5, detail = paste("Applying", input$center_scale, "transformation"))
        if (input$center_scale == "center_row") {
          row_means <- rowMeans(processed, na.rm = TRUE)
          processed <- sweep(processed, 1, row_means, "-")
        } else if (input$center_scale == "scale_row") {
          # Scale each row so that it has zero mean and unit variance.
          processed <- t(scale(t(processed)))
          # Replace any NA values produced (e.g. from constant rows) with 0
          processed[is.na(processed)] <- 0
        } else if (input$center_scale == "center_col") {
          col_means <- colMeans(processed, na.rm = TRUE)
          processed <- t(t(processed) - col_means)
        } else if (input$center_scale == "scale_col") {
          processed <- scale(processed)
          processed[is.na(processed)] <- 0
        }
      }
      
      # 5. Apply Z-score cutoff for outlier capping (applied to the entire matrix)
      if (!is.null(input$do_zscore_cap) && input$do_zscore_cap) {
        progress$set(value = 0.7, detail = "Capping outliers")
        z_input <- input$zscore_cutoff
        if (is.null(z_input) || is.na(z_input) || !is.numeric(z_input)) {
          z_cutoff <- 1  # Default to 1 if the input is invalid
        } else {
          z_cutoff <- max(1, as.numeric(z_input))
        }
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
      progress$set(value = 1, detail = "Transformations complete")
      
      # Generate reproducible code
      rv$code <- generate_code(update_and_capture_ui_settings())
      
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
        # Determine recommended transformation
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
                                  value = rv$ui_settings$zscore_cutoff, min = 1, step = 1)
                   )
                 ),
                 
                 wellPanel(
                   checkboxInput(ns("do_filter_rows"), "Keep top most variable rows", 
                                 value = rv$ui_settings$do_filter_rows),
                   conditionalPanel(
                     condition = "input.do_filter_rows", ns = ns,
                     numericInput(ns("top_n_rows"), "Number of rows to keep:",
                                  value = rv$ui_settings$top_n_rows, min = 2, max = 10000000, step = 100)
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
      
      rv$current_settings <- update_and_capture_ui_settings()
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
    
    # Use debounced event handlers to reduce recalculation frequency
    # for UI inputs that might change rapidly
    observeEvent(list(
      input$na_method, 
      input$do_log_transform, 
      input$log_constant, 
      input$center_scale, 
      input$do_zscore_cap,
      input$zscore_cutoff,
      input$do_filter_rows,
      input$top_n_rows
    ), { 
        req(rv$original_data_matrix) 
        apply_preprocessing()
    })
    observe({
      if (!is.null(input$na_method)) {
        update_and_capture_ui_settings()
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
      
      # Reset reproducible code
      rv$code <- generate_code(rv$ui_settings)
    })
    
    observeEvent(input$cancel, {
      update_and_capture_ui_settings()
      removeModal()
      rv$modal_closed <- TRUE 
    })
    
    observeEvent(input$done, {
      update_and_capture_ui_settings()
      final_settings <- update_and_capture_ui_settings()
      
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

    # Safe observer for top_n_rows that handles missing values first
    observeEvent(input$top_n_rows, {
      req(rv$processed_data)
      # First check if the value exists and is a valid number
      if (is.null(input$top_n_rows) || is.na(input$top_n_rows) || !is.numeric(input$top_n_rows)) {
        updateNumericInput(session, "top_n_rows", value = 2)
        showNotification("Number of rows must be a valid number (minimum 2)", type = "warning")
      } 
      # Then check if it's below the minimum
      else if (input$top_n_rows < 2) {
        updateNumericInput(session, "top_n_rows", value = 2)
        showNotification("Number of rows to keep must be at least 2", type = "warning")
      }
    }, ignoreNULL = FALSE)  # Important: don't ignore NULL values

    # Safe observer for zscore_cutoff
    observeEvent(input$zscore_cutoff, {
      req(rv$processed_data)
      # First check if the value exists and is a valid number
      if (is.null(input$zscore_cutoff) || is.na(input$zscore_cutoff) || !is.numeric(input$zscore_cutoff)) {
        updateNumericInput(session, "zscore_cutoff", value = 1)
        showNotification("Z-score cutoff must be a valid number (minimum 1)", type = "warning")
      }
      # Then check if it's below the minimum
      else if (input$zscore_cutoff < 1) {
        updateNumericInput(session, "zscore_cutoff", value = 1)
        showNotification("Z-score cutoff must be at least 1", type = "warning")
      }
    }, ignoreNULL = FALSE)

    return(list(
      processed_data = reactive({ 
        if (rv$changes_applied) {
          rv$processed_data 
        } else {
          NULL
        }
      }),
      has_transformed = reactive({ rv$changes_applied }),
      modal_closed = reactive({ rv$modal_closed }),
      factor_columns = reactive({ rv$factor_columns }),
      code = reactive({ rv$code })  # NEW: Return the reproducible code
    ))
  })
}