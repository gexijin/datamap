## code_generation_module.R - Module for code generation and display
## This module handles the generation, display, and downloading of R code

# UI function for the code generation module
code_generation_ui <- function(id) {
  ns <- NS(id)
  
  # UI is just a placeholder that will be filled by the server
  uiOutput(ns("code_display"))
}

# Server function for the code generation module
code_generation_server <- function(id, file_data, transform_data, heatmap_results, 
  pca_results, tsne_results, col_annotation_data, row_annotation_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    delimiter <- "\n\n################################################################################"

    # Create a reactive expression for generating the full code
    full_code <- reactive({
      # Initialize an empty vector to store code parts
      code_parts <- c()
      
      utilities_file <- "R/utilities.R"
      if (file.exists(utilities_file)) {
        utilities_code <- readLines(utilities_file)
        code_parts <- c(delimiter, "# Utilty functions", utilities_code, "")
      } else {
        code_parts <- c(paste("# Warning: utilities file not found at", utilities_file), "")
      }

      # Add file upload code if available
      if (!is.null(file_data$code())) {
        code_parts <- c(code_parts, delimiter, "# Data Import Code", file_data$code(), 
          "raw_data <- data", "rm(data)", "")
      }
      
      # Add column annotation file upload code if available
      if (!is.null(col_annotation_data$code())) {
        code_parts <- c(code_parts, "# Column Annotation Import Code", 
          col_annotation_data$code(), "col_annotation_raw <- data", "rm(data)", "")
      }
      
      # Add row annotation file upload code if available
      if (!is.null(row_annotation_data$code())) {
        code_parts <- c(code_parts, "# Row Annotation Import Code", 
          row_annotation_data$code(), "row_annotation_raw <- data", "rm(data)","")
      }

      # Add transform code if available
      if (!is.null(transform_data$code())) {
        code_parts <- c(code_parts, delimiter, "# Data Transformation Code", transform_data$code(), "")
      }
      
      # Add heatmap code from the module if available
      if (!is.null(heatmap_results$heatmap_code())) {
        code_parts <- c(code_parts, delimiter, heatmap_results$heatmap_code())
      }

      # Get PCA code from the module
      if (!is.null(pca_results$pca_code())) {
        code_parts <- c(code_parts, delimiter, "# PCA plot", pca_results$pca_code())
      }
      
      # Get t-SNE code from the module
      if (!is.null(tsne_results$tsne_code())) {
        code_parts <- c(code_parts, delimiter, "# tSNE plot", tsne_results$tsne_code())
      }
      
      # Combine all parts and return
      paste(code_parts, collapse = "\n")
    })

    output$code_display <- renderUI({
      req(full_code())
      
      code_text <- full_code()
      
      # Split the code into sections for better display
      sections <- strsplit(code_text, delimiter)[[1]]
      
      # Initialize HTML output
      html_output <- tagList()
      
      # Process each section
      current_section <- NULL
      section_content <- NULL
      
      for (section in sections) {
        if (nchar(section) == 0) next
        
        # First line is likely a section title
        lines <- strsplit(section, "\n")[[1]]

        section_title <- gsub("#", "", lines[2])
        if(length(lines) > 2) {
          lines <- lines[-(1:2)]
        }
        
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
          fluidRow(
            column(width = 8, style = "margin-top:5px;", tags$p("To reproduce the plots, download and execute the generated R script in RStudio, ensuring all required data files are in the same folder.")),
            column(width = 4, style = "margin-top:5px;", downloadButton(ns("download_combined_code"), "Generated R Script"))
          ),
          html_output
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
    
    # Return the code reactive to make it available to the main app if needed
    return(list(
      full_code = full_code
    ))
  })
}