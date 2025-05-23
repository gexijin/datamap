# DataMap: A Shiny app for visualizing data matrices with heatmaps, PCA, and t-SNE
# by Steven Ge 4/5/2025  

#' Run the DataMap Shiny application
#' 
#' @export
#' 
#' @return A Shiny application object
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {



library(shiny)
library(munsell) # otherwise it will not work; Pheatmap dependency. Sometimes shinylive does not install this.

# these are needed showing static images for both the R package and directly from the app
# Note: The images and html files are saved into two copies!!!!!
# when running directly: using www and extdata
# when running as a package: using inst/www and inst/extdata

max_rows_to_show <- 1000  # Maximum number of rows to show row names in the heatmap
default_width <- 600
default_height <- 600

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Place Upload Files and Reset buttons on the same row
      fluidRow(
        column(6, 
               actionButton("show_upload_modal", "Files", 
                            icon = icon("upload"))
        ),
        column(6,
               conditionalPanel(
                 condition = "output.data_loaded",
                 actionButton("reset_session", "Reset", 
                              icon = icon("refresh"), 
                              style = "background-color: #f8d7da; color: #721c24;")
               )
        )
      ),

      conditionalPanel(
        condition = "output.data_loaded",
        fluidRow(
          column(8, transform_ui("transform"), style = "margin-top: 5px;"),
          column(4, uiOutput("transform_status"), style = "margin-top: 5px;")
        )
      ), 

      # Dynamic UI for selecting row annotation columns
      conditionalPanel(
        condition = "output.row_annotation_available",
        uiOutput("row_annotation_select_ui")
      ),
      conditionalPanel(
        condition = "output.col_annotation_uploaded",
        uiOutput("col_annotation_select_ui")
      ),
           
      # Heatmap customization section - only shown after data is loaded and Heatmap tab is active
      conditionalPanel(
        condition = "output.data_loaded && input['main_tabs'] === 'Heatmap'",
        hr(),
        # Use the heatmap control UI function from the module
        heatmap_control_ui("heatmap")
      ),
      
      # PCA and t-SNE plot control section - only shown when those tabs are active
      conditionalPanel(
        condition = "output.data_loaded && (input['main_tabs'] === 'PCA' || input['main_tabs'] === 't-SNE')",
        hr(),
        h4("Plot Settings"),
        
        # Font size control
        fluidRow(
          column(3, p("Font:", style = "padding-top: 7px; text-align: right;")),
          column(9, sliderInput("global_fontsize", NULL, min = 5, max = 30, value = 12))
        ),
        
        # Width & Height controls
        fluidRow(
          column(3, p("Width:", style = "padding-top: 7px; text-align: right;")),
          column(9, sliderInput("global_width", NULL, min = 200, max = 2000, value = 600, step = 20))
        ),
        
        fluidRow(
          column(3, p("Height:", style = "padding-top: 7px; text-align: right;")),
          column(9, sliderInput("global_height", NULL, min = 200, max = 2000, value = 500, step = 20))
        )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(id = "main_tabs", selected = "About",
        tabPanel("Heatmap", 
                 # Use the heatmap module UI
                 heatmap_ui("heatmap")
        ),
        tabPanel("PCA",
                 pca_plot_ui("pca")
        ),
        tabPanel("t-SNE",
                 tsne_plot_ui("tsne")
        ),
        tabPanel("Code",
                 code_generation_ui("code_gen") 
        ),
        tabPanel("Data",
                 fluidRow(
                   column(12,
                          div(style = "overflow-x: auto;", 
                              tableOutput("data_preview")
                          ),
                          downloadButton("download_data", "Transformed Data")
                   )
                 )
        ),
        tabPanel("About",
                 titlePanel("DataMap: a portable app for visualizing data matrices v0.11"),  
                 img(src = "heatmap.png", width = "375px", height = "300px", alt = "Example heatmap does not show when using R pckage"),
                 img(src = "pca.png", width = "384px", height = "329px", alt = "Exaple PCA plot does not show when using R pckage"),
                 img(src = "countries_label.png", width = "401px", height = "300px", alt = "Exaple heatmap does not show when using R pckage"),
                 img(src = "tsne.png", width = "335px", height = "300px", alt = "Exaple tSNE plot does not show when using R pckage"),
                 includeHTML(datamap_resource("www/help.html"))
        )
      )
      #,tags$head(includeHTML(datamap_resource("www/google_analytics.html")))
    )
  )
)

server <- function(input, output, session) {

  # Track if main data is loaded for UI conditionals
  output$data_loaded <- reactive({
    return(file_data$data_loaded())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Switch to Heatmap tab when data is loaded
  observe({
    if (file_data$data_loaded()) {
      updateTabsetPanel(session, "main_tabs", selected = "Heatmap")
    }
  })

  # Show the modal when the button is clicked
  output$main_file_upload_ui <- renderUI({
    if (!is.null(file_data$data())) {
      # If data is already uploaded
      tags$div(
        style = "padding: 15px; background-color: #f8f9fa; border-radius: 4px; text-align: center;",
        tags$p(
          icon("info-circle"), 
          "Reset the app to upload a new data matrix.", 
          style = "margin: 0; color: #495057;"
        )
      )
    } else {
      tags$div(
        tags$h4("Data file (Excel, CSV, ...)"),
        file_upload_ui("file_upload"),
        fluidRow(
          column(2, h5("Examples:", style = "margin-top: -8px;  text-align: right;")),
          column(10, 
                 downloadButton("download_countries", "Countries", style = "margin-top: -15px;"),
                 downloadButton("download_rnaseq", "RNAseq", style = "margin-top: -15px;"),
                 downloadButton("download_iris", "Iris", style = "margin-top: -15px;"),
                 downloadButton("download_scrnaseq", "scRNAseq", style = "margin-top: -15px;"),
                 align = "left"
          )
        )
      )
    }
  })

  observeEvent(input$show_upload_modal, {
    showModal(modalDialog(
      title = "Upload Files",
      
      uiOutput("main_file_upload_ui"),
      
      hr(),
      tags$div(
        tags$h4("Optional: Column Annotation"),
        file_upload_ui("col_annotation_file_upload"),
        fluidRow(
          column(2, h5("Examples:", style = "margin-top: -8px;  text-align: right;")),
          column(10, 
                 downloadButton("download_col_rnaseq", "RNA-seq factors", style = "margin-top: -15px;"),
                 downloadButton("download_col_iris", "Iris column info", style = "margin-top: -15px;"),
                 align = "left"
          )
        )
      ),
      hr(),
      tags$div(
        tags$h4("Optional: Row Annotations"),
        file_upload_ui("row_annotation_file_upload"),
        fluidRow(         
          column(2, h5("Examples:", style = "margin-top: -8px; text-align: right;")),
          column(10,
                 downloadButton("download_row_rnaseq", "RNAseq gene info", style = "margin-top: -15px;"),
                 downloadButton("download_row_scrnaseq", "scRNAseq clusters", style = "margin-top: -15px;"),
                 align = "left"
          )
        )
      ),
      
      footer = tagList(
        modalButton("Close")
      ),
      size = "m",
      easyClose = TRUE
    ))
  })

  output$download_iris <- downloadHandler(
    filename = function() {
      "iris.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/iris.csv"), file)
    }
  )
  output$download_rnaseq <- downloadHandler(
    filename = function() {
      "RNAseq.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/RNAseq.csv"), file)
    }
  )

  output$download_scrnaseq <- downloadHandler(
    filename = function() {
      "scRNAseq_PCA.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/scRNAseq_PCA.csv"), file)
    }
  )
  output$download_countries <- downloadHandler(
    filename = function() {
      "countries.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/countries.csv"), file)
    }
  )
  output$download_col_rnaseq <- downloadHandler(
    filename = function() {
      "RNAseq_design.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/RNAseq_design.csv"), file)
    }
  )
  output$download_col_iris <- downloadHandler(
    filename = function() {
      "iris_column_annot.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/iris_column_annot.csv"), file)
    }
  )
  output$download_row_rnaseq <- downloadHandler(
    filename = function() {
      "RNAseq_gene_info.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/RNAseq_gene_info.csv"), file)
    }
  )
  output$download_row_scrnaseq <- downloadHandler(
    filename = function() {
      "scRNAseq_clusters.csv"
    },
    content = function(file) {
      file.copy(datamap_resource("extdata/scRNAseq_clusters.csv"), file)
    }
  )
  # Use the file upload module for main data
  file_data <- file_upload_server("file_upload")
  
  # Use a separate file upload module for the column annotation file
  col_annotation_file_data <- file_upload_server("col_annotation_file_upload")
  
  # Use a separate file upload module for the row annotation file
  row_annotation_file_data <- file_upload_server("row_annotation_file_upload")
  
  # Reactive to indicate if column annotation file is uploaded
  output$col_annotation_uploaded <- reactive({
    !is.null(col_annotation_file_data$data())
  })
  outputOptions(output, "col_annotation_uploaded", suspendWhenHidden = FALSE)
  
  output$row_annotation_available <- reactive({
    has_uploaded <- !is.null(row_annotation_file_data$data())
    has_factors <- !is.null(transform_data$factor_columns()) && 
                  ncol(transform_data$factor_columns()) > 0
    return(has_uploaded || has_factors)
  })
  outputOptions(output, "row_annotation_available", suspendWhenHidden = FALSE)
  
  # Render UI for column annotation row selection
  output$col_annotation_select_ui <- renderUI({
    req(col_annotation_file_data$data())
    annot_df <- col_annotation_file_data$data()
    row_choices <- rownames(annot_df)
    if (is.null(row_choices) || length(row_choices) == 0) {
      row_choices <- as.character(seq_len(nrow(annot_df)))
    }
    selectInput("col_annotation_select", "Column annotation:", 
                choices = row_choices, selected = row_choices[1], 
                multiple = TRUE)
  })

  # Render UI for row annotation column selection (merged from both sources)
  output$row_annotation_select_ui <- renderUI({
    all_choices <- c()
    default_selected <- c()
    
    # Get choices from uploaded row annotation file
    if (!is.null(row_annotation_file_data$data())) {
      uploaded_choices <- colnames(row_annotation_file_data$data())
      if (!is.null(uploaded_choices) && length(uploaded_choices) > 0) {
        all_choices <- c(all_choices, uploaded_choices)
        # Select first uploaded column by default
        default_selected <- c(default_selected, uploaded_choices[1])
      }
    }
    
    # Get choices from auto-detected factor columns
    if (!is.null(transform_data$factor_columns()) && 
        ncol(transform_data$factor_columns()) > 0) {
      factor_choices <- colnames(transform_data$factor_columns())
      if (!is.null(factor_choices) && length(factor_choices) > 0) {
        all_choices <- c(all_choices, factor_choices)
        # Select all factor columns by default
        default_selected <- c(default_selected, factor_choices)
      }
    }
    
    # Remove any duplicates
    all_choices <- unique(all_choices)
    default_selected <- unique(default_selected)
    
    # Create the selectInput if we have any choices
    if (length(all_choices) > 0) {
      tags$div(style = "margin-top: 5px;",
               selectInput("row_annotation_select", "Row annotations:", 
                           choices = all_choices, selected = default_selected, multiple = TRUE))
    } else {
      return(NULL)
    }
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

   # Modified reactive function that matches original behavior
  col_annotation_for_heatmap <- reactive({
    req(current_data())
    
    # Check if we have annotation data
    if (is.null(col_annotation_file_data$data())) {
      return(NULL)
    }
    
    # Get annotation data
    annot_df <- col_annotation_file_data$data()
    main_cols <- colnames(current_data())
    selected <- input$col_annotation_select
    
    # Process annotations using the extracted function
    process_column_annotations(
      main_data_cols = main_cols,
      annotation_df = annot_df,
      selected_annotations = selected
    )
  }) 

  row_annotation_for_heatmap <- reactive({
    req(current_data())
    
    # Get selected annotations
    selected <- input$row_annotation_select
    
    # Get main data rows
    main_rows <- rownames(current_data())
    
    # Get file annotation data
    file_annot_df <- NULL
    if (!is.null(row_annotation_file_data$data())) {
      file_annot_df <- row_annotation_file_data$data()
    }
    
    # Get factor annotation data
    factor_annot_df <- NULL
    if (!is.null(transform_data$factor_columns()) && 
        ncol(transform_data$factor_columns()) > 0) {
      factor_annot_df <- transform_data$factor_columns()
    }
    
    # Process annotations using the utility function
    process_row_annotations(
      main_data_rows = main_rows,
      file_annotation_df = file_annot_df,
      factor_annotation_df = factor_annot_df,
      selected_annotations = selected
    )
  })


  # Create reactives for font size, width, and height to ensure availability for PCA and t-SNE
  fontsize_for_plots <- reactive({ 
    if (!is.null(input$global_fontsize)) input$global_fontsize else 12 
  })
  width_for_plots <- reactive({ 
    if (!is.null(input$global_width)) input$global_width else default_width
  })
  height_for_plots <- reactive({ 
    if (!is.null(input$global_height)) input$global_height else default_height
  })

  # Use the heatmap module
  heatmap_results <- heatmap_server(
    "heatmap",
    current_data,
    file_data,
    transform_data$unprocessed_data,
    col_annotation_for_heatmap,
    row_annotation_for_heatmap,
    transform_data,
    max_rows_to_show,
    default_width,
    default_height
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

  pca_results <- pca_plot_server(
    "pca", 
    current_data, 
    col_annotation_for_heatmap, 
    row_annotation_for_heatmap, 
    fontsize_for_plots,  # Use our reactive 
    width_for_plots,     # Use our reactive
    height_for_plots     # Use our reactive
  )

  tsne_results <- tsne_plot_server(
    "tsne", 
    current_data, 
    col_annotation_for_heatmap, 
    row_annotation_for_heatmap, 
    fontsize_for_plots,  # Use our reactive
    width_for_plots,     # Use our reactive
    height_for_plots     # Use our reactive
  )

  code_gen_results <- code_generation_server(
    "code_gen",
    file_data,
    transform_data,
    heatmap_results,
    pca_results,
    tsne_results,
    col_annotation_file_data,
    row_annotation_file_data
  )

  output$data_preview <- renderTable({
    req(current_data())
    data <- current_data()
    
    # Ensure data is a data frame
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)
    }
    
    # Limit to 30 rows and 30 columns
    max_rows <- min(30, nrow(data))
    max_cols <- min(30, ncol(data))
    
    # Get the row indices and column indices
    row_indices <- seq_len(max_rows)
    col_indices <- seq_len(max_cols)
    
    # Create a preview version of the data
    preview_data <- data[row_indices, col_indices, drop = FALSE]
    
    # Return the preview with row names preserved
    preview_data
  }, rownames = TRUE, colnames = TRUE, bordered = TRUE, width = "100%", 
     digits = 3, align = 'c')

  
  # Download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("transformed_data-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv")
    },
    content = function(file) {
      data <- current_data()
      write.csv(data, file, row.names = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)

}