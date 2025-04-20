# Utility functions for plotting and processing annotations. DataMap


#' Create Dimensionality Reduction Plot
#'
#' Generates a scatter plot for dimensionality reduction with support for annotations that alter
#' point colors and shapes, and optionally includes point labels.
#'
#' @param coords_data A numeric matrix or data frame containing at least two columns for the x and y coordinates.
#' @param x_label A character string specifying the label for the x-axis.
#' @param y_label A character string specifying the label for the y-axis.
#' @param point_annot Optional data frame containing annotation data. The first column is used for assigning colors,
#'   and if a second column is provided, its values are used for assigning point shapes.
#' @param fontsize A numeric value specifying the base font size used for labels, axes, and legends. Default is 12.
#' @param show_labels Logical flag indicating whether to display text labels next to points. Default is FALSE.
#' @param point_labels Optional character vector containing labels for each point. Must have at least as many elements as rows in coords_data when show_labels is TRUE.
#'
#' @return A recorded plot object that can be replayed using \code{replayPlot()}.
#'
#' @details The function sets up the plotting environment, adjusts plot limits based on whether labels are shown,
#' and manages legends for both color and shape annotations. If annotation data is provided, the first column
#' determines the color palette (using a rainbow palette) and the second column (if available) assigns point shapes.
#'
#' @examples
#' # Example with simulated data:
#' coords <- matrix(rnorm(200), ncol = 2)
#' annot <- data.frame(Group = sample(c("A", "B", "C"), 100, replace = TRUE),
#'                     Type = sample(c("X", "Y"), 100, replace = TRUE))
#' plot_obj <- create_dr_plot(coords, "X Axis", "Y Axis", point_annot = annot, show_labels = TRUE, 
#'                            point_labels = paste("P", 1:100, sep=""))
#' replayPlot(plot_obj)
#'
create_dr_plot <- function(coords_data, x_label, y_label, point_annot = NULL, fontsize = 12, 
                          show_labels = FALSE, point_labels = NULL) {
  # Default plot settings
  point_colors <- "black"
  point_shapes <- 16
  point_sizes <- rep(fontsize/12, nrow(coords_data))
  
  # If annotations are available, use them for colors and shapes
  legend_items <- list()
  
  if (!is.null(point_annot) && ncol(point_annot) > 0) {
    # Get all annotation columns
    selected_cols <- names(point_annot)
    
    # Use first column for colors
    if (length(selected_cols) >= 1) {
      color_col <- selected_cols[1]
      color_factor <- as.factor(point_annot[[color_col]])
      color_levels <- levels(color_factor)
      color_palette <- rainbow(length(color_levels))
      point_colors <- color_palette[as.numeric(color_factor)]
      
      # Store color legend info
      legend_items$colors <- list(
        title = color_col,
        labels = color_levels,
        palette = color_palette
      )
    }
    
    # Use second column for shapes (ONLY if we have multiple annotations)
    if (length(selected_cols) >= 2) {
      shape_col <- selected_cols[2]
      shape_factor <- as.factor(point_annot[[shape_col]])
      shape_levels <- levels(shape_factor)
      
      available_shapes <- c(16, 17, 15, 18, 19, 1, 2, 5, 6, 8)
      shape_numbers <- available_shapes[1:min(length(available_shapes), length(shape_levels))]
      point_shapes <- shape_numbers[as.numeric(shape_factor)]
      
      # Store shape legend info
      legend_items$shapes <- list(
        title = shape_col,
        labels = shape_levels,
        shapes = shape_numbers
      )
    } else {
      # If only one annotation, use circle shape for all points
      point_shapes <- 16
    }
  }
  
  # Create a new plotting environment
  plot_env <- new.env()
  
  # Create and record the plot
  p <- with(plot_env, {
    # Set margins
    par(mar = c(5, 5, 2, 10) + 0.1)

    adjusted_xlim <- c(min(coords_data[, 1]), max(coords_data[, 1]))
    adjusted_ylim <- c(min(coords_data[, 2]), max(coords_data[, 2]))
    if(show_labels){
      # extend xlimit in both directions
      xrange_adjust <- (max(coords_data[, 1]) - min(coords_data[, 1])) * 0.1
      adjusted_xlim <- c(min(coords_data[, 1]) - xrange_adjust, 
                         max(coords_data[, 1]) + xrange_adjust)
      # increase ylim on top
      yrange_adjust <- (max(coords_data[, 2]) - min(coords_data[, 2])) * 0.05
      adjusted_ylim <- c(min(coords_data[, 2]), max(coords_data[, 2]) + yrange_adjust)
    }

    # Create the points plot
    plot(coords_data[,1], coords_data[,2], 
       xlab = x_label,
       ylab = y_label,
       main = "",
       pch = point_shapes,
       col = point_colors,
       cex = point_sizes,
       cex.lab = fontsize/12,
       cex.axis = fontsize/12,      # increase limits so that the labels show up.
       xlim = adjusted_xlim,
       ylim = adjusted_ylim)
    
    # Add point labels if enabled
    if (show_labels && !is.null(point_labels) && length(point_labels) >= nrow(coords_data)) {
      text(coords_data[,1], coords_data[,2], 
           labels = point_labels[1:nrow(coords_data)], 
             pos = 3, offset = 0.5, cex = fontsize/15)
    }
    
    # Add legend if using annotations
    if (length(legend_items) > 0) {
      # Allow plotting outside the plot region
      par(xpd = TRUE)
      
      # Color legend
      if (!is.null(legend_items$colors)) {
        color_info <- legend_items$colors
        
        legend("topright", 
               legend = color_info$labels,
               fill = color_info$palette,
               title = color_info$title,
               cex = fontsize/15,
               inset = c(-0.25, 0),
               bty = "n")
      }
      
      # Shape legend (if available)
      if (!is.null(legend_items$shapes)) {
        shape_info <- legend_items$shapes
        
        legend("topright", 
               legend = shape_info$labels,
               pch = shape_info$shapes,
               title = shape_info$title,
               cex = fontsize/15,
               inset = c(-0.25, 0.3),
               bty = "n")
      }
      
      par(xpd = FALSE)
    }
    
    # Return the recorded plot
    recordPlot()
  })
  
  return(p)
}



#' Process Column Annotations for Heatmap
#'
#' This function processes column annotations by aligning selected annotation rows with the main dataset's columns.
#' It attempts to safely subset and merge annotation data based on common sample names and handles errors during processing.
#'
#' @param main_data_cols A character vector of column names corresponding to the main dataset.
#' @param annotation_df A data frame containing annotation information where rows correspond to different annotation types 
#'   and columns represent samples.
#' @param selected_annotations A vector indicating the rows (by their names or indices) in annotation_df to be selected.
#'
#' @return A data frame with the main dataset's column names as its row names and the selected annotation types as its column names.
#'   If there are no annotations selected, no common samples between datasets, or if all annotations fail to process, the function returns NULL.
#'
#'
#' @export
process_column_annotations <- function(main_data_cols, annotation_df, selected_annotations) {
  # Check if annotation rows are selected
  if (is.null(selected_annotations) || length(selected_annotations) == 0) {
    return(NULL)
  }
  
  # Safely select annotation rows - note the difference in approach here
  annot_selected <- NULL
  tryCatch({
    annot_selected <- annotation_df[selected_annotations, , drop = FALSE]
  }, error = function(e) {
    # Log the error
    warning("Error selecting annotations: ", e$message)
  })
  
  # Check if annotation selection failed
  if (is.null(annot_selected)) {
    return(NULL)
  }
  
  # Check if there are any common samples between main data and annotation file
  common_samples <- intersect(main_data_cols, colnames(annot_selected))
  if (length(common_samples) == 0) {
    return(NULL)
  }
  
  # Create an empty data frame with all main data samples
  output_df <- data.frame(matrix(NA, nrow = length(main_data_cols), ncol = nrow(annot_selected)))
  rownames(output_df) <- main_data_cols
  colnames(output_df) <- rownames(annot_selected)
  
  for (ann in rownames(annot_selected)) {
    success <- tryCatch({
      values <- as.character(annot_selected[ann, common_samples])
      names(values) <- common_samples
      output_df[common_samples, ann] <- values
      TRUE #indicate success
    }, error = function(e) {
      warning("Error processing annotation '", ann, "': ", e$message)
      FALSE #indicate failure
    })
    if (!success) {
      output_df <- output_df[, colnames(output_df) != ann, drop = FALSE]
    }
  }

  # Check if we have any annotations left
  if (ncol(output_df) == 0) {
    warning("All annotations failed to process")
    return(NULL)
  }
  
  return(as.data.frame(output_df))
}



#' Process Row Annotations
#'
#' This function processes and combines annotation data for rows from two potential sources:
#' a file-uploaded annotation data frame and an auto-detected factor annotation data frame. It selects
#' desired annotation columns based on the provided list, aligns these annotations with the main data rows,
#' and merges them into a single data frame.
#'
#' @param main_data_rows A vector of row identifiers from the main data set.
#' @param file_annotation_df An optional data frame containing row annotations from an uploaded file.
#'   Default is NULL.
#' @param factor_annotation_df An optional data frame containing row annotations based on factors.
#'   Default is NULL.
#' @param selected_annotations A character vector specifying the names of annotations to extract.
#'
#' @return A data frame with combined annotations that correspond to the main data rows. Returns
#'   NULL if no annotations are selected or if no matching annotations are found.
#'
process_row_annotations <- function(main_data_rows, 
                                   file_annotation_df = NULL,
                                   factor_annotation_df = NULL,
                                   selected_annotations) {
  # Return NULL if no annotations are selected
  if (is.null(selected_annotations) || length(selected_annotations) == 0) {
    return(NULL)
  }
  
  # Initialize combined annotations as NULL
  combined_annot <- NULL
  added_columns <- c()
  
  # First try to add columns from uploaded row annotation file
  if (!is.null(file_annotation_df)) {
    # Find selected columns that exist in the file
    file_cols <- intersect(selected_annotations, colnames(file_annotation_df))
    
    if (length(file_cols) > 0) {
      # Create a new data frame with the same structure as the annotation columns
      template_df <- file_annotation_df[1:0, file_cols, drop = FALSE] # Empty df with same column types
      
      # Add rows for all main data rows (with NAs)
      template_df[main_data_rows,] <- NA
      
      # Fill in values for rows that exist in the annotation file
      common_rows <- intersect(main_data_rows, rownames(file_annotation_df))
      if (length(common_rows) > 0) {
        template_df[common_rows, ] <- file_annotation_df[common_rows, file_cols, drop = FALSE]
      }
      
      # Start the combined annotations
      combined_annot <- template_df
      added_columns <- c(added_columns, file_cols)
    }
  }
  
  # Next try to add columns from auto-detected factors
  if (!is.null(factor_annotation_df) && ncol(factor_annotation_df) > 0) {
    # Find selected columns that exist in factors (and aren't already added)
    factor_cols <- setdiff(intersect(selected_annotations, colnames(factor_annotation_df)), added_columns)
    
    if (length(factor_cols) > 0) {
      # Create a new data frame with the same structure as the factor columns
      template_df <- factor_annotation_df[1:0, factor_cols, drop = FALSE] # Empty df with same column types
      
      # Add rows for all main data rows (with NAs)
      template_df[main_data_rows,] <- NA
      
      # Fill in values for rows that exist in the factor data
      common_rows <- intersect(main_data_rows, rownames(factor_annotation_df))
      if (length(common_rows) > 0) {
        template_df[common_rows, ] <- factor_annotation_df[common_rows, factor_cols, drop = FALSE]
      }
      
      # Start or add to the combined annotations
      if (is.null(combined_annot)) {
        combined_annot <- template_df
      } else {
        combined_annot <- cbind(combined_annot, template_df)
      }
    }
  }
  
  # Return NULL if no annotations were added
  if (is.null(combined_annot) || ncol(combined_annot) == 0) {
    return(NULL)
  }
  
  return(combined_annot)
}
