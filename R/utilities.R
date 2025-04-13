guess_transform <- function(data_matrix) {
  # sample this meany rows or columns
  n_sample <- 500

  # Check if input is valid
  if (!is.matrix(data_matrix) && !is.data.frame(data_matrix)) {
    return(0)  # Return 0 for invalid input
  }
  
  # Convert to matrix if it's a data frame
  if (is.data.frame(data_matrix)) {
    # Try to convert to numeric matrix, handling potential errors
    tryCatch({
      data_matrix <- as.matrix(data_matrix)
      data_matrix <- matrix(as.numeric(data_matrix), nrow = nrow(data_matrix))
    }, error = function(e) {
      return(0)  # Return 0 if conversion fails
    })
  }
  
  # Check if matrix is empty or contains only NAs
  if (nrow(data_matrix) == 0 || ncol(data_matrix) == 0 || all(is.na(data_matrix))) {
    return(0)
  }
  
  if (nrow(data_matrix) > n_sample) {
    sampled_rows <- sample(1:nrow(data_matrix), n_sample)
    data_matrix <- data_matrix[sampled_rows, , drop = FALSE]
  }
  
  # Sample columns if more than 500
  if (ncol(data_matrix) > n_sample) {
    sampled_cols <- sample(1:ncol(data_matrix), n_sample)
    data_matrix <- data_matrix[, sampled_cols, drop = FALSE]
  }
  
  # Calculate row and column medians, ignoring NA values
  row_medians <- apply(data_matrix, 1, median, na.rm = TRUE)
  col_medians <- apply(data_matrix, 2, median, na.rm = TRUE)
  
  # Check if all medians are NA
  if (all(is.na(row_medians)) || all(is.na(col_medians))) {
    return(0)
  }
  
  # Calculate MAD of medians
  row_mad <- mad(row_medians, na.rm = TRUE)
  col_mad <- mad(col_medians, na.rm = TRUE)
  
  # Handle edge cases where MAD is 0 or NA
  if (is.na(row_mad) || is.na(col_mad) || row_mad == 0 || col_mad == 0) {
    # If MAD is 0, calculate standard deviation instead
    row_mad <- sd(row_medians, na.rm = TRUE)
    col_mad <- sd(col_medians, na.rm = TRUE)
    
    # If still 0 or NA, return 0
    if (is.na(row_mad) || is.na(col_mad) || row_mad == 0 || col_mad == 0) {
      return(0)
    }
  }
  
  # Compare MADs without division
  if (col_mad <= row_mad) {  # Rows are variables, columns are observations
    if (max(row_medians) < 10 * min(row_medians)) {
      return(2)  # Largest row_mad < 10 times smallest row_mad
    } else {
      return(3)  # Otherwise, recommend row scaling
    }
  } else {  # Columns are variables, rows are observations
    if (max(col_medians) < 10 * min(col_medians)) {
      return(4)  # Largest col_mad < 10 times smallest col_mad
    } else {
      return(5)  # Otherwise, recommend column scaling
    }
  }
}

# Generic dimensionality reduction plot function
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