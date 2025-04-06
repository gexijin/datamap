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


#' Map R Column Classes to User-Friendly Types
#'
#' This function takes a column from a dataset and maps its R class
#' to one of the predefined user-friendly categories:
#' "character", "numeric", "integer", "factor", or "Date".
#'
#' @param col A column vector from a dataframe (e.g., `df$column_name`).
#'
#' @return A character string representing the mapped data type.
#' Possible values: `"character"`, `"numeric"`, `"integer"`, `"factor"`, `"Date"`, `"Datetime"`.
#'
#' @export
#'
#' @examples
#' map_class_to_type(Sys.Date())   # Returns "Date"
#' map_class_to_type(42)           # Returns "numeric"
#' map_class_to_type(factor("A"))  # Returns "factor"
#'
map_class_to_type <- function(col) {
  col_class <- class(col)
  
  if ("character" %in% col_class) {
    return("character")
  } else if ("factor" %in% col_class) {
    return("factor")
  } else if ("integer" %in% col_class) {
    return("integer")
  } else if ("numeric" %in% col_class) {
    return("numeric")
  } else if ("Date" %in% col_class) {
    return("Date")  # Pure dates
  } else if (any(c("POSIXct", "POSIXt") %in% col_class)) {
    return("Datetime")  # Preserve Datetime
  } else {
    return("character")  # Default to character if unrecognized
  }
}