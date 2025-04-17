import numpy as np
import pandas as pd

def preprocess_data(
    df,
    na_method="zero",
    do_log_transform=False,
    log_constant=1e-6,
    center_scale="none",
    do_zscore_cap=True,
    zscore_cutoff=3,
    do_filter_rows=True,
    top_n_rows=3000
):
    """
    Preprocess a pandas DataFrame similar to the R mod_transform.R logic.
    Args:
        df: pandas DataFrame
        na_method: 'zero', 'mean', 'median', or 'leave'
        do_log_transform: bool
        log_constant: float
        center_scale: 'none', 'center_row', 'scale_row', 'center_col', 'scale_col'
        do_zscore_cap: bool
        zscore_cutoff: float
        do_filter_rows: bool
        top_n_rows: int
    Returns:
        processed_data: np.ndarray
        factor_columns: pd.DataFrame or None
    """
    # Separate factor-like columns (categorical)
    factor_like_cols = []
    for col in df.columns:
        col_data = df[col]
        if (col_data.dtype == object or pd.api.types.is_categorical_dtype(col_data)) and \
           col_data.nunique() < min(50, len(df) * 0.5):
            factor_like_cols.append(col)
    factor_columns = df[factor_like_cols] if factor_like_cols else None
    df_numeric = df.drop(columns=factor_like_cols) if factor_like_cols else df.copy()
    # Convert to numeric
    df_numeric = df_numeric.apply(pd.to_numeric, errors='coerce')
    data_matrix = df_numeric.values
    # Remove rows/cols that are all NA
    row_all_na = np.all(np.isnan(data_matrix), axis=1)
    col_all_na = np.all(np.isnan(data_matrix), axis=0)
    data_matrix = data_matrix[~row_all_na, :]
    data_matrix = data_matrix[:, ~col_all_na]
    # Handle missing values
    if na_method != "leave":
        if na_method == "zero":
            data_matrix = np.where(np.isnan(data_matrix), 0, data_matrix)
        elif na_method == "mean":
            col_means = np.nanmean(data_matrix, axis=0)
            inds = np.where(np.isnan(data_matrix))
            data_matrix[inds] = np.take(col_means, inds[1])
        elif na_method == "median":
            col_medians = np.nanmedian(data_matrix, axis=0)
            inds = np.where(np.isnan(data_matrix))
            data_matrix[inds] = np.take(col_medians, inds[1])
    # Log transform
    if do_log_transform:
        data_matrix[data_matrix < 0] = 0
        data_matrix = np.log10(data_matrix + log_constant)
    # Filter to top N most variable rows
    if do_filter_rows and data_matrix.shape[0] > top_n_rows:
        row_sds = np.nanstd(data_matrix, axis=1)
        top_indices = np.argsort(row_sds)[-top_n_rows:][::-1]
        data_matrix = data_matrix[top_indices, :]
        if factor_columns is not None:
            factor_columns = factor_columns.iloc[top_indices, :]
    # Centering/scaling
    if center_scale == "center_row":
        row_means = np.nanmean(data_matrix, axis=1, keepdims=True)
        data_matrix = data_matrix - row_means
    elif center_scale == "scale_row":
        means = np.nanmean(data_matrix, axis=1, keepdims=True)
        stds = np.nanstd(data_matrix, axis=1, keepdims=True)
        data_matrix = (data_matrix - means) / stds
        data_matrix = np.where(np.isnan(data_matrix), 0, data_matrix)
    elif center_scale == "center_col":
        col_means = np.nanmean(data_matrix, axis=0, keepdims=True)
        data_matrix = data_matrix - col_means
    elif center_scale == "scale_col":
        means = np.nanmean(data_matrix, axis=0, keepdims=True)
        stds = np.nanstd(data_matrix, axis=0, keepdims=True)
        data_matrix = (data_matrix - means) / stds
        data_matrix = np.where(np.isnan(data_matrix), 0, data_matrix)
    # Z-score capping
    if do_zscore_cap:
        flat = data_matrix.flatten()
        flat = flat[np.isfinite(flat)]
        if flat.size > 0:
            overall_mean = np.nanmean(flat)
            overall_sd = np.nanstd(flat)
            upper = overall_mean + zscore_cutoff * overall_sd
            lower = overall_mean - zscore_cutoff * overall_sd
            data_matrix = np.clip(data_matrix, lower, upper)
    return data_matrix, factor_columns 