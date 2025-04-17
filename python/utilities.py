import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches


def guess_transform(data_matrix):
    n_sample = 500

    # Convert to numpy array if it's a DataFrame
    if isinstance(data_matrix, pd.DataFrame):
        try:
            data_matrix = data_matrix.apply(pd.to_numeric, errors='coerce').values
        except Exception:
            return 0
    elif not isinstance(data_matrix, np.ndarray):
        return 0

    # Check if matrix is empty or contains only NaNs
    if data_matrix.size == 0 or np.all(np.isnan(data_matrix)):
        return 0

    # Sample rows/columns if too large
    if data_matrix.shape[0] > n_sample:
        idx = np.random.choice(data_matrix.shape[0], n_sample, replace=False)
        data_matrix = data_matrix[idx, :]
    if data_matrix.shape[1] > n_sample:
        idx = np.random.choice(data_matrix.shape[1], n_sample, replace=False)
        data_matrix = data_matrix[:, idx]

    # Row/col medians
    row_medians = np.nanmedian(data_matrix, axis=1)
    col_medians = np.nanmedian(data_matrix, axis=0)

    if np.all(np.isnan(row_medians)) or np.all(np.isnan(col_medians)):
        return 0

    # MAD (Median Absolute Deviation)
    row_mad = np.nanmedian(np.abs(row_medians - np.nanmedian(row_medians)))
    col_mad = np.nanmedian(np.abs(col_medians - np.nanmedian(col_medians)))

    # Fallback to std if MAD is 0 or nan
    if np.isnan(row_mad) or np.isnan(col_mad) or row_mad == 0 or col_mad == 0:
        row_mad = np.nanstd(row_medians)
        col_mad = np.nanstd(col_medians)
        if np.isnan(row_mad) or np.isnan(col_mad) or row_mad == 0 or col_mad == 0:
            return 0

    if col_mad <= row_mad:
        if np.nanmax(row_medians) < 10 * np.nanmin(row_medians):
            return 2
        else:
            return 3
    else:
        if np.nanmax(col_medians) < 10 * np.nanmin(col_medians):
            return 4
        else:
            return 5


def create_dr_plot(coords_data, x_label, y_label, point_annot=None, fontsize=12, show_labels=False, point_labels=None):
    fig, ax = plt.subplots()
    point_colors = 'black'
    point_shapes = 'o'
    point_sizes = np.full(coords_data.shape[0], fontsize)
    legend_items = []

    if point_annot is not None and point_annot.shape[1] > 0:
        selected_cols = point_annot.columns
        # Color by first annotation
        if len(selected_cols) >= 1:
            color_col = selected_cols[0]
            color_factor = pd.Categorical(point_annot[color_col])
            color_palette = plt.cm.rainbow(np.linspace(0, 1, len(color_factor.categories)))
            point_colors = [color_palette[list(color_factor.categories).index(val)] for val in color_factor]
            legend_items.append(mpatches.Patch(color=color_palette[0], label=color_col))
        # Shape by second annotation
        if len(selected_cols) >= 2:
            shape_col = selected_cols[1]
            shape_factor = pd.Categorical(point_annot[shape_col])
            available_shapes = ['o', '^', 's', 'D', 'P', 'X', '*', 'v', '<', '>']
            point_shapes = [available_shapes[list(shape_factor.categories).index(val) % len(available_shapes)] for val in shape_factor]
            legend_items.append(mpatches.Patch(label=shape_col))
        else:
            point_shapes = ['o'] * coords_data.shape[0]
    else:
        point_colors = ['black'] * coords_data.shape[0]
        point_shapes = ['o'] * coords_data.shape[0]

    for i in range(coords_data.shape[0]):
        ax.scatter(coords_data[i, 0], coords_data[i, 1], c=[point_colors[i]], marker=point_shapes[i], s=point_sizes[i])
        if show_labels and point_labels is not None and len(point_labels) >= coords_data.shape[0]:
            ax.text(coords_data[i, 0], coords_data[i, 1], str(point_labels[i]), fontsize=fontsize * 0.8)

    ax.set_xlabel(x_label, fontsize=fontsize)
    ax.set_ylabel(y_label, fontsize=fontsize)
    if legend_items:
        ax.legend(handles=legend_items)
    plt.tight_layout()
    plt.show() 