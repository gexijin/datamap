import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from .utilities import create_dr_plot

def run_pca(data, transpose=False, plot=False, point_annot=None, fontsize=12, show_labels=False, point_labels=None):
    """
    Perform PCA and return the first two principal components and explained variance.
    Args:
        data: np.ndarray or pd.DataFrame
        transpose: bool, if True, transpose data before PCA
        plot: bool, if True, plot the result
        point_annot: pd.DataFrame or None
        fontsize: int
        show_labels: bool
        point_labels: list or None
    Returns:
        pc_data: np.ndarray (n_samples x 2)
        explained_var: list of explained variance ratios for PC1 and PC2
    """
    if isinstance(data, pd.DataFrame):
        data = data.values
    if transpose:
        data = data.T
    # Remove rows with NaN
    data = data[~np.isnan(data).any(axis=1)]
    pca = PCA(n_components=2)
    pcs = pca.fit_transform(data)
    explained_var = pca.explained_variance_ratio_[:2]
    if plot:
        create_dr_plot(pcs, f"PC1 ({explained_var[0]*100:.1f}%)", f"PC2 ({explained_var[1]*100:.1f}%)", point_annot, fontsize, show_labels, point_labels)
    return pcs, explained_var 