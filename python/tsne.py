import numpy as np
import pandas as pd
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA
from .utilities import create_dr_plot

def run_tsne(
    data,
    transpose=False,
    perplexity=30,
    early_exaggeration=12,
    learning_rate=200,
    n_iter=1000,
    pca_preprocessing=False,
    plot=False,
    point_annot=None,
    fontsize=12,
    show_labels=False,
    point_labels=None
):
    """
    Perform t-SNE and return the 2D embedding.
    Args:
        data: np.ndarray or pd.DataFrame
        transpose: bool
        perplexity: int
        early_exaggeration: float
        learning_rate: float
        n_iter: int
        pca_preprocessing: bool
        plot: bool
        point_annot: pd.DataFrame or None
        fontsize: int
        show_labels: bool
        point_labels: list or None
    Returns:
        tsne_coords: np.ndarray (n_samples x 2)
    """
    if isinstance(data, pd.DataFrame):
        data = data.values
    if transpose:
        data = data.T
    # Remove rows with NaN
    data = data[~np.isnan(data).any(axis=1)]
    # Optionally apply PCA preprocessing
    if pca_preprocessing and data.shape[1] > 50:
        data = PCA(n_components=50).fit_transform(data)
    tsne = TSNE(
        n_components=2,
        perplexity=perplexity,
        early_exaggeration=early_exaggeration,
        learning_rate=learning_rate,
        n_iter=n_iter,
        random_state=42
    )
    tsne_coords = tsne.fit_transform(data)
    if plot:
        create_dr_plot(tsne_coords, "tSNE 1", "tSNE 2", point_annot, fontsize, show_labels, point_labels)
    return tsne_coords 