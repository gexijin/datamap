import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.spatial.distance import pdist, squareform
from scipy.cluster.hierarchy import linkage, dendrogram
from scipy.stats import spearmanr, kendalltau

def custom_correlation_distance(X, method="pearson"):
    if method == "pearson":
        corr = np.corrcoef(X)
    elif method == "spearman":
        corr, _ = spearmanr(X, axis=1)
    elif method == "kendall":
        corr = np.ones((X.shape[0], X.shape[0]))
        for i in range(X.shape[0]):
            for j in range(i+1, X.shape[0]):
                tau, _ = kendalltau(X[i], X[j])
                corr[i, j] = corr[j, i] = tau
    else:
        raise ValueError("Unknown correlation method")
    corr = np.nan_to_num(corr)
    return squareform(1 - corr)

def plot_heatmap(
    data,
    cluster_rows=True,
    cluster_cols=True,
    clustering_method="average",
    distance_method="euclidean",
    color_palette="RdYlBu",
    annotation_col=None,
    annotation_row=None,
    show_rownames=True,
    fontsize=12,
    figsize=(10, 10),
    label_heatmap=False
):
    """
    Plot a clustered heatmap with options for distance/correlation and annotation.
    """
    if isinstance(data, pd.DataFrame):
        data = data.values
    # Row/col clustering
    row_linkage = col_linkage = None
    if cluster_rows:
        if distance_method in ["pearson", "spearman", "kendall"]:
            row_dist = custom_correlation_distance(data, method=distance_method)
        else:
            row_dist = pdist(data, metric=distance_method)
        row_linkage = linkage(row_dist, method=clustering_method)
    if cluster_cols:
        if distance_method in ["pearson", "spearman", "kendall"]:
            col_dist = custom_correlation_distance(data.T, method=distance_method)
        else:
            col_dist = pdist(data.T, metric=distance_method)
        col_linkage = linkage(col_dist, method=clustering_method)
    # Plot
    g = sns.clustermap(
        data,
        row_linkage=row_linkage,
        col_linkage=col_linkage,
        cmap=color_palette,
        figsize=figsize,
        annot=label_heatmap,
        fmt=".2f" if label_heatmap else None,
        yticklabels=show_rownames,
        xticklabels=True,
        cbar_kws={"label": "Value"},
        dendrogram_ratio=(.2, .2),
        annot_kws={"size": fontsize} if label_heatmap else None
    )
    plt.show() 