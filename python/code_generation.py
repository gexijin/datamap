def generate_code(transform_params, analysis_type=None, analysis_params=None):
    """
    Generate a Python script for reproducible data transformation and analysis.
    Args:
        transform_params: dict of preprocessing options (see transform.py)
        analysis_type: 'pca', 'tsne', or 'heatmap'
        analysis_params: dict of analysis-specific options
    Returns:
        code: str
    """
    code_lines = [
        '# Reproducible Data Transformation and Analysis Code',
        'import pandas as pd',
        'import numpy as np',
        'from python.transform import preprocess_data',
        'from python.pca import run_pca',
        'from python.tsne import run_tsne',
        'from python.heatmap import plot_heatmap',
        '',
        '# Load your data',
        "df = pd.read_csv('your_data.csv')",
        '',
        '# Preprocess the data',
        f"data_matrix, factor_columns = preprocess_data(df, {', '.join(f'{k}={repr(v)}' for k, v in transform_params.items())})",
        '',
    ]
    if analysis_type == 'pca':
        code_lines += [
            '# Run PCA',
            'pcs, explained_var = run_pca(data_matrix, plot=True)',
        ]
    elif analysis_type == 'tsne':
        code_lines += [
            '# Run t-SNE',
            'tsne_coords = run_tsne(data_matrix, plot=True)',
        ]
    elif analysis_type == 'heatmap':
        code_lines += [
            '# Plot heatmap',
            'plot_heatmap(data_matrix)',
        ]
    return '\n'.join(code_lines) 