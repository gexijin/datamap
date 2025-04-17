import streamlit as st
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from sklearn.decomposition import PCA
from sklearn.manifold import TSNE
from scipy.cluster.hierarchy import linkage, dendrogram
import plotly.figure_factory as ff
import io

st.set_page_config(layout="wide")

def load_data(uploaded_file):
    if uploaded_file.name.endswith('.xlsx'):
        df = pd.read_excel(uploaded_file)
    else:
        df = pd.read_csv(uploaded_file)
    return df

def create_heatmap(df, selected_columns, cluster_rows=True, cluster_cols=True):
    # Select numeric columns for clustering
    numeric_df = df[selected_columns].select_dtypes(include=[np.number])
    
    # Create linkage matrices
    if cluster_rows:
        row_linkage = linkage(numeric_df, method='complete', metric='euclidean')
    if cluster_cols:
        col_linkage = linkage(numeric_df.T, method='complete', metric='euclidean')
    
    # Create heatmap
    fig = ff.create_dendrogram(numeric_df, orientation='left', linkagefun=lambda x: row_linkage)
    fig.update_layout(width=800, height=600)
    return fig

def create_pca_plot(df, selected_columns):
    pca = PCA(n_components=2)
    numeric_df = df[selected_columns].select_dtypes(include=[np.number])
    pca_result = pca.fit_transform(numeric_df)
    
    fig = px.scatter(x=pca_result[:, 0], y=pca_result[:, 1],
                    title='PCA Plot',
                    labels={'x': 'PC1', 'y': 'PC2'})
    return fig

def create_tsne_plot(df, selected_columns):
    tsne = TSNE(n_components=2, random_state=42)
    numeric_df = df[selected_columns].select_dtypes(include=[np.number])
    tsne_result = tsne.fit_transform(numeric_df)
    
    fig = px.scatter(x=tsne_result[:, 0], y=tsne_result[:, 1],
                    title='t-SNE Plot',
                    labels={'x': 't-SNE 1', 'y': 't-SNE 2'})
    return fig

def main():
    st.title("DataMap: Visualizing Data Matrices")
    
    # File upload
    uploaded_file = st.file_uploader("Upload your data file (CSV, TSV, or Excel)", 
                                   type=['csv', 'tsv', 'xlsx'])
    
    if uploaded_file is not None:
        df = load_data(uploaded_file)
        
        # Display data preview
        st.subheader("Data Preview")
        st.dataframe(df.head())
        
        # Column selection
        st.subheader("Select Columns for Analysis")
        selected_columns = st.multiselect("Choose columns to analyze", 
                                        df.columns.tolist(),
                                        default=df.columns.tolist()[:5])
        
        if selected_columns:
            # Visualization options
            st.subheader("Visualization Options")
            col1, col2 = st.columns(2)
            
            with col1:
                cluster_rows = st.checkbox("Cluster Rows", value=True)
            with col2:
                cluster_cols = st.checkbox("Cluster Columns", value=True)
            
            # Create visualizations
            st.subheader("Heatmap")
            heatmap_fig = create_heatmap(df, selected_columns, cluster_rows, cluster_cols)
            st.plotly_chart(heatmap_fig, use_container_width=True)
            
            st.subheader("PCA Plot")
            pca_fig = create_pca_plot(df, selected_columns)
            st.plotly_chart(pca_fig, use_container_width=True)
            
            st.subheader("t-SNE Plot")
            tsne_fig = create_tsne_plot(df, selected_columns)
            st.plotly_chart(tsne_fig, use_container_width=True)
            
            # Export options
            st.subheader("Export")
            if st.button("Export Analysis as HTML"):
                html = f"""
                <html>
                <head>
                    <title>DataMap Analysis</title>
                </head>
                <body>
                    <h1>DataMap Analysis Report</h1>
                    <h2>Heatmap</h2>
                    {heatmap_fig.to_html()}
                    <h2>PCA Plot</h2>
                    {pca_fig.to_html()}
                    <h2>t-SNE Plot</h2>
                    {tsne_fig.to_html()}
                </body>
                </html>
                """
                st.download_button(
                    label="Download HTML Report",
                    data=html,
                    file_name="datamap_analysis.html",
                    mime="text/html"
                )

if __name__ == "__main__":
    main() 