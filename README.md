## DataMap: Visualizing Data Matrices in Your Browser

To use this app, just visit this static [GitHub page](https://gexijin.github.io/datamap/) deployed from this repo. For large datasets, install it as an R package: 
```{R}
install.packages("remotes")
remotes::install_github("gexijin/datamap", upgrade = "never")
datamap::run_app()
```

DataMap is a secure, browser-based application for visualizing high-dimensional 'omics and other data matrices with heatmaps, PCA, and t-SNE. Built with R/Shiny using [shinylive](https://posit-dev.github.io/r-shinylive/), DataMap is a serverless app that is secure and scalable. 

![heatmap](https://github.com/user-attachments/assets/b649808a-d8d3-4a84-94ed-bec42a9b8f81)
![image](https://github.com/user-attachments/assets/cbdaaa45-e681-4cbd-b8ef-500b0c4b0b8a)
![tsne](https://github.com/user-attachments/assets/e732c12b-d042-475a-baaf-3424232f63ce)

## Video tutorials

Quick start: watch a 2-min video on [**YouTube.**](https://youtu.be/9G508BxzjBk) Another [ **video.**](https://www.youtube.com/watch?v=a4ioAVTcCoo)

## Features

- **Browser-based**: No installation required, runs completely in your browser
- **Interactive Visualizations**: 
  - Heatmaps with hierarchical clustering
  - Principal Component Analysis (PCA)
  - t-SNE (t-Distributed Stochastic Neighbor Embedding)
- **Exportable**: Generate R code to reproduce the plots.
- **Publication-ready plots**: Export as PNG or PDF formats
- **Multiple File Formats**: Supports CSV, TSV, TXT, and Excel files

### Data Format Requirements

Your data should be organized in a matrix format where:
- The first row must contain column headers
- The first column may contain row identifiers.
- Some columns can be categorical, which will be used to color rows.
- Column annotation can be included in the data matrix or uploaded separately.

## FAQ

**Is my data secure?**  
Yes. DataMap runs entirely in your browser. After loading the app, it works fine even if you disconnect the internet. 

**What browsers are supported?**  
Chrome, Firefox (slower to load), Edge, and Safari.

**Can I use DataMap offline?**  
Yes, once loaded in your browser, DataMap can operate without an internet connection. Alternatively, you can intsall DataMap as an R package.

**Limitations?**  
Slower in the browser when clustering 5000 rows or columns. Can take up to 2 minutes. For large datasets, install and use it as an R package.

**Why did you write DataMap?**  
A. I love heatmaps! B. I wanted to do an vibe coding experiment. Claude.ai wrote 95% of the code. See my [blog](https://www.ge-lab.org/2025/04/21/extreme-vibe-coding-the-making-of-datamap/) on how DataMap was developed.

## Cite our preprint

If you use DataMap in your research, please cite:

> Ge, X. (2025). DataMap: A Portable Application for Visualizing High-Dimensional Data,	[arXiv:2504.08875](https://arxiv.org/abs/2504.08875), 2025.

**Dr. Xijin Ge** is a Professor at South Dakota State University. [LinkedIn](https://www.linkedin.com/in/steven-ge-ab016947/), [BlueSky.](https://bsky.app/profile/stevenge.bsky.social)

