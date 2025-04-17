## DataMap: Visualizing Data Matrices in Your Browser

To use this app, just visit this [static GitHub page.](https://gexijin.github.io/datamap/) For large datasets, install it as an R package for local execution: 
```{R}
install.packages("remotes")
remotes::install_github("gexijin/datamap", ref = "rPackage", upgrade = "never")
DataMap::run_datamap()
```

DataMap is a secure, browser-based application for visualizing high-dimensional 'omics and other data matrices with heatmaps, PCA, and t-SNE. Built with Shiny and running entirely in your browser through Shinylive technology, DataMap ensures your data never leaves your device.

![heatmap](https://github.com/user-attachments/assets/b649808a-d8d3-4a84-94ed-bec42a9b8f81)
![image](https://github.com/user-attachments/assets/cbdaaa45-e681-4cbd-b8ef-500b0c4b0b8a)








## Features

- **Browser-based**: No installation required, runs completely in your browser
- **Secure**: Your data stays on your device, never uploaded to any server
- **Interactive Visualizations**: 
  - Heatmaps with hierarchical clustering
  - Principal Component Analysis (PCA)
  - t-SNE (t-Distributed Stochastic Neighbor Embedding)
- **Exportable**: Generate reproducible R code for your analysis
- **Multiple File Formats**: Supports CSV, TSV, TXT, and Excel files
- **Customizable**: Extensive visualization parameters

### Data Format Requirements

Your data should be organized in a matrix format where:
- The first row must contain column headers
- The first column may contain row identifiers.
- Some columns can be categorical, which will be used to color rows.
- Column annotation can be uploaded separately.


## FAQ

**Is my data secure?**  
Yes. DataMap runs entirely in your browser. Your data never leaves your device.

**What browsers are supported?**  
Chrome, Firefox, Edge, and Safari (latest versions recommended).

**Can I use DataMap offline?**  
Yes, once loaded in your browser, DataMap can operate without an internet connection.

**Limitations?**  
Slower in the browser when clustering 5000 rows or columns. Can take up to 2 minutes.

## How to Cite

If you use DataMap in your research, please cite it as:

> Ge, X. (2025). DataMap: A secure browser-based application for visualizing high-dimensional data matrices. [Software]. Available from: https://gexijin.github.io/datamap/

**Dr. Xijin Ge** is a Professor at South Dakota State University. [LinkedIn](https://www.linkedin.com/in/steven-ge-ab016947/), [BlueSky.](https://bsky.app/profile/stevenge.bsky.social)

# DataMap Project

This project uses Python with various data science packages for data analysis and visualization.

## Prerequisites

- Python 3.8 or higher
- pip (Python package installer)

## Installation

### Windows Installation

1. Open PowerShell in the project directory
2. Run the installation script:
   ```powershell
   .\install_packages.ps1
   ```
   If you encounter a PowerShell execution policy error, run:
   ```powershell
   Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
   ```

### Unix-like Systems (Linux/macOS) Installation

1. Open a terminal in the project directory
2. Make the installation script executable:
   ```bash
   chmod +x install_packages.sh
   ```
3. Run the installation script:
   ```bash
   ./install_packages.sh
   ```

## Manual Installation

If you prefer to install packages manually:

1. Install `uv`:
   ```bash
   pip install uv
   ```

2. Create a virtual environment:
   ```bash
   uv venv
   ```

3. Activate the virtual environment:
   - Windows:
     ```powershell
     .\venv\Scripts\Activate.ps1
     ```
   - Unix-like systems:
     ```bash
     source venv/bin/activate
     ```

4. Install required packages:
   ```bash
   uv pip install -r requirements.txt
   ```

## Required Packages

The project requires the following Python packages:
- numpy
- pandas
- matplotlib
- seaborn
- scipy
- scikit-learn
- openpyxl

These packages will be automatically installed when running the installation script.

## Troubleshooting

If you encounter any issues during installation:

1. Ensure Python and pip are properly installed
2. Check your internet connection
3. Try running the installation commands with administrator/root privileges
4. If using Windows, make sure PowerShell execution policy allows script execution

## License

This project is licensed under the MIT License - see the LICENSE file for details.

