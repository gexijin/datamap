#' Run the DataMap Shiny application
#' 
#' @export
#' 
#' @return A Shiny application object
#' @examples
#' if (interactive()) {
#'   run_datamap()
#' }
#' 

source(datamap_resource("R/utilities.R"))
source(datamap_resource("R/mod_file_upload.R"))
source(datamap_resource("R/mod_transform.R"))
source(datamap_resource("R/mod_pca.R"))
source(datamap_resource("R/mod_tsne.R"))
source(datamap_resource("R/mod_heatmap.R"))
source(datamap_resource("R/mod_code_generation.R"))
source(datamap_resource("R/main_app.R"))

run_app()

