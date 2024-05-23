#' Function to prepare data from other cubes and accessible datasets
#'
#' All functions developed by Stellenbosch BioMath for the b-cubed project
#'
#' @param arg1 Explain parameters here
#' @param ... Explain parameters here
#'
#' @return Explain what outputs are returned
#' @export
#'
#' @examples dataGEN(1)
#' @examples dataGEN('Also works for character strings')

dataGEN = function(arg1,...){
  # prepare data from other cubes and accessible datasets
  # spatial polygons, shapefiles, squares (corner coordinates) for spatial extent
  # spatial resolution, specified and default
  # alien status of identified species list
  # [we need to make a decision on species list completeness (this needs to consider other cubes and SDM cubes, etc.)]
  # temporal range
  # option = 1, 2; 1 for dissim() and 2 for invasib()
  # Return, site by species, site by xyt, site by env, site by site distance; species by trait, species by species phylogenetic distance, matrices (and default setting if null)
  return(arg1)
}
