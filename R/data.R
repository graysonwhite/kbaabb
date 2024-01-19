#' Population dataset for San Juan County, Washington, USA
#'
#' Population level data for each 90m pixel in San Juan County, Washington, USA.
#' For more information on the auxiliary data, see Wieczorek et al. (2023+). 
#' 
#'
#' @format `SJC_population`:
#' A data frame and sf object with 55,570 rows and 5 columns:
#' \describe{
#'   \item{island}{Name of the island that the pixel occurs on.}
#'   \item{tcc}{Auxiliary data. Tree canopy cover value at given pixel.}
#'   \item{elev}{Auxiliary data. Elevation value at given pixel.}
#'   \item{wc3cl}{Auxiliary data. Three-class world cover value at a given 
#'   pixel.}
#'   \item{geometry}{`sf` geometry.}
#' }
"SJC_population"

#' Sample dataset for San Juan County, Washington, USA
#'
#' Survey data collected by the USDA Forest Service, Forest Inventory & Analysis
#' Program (FIA). This dataset contains plot-level data for biomass as collected 
#' and reported by the FIA. Further, auxiliary data has been matched with this
#' plot level data to match the population dataset. 
#' 
#'
#' @format `SJC_sample`:
#' A data frame and sf object with 25 rows and 8 columns:
#' \describe{
#'   \item{plt_cn}{Name of the island that the pixel occurs on.}
#'   \item{biomass}{Average biomass measurement calculated from ground-collected
#'   FIA data. Units: short tons/acre.}
#'   \item{tcc}{Auxiliary data. Tree canopy cover value at given pixel.}
#'   \item{elev}{Auxiliary data. Elevation value at given pixel.}
#'   \item{wc3cl}{Auxiliary data. Three-class world cover value at a given 
#'   pixel.}
#'   \item{measurement_year}{Year in which the FIA plot was measured.}
#'   \item{island}{Name of the island that the plot is on.}
#'   \item{geometry}{`sf` geometry.}
#' }
"SJC_sample"

#' Polygon boundary dataset for San Juan County, Washington, USA
#'
#' An `sf` object containing the polygon boundaries of San Juan County,
#' Washington, USA. 
#' 
#'
#' @format `SJC_boundary`:
#' A data frame and sf object with 124 rows and 2 columns:
#' \describe{
#'   \item{island}{Name of the island that the polygon represents.}
#'   \item{geometry}{`sf` geometry.}
#' }
"SJC_boundary"