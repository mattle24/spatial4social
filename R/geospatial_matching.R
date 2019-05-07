#' Get Sub-Geometries
#'
#' Get the sub-geometries for a macro geometry
#' @param macro_geometry_sf An `sf` object for one macro geometry
#' @param sub_geometry_sf An `sf` object for sub-geometries that might be in the
#'   macro geometry
#' @param wt_threshold The threshold of intersecting area to filter out.
#'   Defaults to 0.01.
#'
#' @return An tidy `sf` for tracts that intersect the neighborhood
#'
#' @importFrom sf st_area st_intersection
#' @importFrom magrittr %>%
#' @export
get_sub_geometries <- function(macro_geometry_sf, sub_geometry_sf, wt_threshold = 0.01) {
  sub_geometry_sf$a1 <- st_area(sub_geometry_sf)
  intersect_sf <- st_intersection(macro_geometry_sf, sub_geometry_sf)
  intersect_sf$a2 <- st_area(intersect_sf)
  intersect_sf$wt <- as.numeric(intersect_sf$a2 / intersect_sf$a1)
  intersect_sf %>%
    dplyr::filter(wt > wt_threshold) %>%
    dplyr::select(-a1, -a2)
}

#' Sub-geometry to Macro-Geometry
#'
#' Match sub-geometries to macro-geometries
#' @param macro_geometry_sf The `sf` object with macro geometries
#' @param sub_geometry_sf An `sf` object for sub-geometries that might be in the
#'   macro geometries
#' @param wt_threshold The threshold of intersecting area to filter out.
#'   Defaults to 0.01.
#'
#' @return A tidy `sf` object with sub-geometries and their corresponding neighborhoods
#' @importFrom sf st_area st_intersection
#' @importFrom magrittr %>%
#' @export
sub_geometry_to_macro <- function(macro_geometry_sf, sub_geometry_sf, wt_theshold = 0.01) {
  # check that CRS are the same
  if (sf::st_crs(macro_geometry_sf) != sf::st_crs(sub_geometry_sf)) {
    rlang::warn("CRS for `macro_geometry_sf` and `sub_geometry_sf` do not match. Casting macro-geomety CRS")
    macro_geometry_sf <- macro_geometry_sf %>%
      lwgeom::st_transform_proj(sf::st_crs(sub_geometry_sf))
  }

  # one row for each sub-geometry, one col for each macro geometry
  intersects_matrix <- sf::st_intersects(macro_geometry_sf, sub_geometry_sf, sparse = FALSE)

  res <- NULL # init
  for (n in 1:nrow(intersects_matrix)) {
    sub_geometries_in_macro_geometry <- get_sub_geometries(macro_geometry_sf[n, ],
                                                         sub_geometry_sf[as.vector(intersects_matrix[n, ]), ],
                                                         wt_theshold)
    if (is.null(res)) {
      res <- sub_geometries_in_macro_geometry
    } else {
      res <- res %>%
        rbind(sub_geometries_in_macro_geometry)
    }
  }
  return(res)
}
