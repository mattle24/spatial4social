% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geospatial_matching.R
\name{sub_geometry_to_macro}
\alias{sub_geometry_to_macro}
\title{Sub-geometry to Macro-Geometry}
\usage{
sub_geometry_to_macro(macro_geometry_sf, sub_geometry_sf,
  wt_theshold = 0.01)
}
\arguments{
\item{macro_geometry_sf}{The `sf` object with macro geometries}

\item{sub_geometry_sf}{An `sf` object for sub-geometries that might be in the
macro geometries}

\item{wt_threshold}{The threshold of intersecting area to filter out.
Defaults to 0.01.}
}
\value{
A tidy `sf` object with sub-geometries and their corresponding neighborhoods
}
\description{
Match sub-geometries to macro-geometries
}
