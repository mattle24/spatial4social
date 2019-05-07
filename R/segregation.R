#' Calculate segregation
#'
#' Calculate segregation for macro geometries. Method definitions from the
#' \href{https://www.census.gov/hhes/www/housing/resseg/pdf/app_b.pdf}{US
#' Census}.
#'
#' @param macro_geometry_sf An `sf` object. Contains simple features for
#'   macro-geometries (eg cities)
#' @param sub_geometry_sf An `sf` object. Contains simple features for
#'  sub-geometries (eg tracts or neighborhoods) and demographic data.
#' @param majority_pop Character. The name of the column with the majority
#'  race/ ethnicity county.
#' @param minority_pop Character. The name of the column with the minority
#'  race/ ethnicity county.
#' @param method Character. The method to use to calculate gentrification.
#'  One of "dissimilarity_index", "gini", "entropy", or "interaction".
#'  Currently only one at a time is supported.
#'
#' @return `macro_geometry_sf` with one more column containing the segregation
#'   measure selected.
#'
#' @include geospatial_matching.R
#' @export
calculate_segregation <- function(macro_geometry_sf, sub_geometry_sf,
                                  majority_pop, minority_pop,
                                 method = c("dissimilarity_index", "gini",
                                            "entropy", "interaction")) {
  majority_pop <- rlang::enexpr(majority_pop)
  minority_pop <- rlang::enexpr(minority_pop)

  # add .id for grouping in later operations
  macro_geometry_sf <- macro_geometry_sf %>%
    dplyr::mutate(.id = dplyr::row_number())

  tmp <- sub_geometry_to_macro(macro_geometry_sf, sub_geometry_sf, 0.01)

  tmp$geometry <- NULL # remove geometry because otherwise calculations take a long time

  tmp <- tmp %>%
    dplyr::select(.id, majority_pop = !!majority_pop,
                  minority_pop = !!minority_pop, wt) %>%
    rlang::call2(.fn = paste0(".", method), .) %>%
    rlang::eval_bare()

  macro_geometry_sf %>%
    dplyr::left_join(tmp, by = ".id") %>%
    dplyr::select(-.id)
}

# majority_pop <- "white_non_hispanic"
# minority_pop <- "afam_non_hispanic"

#' Dissimilarity Index
.dissimilarity_index <- function(data) {
  data %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(
      t =  (majority_pop + minority_pop) * wt # pop in subgeometry
      ,p = (minority_pop * wt) / t # prop minority in tract
    ) %>%
    dplyr::summarise(
      Total = sum(t, na.rm = TRUE) # total pop in macro geometry
      ,Prop = sum(minority_pop, na.rm = TRUE) / Total # prop minority in macro geometry
      ,dissimilarity_index = sum(t * abs(p - Prop), na.rm = TRUE) / (2 * Total * Prop * (1 - Prop))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.id, dissimilarity_index)
}


#' Gini
.gini <- function(data) {
  data <- data %>%
    dplyr::mutate(
      t_i = (majority_pop + minority_pop) * wt # total pop in area i
      ,p_i = (minority_pop * wt) / t_i # minority pop in area i
    )

  data$step_2 <- rep(NA_real_, nrow(data))
  for (i in 1:nrow(data)) {
    id = data$.id[i]
    data$step_2[i] <- sum(data$t_i[i] %*% data$t_i[data$.id == id] *
                            abs(data$p_i[i] - data$p_i[data$.id == id]),
                           na.rm = TRUE)
  }

  data %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(
      Total = sum(t_i, na.rm = TRUE)
      ,Prop = sum(minority_pop * wt, na.rm = TRUE) / sum(t_i, na.rm = TRUE)
      ,numerator = sum(step_2, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      gini = numerator / (2 * (Total^2) * Prop * (1 - Prop))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(.id, gini)
}

#' Entropy
.entropy <- function(data) {
  data %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(
      p_i = minority_pop / (majority_pop + minority_pop)
      ,e_i = .entropy_helper(p_i)
      ,t_i = (majority_pop + minority_pop) * wt
      ,Prop = sum(minority_pop * wt, na.rm = TRUE) /
        sum((majority_pop + minority_pop) * wt, na.rm = TRUE)
      ,E = .entropy_helper(Prop)
      ,Total = sum((majority_pop + minority_pop) * wt, na.rm = TRUE)
    ) %>%
    dplyr::summarise(
      entropy = sum(t_i * (E - e_i) / (E * Total), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}

# Entropy helper
.entropy_helper <- function(p) {
  p * log(1/p) + (1 - p) * log(1 / (1 - p))
}

#' Interaction
.interaction <- function(data) {
  data %>%
    dplyr::group_by(.id) %>%
    dplyr::mutate(
      t_i = (majority_pop + minority_pop) * wt
      ,x_i = (minority_pop * wt) / t_i
      ,X = (minority_pop * wt)
      ,y_i = (majority_pop * wt) / t_i
    ) %>%  # total pop
    dplyr::summarise(
      interaction = sum((x_i / X) * (y_i / t_i), na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
}
