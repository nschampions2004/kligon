#' Create a formula of interactions of a certain depth (default: 2) from
#'  a vector or list of column names
#'
#' @param vector_or_list: the vector or list of of columns you want to interact
#' @param interaction_depth: how deep of interactions do you want?
#' @param response: the response variable you're trying to predict for
#'
#' @examples
#' iris
#' iris$is_setosa <- ifelse(iris$Species == "setosa", 1, 0)
#' iris_trim <- iris[, -which(names(iris) %in% c("Species"))]
#' col_names <- names(iris_trim)[names(iris_trim) != "is_setosa"]
#' create_interactions(col_names,
#'                    interaction_depth = 2,
#'                    response = "is_setosa")

create_interactions <- function(vector_or_list,
                                interaction_depth=2,
                                response = NULL) {
  #'
  #' @param: vector_or_list- vector or list of columns to interact
  #' @param: interaction_depth- how deep do you want the interactions to go?
  #' @param: response- the target variable on the left side of the formula

  require(glue)

  formula_list <- list()
  initial_stem <- glue::glue("{response} ~ ")
  for (i in 1:length(vector_or_list)) {
    if (i == 1) {
      running_interactions <- glue::glue("{vector_or_list[i]}")
      mf <- glue::glue("{initial_stem} ({running_interactions})^{interaction_depth}")
    } else {
      running_interactions <- glue::glue("{running_interactions}+{vector_or_list[i]}")
      mf <- glue::glue("{initial_stem} ({running_interactions})^{interaction_depth}")
    }

    formula_list[i] <- mf
  }

  formula_list
}
