#' A function to save and subsequently archive and object
#'
#' @param object: the object to save
#' @param file_destination: where you want to save the object
#'
#' @examples
#'path_to_save <- file.path('iris.rds')
#'archive_it(object = iris,
#'            file_destination = path_to_save)


archive_it <- function(object, file_destination) {
  #' @param object: the object to save
  #' @param file_destination: where you want to save the object

  require(readr)
  require(ggplot2)
  require(writexl)

  # helpers
  directory <- dirname(file_destination)
  file_start <- tools::file_path_sans_ext(basename(file_destination))

  # archiving
  formatted_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  archive_path <- file.path(directory, 'archive')
  extension <- tools::file_ext(file_destination)
  archive_file <- file.path(archive_path, glue::glue("{file_start}_{formatted_time}.{extension}"))

  if (file.exists(file_destination)) {
    if (!dir.exists(archive_path)) {
      dir.create(file.path(archive_path))
    }
  }

  file.copy(file_destination, archive_file)

  if (extension == "csv") {
    readr::write_csv(object, file_destination)
  } else if (extension == "png") {
    ggplot2::ggsave(plot = object,
           filename =  file_destination)
  } else if (extension == "rds") {
    saveRDS(object = object, file = file_destination)
  } else if (extension %in% c("xlsx", "xls")) {
    writexl::write_xlsx(x = object, path = file_destination)
  }
}

interaction_creator <- function(vector_or_list, interaction_depth=2) {
  #'
  #' @param: vector_or_list- vector or list of columns to interact
  #' @param: interaction_depth- how deep do you want the interactions to go?
  #'

  require(glue)

  formula_list <- list()
  initial_stem <- "sex ~ "
  for (i in 1:length(col_names)) {
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

