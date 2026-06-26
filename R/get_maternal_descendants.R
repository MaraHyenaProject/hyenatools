#' Get all maternal descendants of a matriarch
#'
#' Returns a data frame of every maternal descendant of the supplied matriarch,
#' found by recursively following the `mom` column of tblHyenas. Each descendant
#' is labelled with the number of maternal generations separating it from the
#' matriarch (direct offspring = 1, grand-offspring = 2, and so on). The
#' matriarch herself is not included. This function uses tblHyenas.
#'
#' @param matriarch A string specifying the hyena id of the matriarch.
#' @param generation_step Internal counter used by the recursion. Leave at the
#'   default of 1 when calling.
#'
#' @return A data frame with one row per descendant and columns `id`,
#'   `birthdate`, `mom`, and `generation_step`. Returns NULL if the matriarch has
#'   no maternal descendants.
#'
#' @examples
#' ## all maternal descendants of kb
#' get_maternal_descendants('kb')
#'
#' @export
#'

get_maternal_descendants <- function(matriarch, generation_step = 1) {
  # Direct offspring of the current matriarch(s)
  offspring <- dplyr::filter(tblHyenas, mom %in% matriarch)[, c('id', 'birthdate', 'mom')]

  # No offspring -> end of this maternal line
  if (nrow(offspring) == 0) {
    return(NULL)
  }

  # Label the current generation
  current_gen <- data.frame(offspring, generation_step = generation_step)

  # Recurse into each offspring to collect deeper generations
  next_gen <- do.call(rbind, lapply(offspring$id, function(x) {
    get_maternal_descendants(x, generation_step + 1)
  }))

  # Combine this generation with everything below it
  if (!is.null(next_gen) && nrow(next_gen) > 0) {
    result <- rbind(current_gen, next_gen)
  } else {
    result <- current_gen
  }

  rownames(result) <- NULL
  return(result)
}
