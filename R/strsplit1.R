#' Split a String
#'
#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")
#'
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}


#' Categorical Variables to Binary
#'
#' @param x Categorical variable to be categorized.
#'
#' @return A mutated variable.
#' @export
#'
#' @examples
#' asdf
cat_to_bin <- function(x) {
  out <- dplyr::case_when(
    grepl("no|female|not|rarely|never|sometimes", x, ignore.case = TRUE) ~ 0,
    grepl("yes|male|daily|always|often", x, ignore.case = TRUE) ~ 1,
    TRUE ~ NA_real_
  )

  dplyr::as_tibble(out)
}
