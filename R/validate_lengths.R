#' Validate argument lengths
#'
#' All arguments must be of the same length, or they call all be of
#' length 1 except for a single argument
#'
#' @param ... named or positional arguments
#'
#' @return `TRUE`, or throw error
validate_lengths <- function(...) {
  # named list of the arguments
  args <- list(...)
  # number of arguments
  n_args <- length(args)
  # vector of lengths of each argument, potentially named
  lens <- sapply(args, length)
  # number of unique argument lengths
  n_unique_lens <- length(unique(lens))
  # number of arguments of length 1
  n_len_1 <- sum(lens == 1)
  # vector of number of times each argument-length occurs
  tab <- tabulate(lens)

  # otherwise, check that all arguments except one are of
  # length 1
  if (n_unique_lens == 1) {
    # if all arguments are the same length, pass!
    return(TRUE)
  } else if (n_unique_lens == 2 && n_args == n_len_1 + 1) {
    # if there are two unique lengths, and only one argument
    # is not of length 1, also pass!
    return(TRUE)
  } else {
    stop(paste0(c("Mismatched argument lengths:", lens, collapse = " ")))
  }
}
