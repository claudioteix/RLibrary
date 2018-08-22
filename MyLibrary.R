#---------------------------------------------------#
# Some functions from Data Explorer Package
#---------------------------------------------------#
#' Get all missing columns
#'
#' Get number of columns with all values missing
#' @param dt input data object.
#' @return a named logical vector indicating if a column has only missing values.
#' @import data.table
.getAllMissing <- function(dt) {
  if (!is.data.table(dt)) dt <- data.table(dt)
  sapply(dt, function(x) {
    sum(is.na(x)) == length(x)
  })
}

split_columns <- function(data) {
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Find indicies for continuous features
  all_missing_ind <- .getAllMissing(data)
  ind <- sapply(data[, which(!all_missing_ind), with = FALSE], is.numeric)
  ## Count number of discrete, continuous and all-missing features
  n_all_missing <- sum(all_missing_ind)
  n_continuous <- sum(ind)
  n_discrete <- ncol(data) - n_continuous - n_all_missing
  ## Create object for continuous features
  continuous <- data[, which(ind), with = FALSE]
  ## Create object for discrete features
  discrete <- data[, which(!ind), with = FALSE]
  ## Set data class back to original
  if (!is_data_table) class(discrete) <- class(continuous) <- data_class
  ## Set return object
  return(
    list(
      "discrete" = discrete,
      "continuous" = continuous,
      "num_discrete" = n_discrete,
      "num_continuous" = n_continuous,
      "num_all_missing" = n_all_missing
    )
  )
}

SplitColType <- function(data) {
  .Deprecated("split_columns")
  split_columns(data)
}

introduce <- function(data) {
  split_data <- split_columns(data)
  data.frame(
    "rows" = nrow(data),
    "columns" = ncol(data),
    "discrete_columns" = split_data[["num_discrete"]],
    "continuous_columns" = split_data[["num_continuous"]],
    "all_missing_columns" = split_data[["num_all_missing"]],
    "total_missing_values" = sum(is.na(data)),
    "total_observations" = nrow(data) * ncol(data),
    "memory_usage" = as.numeric(object.size(data))
  )
}

