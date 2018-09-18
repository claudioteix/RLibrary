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

#' Plot missing value profile
#'
#' This function returns and plots frequency of missing values for each feature.
#' @param data input data
#' @param title plot title
#' @param ggtheme complete ggplot2 themes. The default is \link{theme_gray}.
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @keywords plot_missing
#' @aliases PlotMissing
#' @details The returned object is suppressed by \link{invisible}.
#' @details To change default font family and size, you may pass \code{base_size} and \code{base_family} to \code{ggtheme} options, e.g., \code{ggtheme = theme_gray(base_size = 15, base_family = "serif")}
#' @details \code{theme_config} argument expects all inputs to be wrapped in a list object, e.g., to change the text color: \code{theme_config = list("text" = element_text(color = "blue"))}
#' @return missing value information, such as frequency, percentage and suggested action.
#' @import data.table
#' @import ggplot2
#' @importFrom scales comma
#' @export plot_missing PlotMissing
#' @examples
#' # Load packages
#' library(data.table)
#'
#' # Add missing values to iris data
#' dt <- data.table(iris)
#' for (j in 1:4) set(dt, i = sample(150, j * 30), j, value = NA_integer_)
#'
#' # Plot and assign missing value information
#' na_profile <- plot_missing(dt)
#' na_profile
#'
#' # Drop columns with more than 50% missing values
#' drop_columns(dt, as.character(na_profile[pct_missing >= 0.5][["feature"]]))
#' plot_missing(dt)

plot_missing <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = c("bottom"))) {
  ## Declare variable first to pass R CMD check
  feature <- num_missing <- pct_missing <- group <- NULL
  ## Check if input is data.table
  is_data_table <- is.data.table(data)
  ## Detect input data class
  data_class <- class(data)
  ## Set data to data.table
  if (!is_data_table) data <- data.table(data)
  ## Extract missing value distribution
  missing_value <- data.table(
    "feature" = names(data),
    "num_missing" = sapply(data, function(x) {sum(is.na(x))})
  )
  missing_value[, feature := factor(feature, levels = feature[order(-rank(num_missing))])]
  missing_value[, pct_missing := num_missing / nrow(data)]
  missing_value[pct_missing < 0.05, group := "Good"]
  missing_value[pct_missing >= 0.05 & pct_missing < 0.4, group := "OK"]
  missing_value[pct_missing >= 0.4 & pct_missing < 0.8, group := "Bad"]
  missing_value[pct_missing >= 0.8, group := "Remove"][]
  ## Set data class back to original
  if (!is_data_table) class(missing_value) <- data_class
  ## Create ggplot object
  output <- ggplot(missing_value, aes_string(x = "feature", y = "num_missing", fill = "group")) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(100 * pct_missing, 2), "%"))) +
    scale_fill_manual("Group", values = c("Good" = "#1a9641", "OK" = "#a6d96a", "Bad" = "#fdae61", "Remove" = "#d7191c"), breaks = c("Good", "OK", "Bad", "Remove")) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    xlab("Features") + ylab("Number of missing rows") +
    ggtitle(title) +
    ggtheme +
    do.call(theme, theme_config)
  ## Print plot
  print(output)
  ## Set return object
  return(invisible(missing_value))
}

PlotMissing <- function(data, title = NULL) {
  .Deprecated("plot_missing")
  plot_missing(data = data, title = title)
}

#' Create bar charts for discrete features
#'
#' This function creates frequency bar charts for each discrete feature.
#' @param data input data
#' @param with name of continuous feature to be summed. Default is \code{NULL}, i.e., frequency.
#' @param maxcat maximum categories allowed for each feature. The default is 50. More information in 'Details' section.
#' @param order_bar logical, indicating if bars should be ordered.
#' @param title plot title
#' @param ggtheme complete ggplot2 themes. The default is \link{theme_gray}.
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @keywords plot_bar
#' @aliases BarDiscrete
#' @details If a discrete feature contains more categories than \code{maxcat} specifies, it will not be passed to the plotting function.
#' @details To change default font family and size, you may pass \code{base_size} and \code{base_family} to \code{ggtheme} options, e.g., \code{ggtheme = theme_gray(base_size = 15, base_family = "serif")}
#' @details \code{theme_config} argument expects all inputs to be wrapped in a list object, e.g., to change the text color: \code{theme_config = list("text" = element_text(color = "blue"))}
#' @import data.table
#' @import ggplot2
#' @import gridExtra
#' @importFrom scales comma
#' @importFrom stats reorder
#' @importFrom tools toTitleCase
#' @export plot_bar BarDiscrete
#' @examples
#' # Load diamonds dataset from ggplot2
#' library(ggplot2)
#' data("diamonds", package = "ggplot2")
#'
#' # Plot bar charts for diamonds dataset
#' plot_bar(diamonds)
#' plot_bar(diamonds, maxcat = 5)
#'
#' # Plot bar charts with `price` feature
#' plot_bar(diamonds, with = "price")
#'
#' # Plot bar charts with preset ggplot2 themes
#' plot_bar(diamonds, ggtheme = theme_light())
#' plot_bar(diamonds, ggtheme = theme_minimal(base_size = 20))
#'
#' # Plot bar charts with customized theme components
#' plot_bar(diamonds,
#' theme_config = list(
#'   "plot.background" = element_rect(fill = "yellow"),
#'   "aspect.ratio" = 1
#' ))

plot_bar <- function(data, with = NULL, maxcat = 50, order_bar = TRUE, title = NULL, ggtheme = theme_gray(), theme_config = list()) {
  ## Declare variable first to pass R CMD check
  frequency <- agg_by <- NULL
  ## Check if input is data.table
  if (!is.data.table(data)) data <- data.table(data)
  ## Stop if no discrete features
  if (split_columns(data)$num_discrete == 0) stop("No Discrete Features!")
  ## Get discrete features
  discrete <- split_columns(data)$discrete
  ## Drop features with categories greater than `maxcat`
  ind <- .ignoreCat(discrete, maxcat = maxcat)
  if (length(ind)) {
    message(length(ind), " columns ignored with more than ", maxcat, " categories.\n", paste0(names(ind), ": ", ind, " categories\n"))
    drop_columns(discrete, names(ind))
  }
  ## Calculate number of pages
  p <- ncol(discrete)
  pages <- ceiling(p / 9L)
  for (pg in seq.int(pages)) {
    ## Subset data by column
    subset_data <- discrete[, seq.int(9L * pg - 8L, min(p, 9L * pg)), with = FALSE]
    n_col <- ifelse(ncol(subset_data) %% 3L, ncol(subset_data) %/% 3L + 1L, ncol(subset_data) %/% 3L)
    ## Create ggplot object
    plot <- lapply(
      seq_along(subset_data),
      function(j) {
        if (is.null(with)) {
          x <- subset_data[, j, with = FALSE]
          agg_x <- x[, list(frequency = .N), by = names(x)]
        } else {
          if (!is.numeric(data[[with]])) stop("`with` should be continuous!")
          x <- data.table(subset_data[, j, with = FALSE], "agg_by" = data[[with]])
          agg_x <- x[, list(frequency = sum(agg_by, na.rm = TRUE)), by = eval(names(x)[1])]
        }
        if (order_bar) {
          base_plot <- ggplot(agg_x, aes(x = reorder(get(names(agg_x)[1]), frequency), y = frequency))
        } else {
          base_plot <- ggplot(agg_x, aes_string(x = names(agg_x)[1], y = "frequency"))
        }
        base_plot +
          geom_bar(stat = "identity") +
          scale_y_continuous(labels = comma) +
          coord_flip() +
          xlab(names(agg_x)[1]) + ylab(ifelse(is.null(with), "Frequency", toTitleCase(with))) +
          ggtheme +
          do.call(theme, theme_config)
      }
    )
    ## Print plot object
    if (pages > 1) {
      suppressWarnings(do.call(grid.arrange, c(plot, ncol = n_col, nrow = 3L, top = title, bottom = paste("Page", pg))))
    } else {
      suppressWarnings(do.call(grid.arrange, c(plot, top = title)))
    }
  }
}

BarDiscrete <- function(data, maxcat = 1500, order_bar = TRUE, title = NULL) {
  .Deprecated("plot_bar")
  plot_bar(data = data, maxcat = maxcat, order_bar = order_bar, title = title)
}

#' Create histogram for continuous features
#'
#' This function creates histogram for each continuous feature.
#' @param data input data
#' @param title plot title
#' @param ggtheme complete ggplot2 themes. The default is \link{theme_gray}.
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @param \dots other arguments to be passed to \link{geom_histogram}.
#' @keywords plot_histogram
#' @aliases HistogramContinuous
#' @details To change default font family and size, you may pass \code{base_size} and \code{base_family} to \code{ggtheme} options, e.g., \code{ggtheme = theme_gray(base_size = 15, base_family = "serif")}
#' @details \code{theme_config} argument expects all inputs to be wrapped in a list object, e.g., to change the text color: \code{theme_config = list("text" = element_text(color = "blue"))}
#' @import data.table
#' @import ggplot2
#' @importFrom scales comma
#' @importFrom stats na.omit
#' @import gridExtra
#' @export plot_histogram HistogramContinuous
#' @seealso \link{geom_histogram} \link{plot_density}
#' @examples
#' # Plot iris data
#' plot_histogram(iris)
#'
#' # Plot random data with customized geom_histogram settings
#' set.seed(1)
#' data <- cbind(sapply(seq.int(4L), function(x) {rnorm(1000, sd = 30 * x)}))
#' plot_histogram(data, breaks = seq(-400, 400, length = 50))
#'
#' # Plot histogram with preset ggplot2 themes
#' library(ggplot2)
#' plot_histogram(data, ggtheme = theme_light())
#' plot_histogram(data, ggtheme = theme_minimal(base_size = 15))
#'
#' # Plot histogram with customized theme components
#' plot_histogram(data,
#' theme_config = list(
#'   "plot.background" = element_rect(fill = "yellow"),
#'   "aspect.ratio" = 1
#' ))

plot_histogram <- function(data, title = NULL, ggtheme = theme_gray(), theme_config = list(), ...) {
  if (!is.data.table(data)) data <- data.table(data)
  ## Stop if no continuous features
  if (split_columns(data)$num_continuous == 0) stop("No Continuous Features")
  ## Get continuous features
  continuous <- split_columns(data)$continuous
  ## Get dimension
  n <- nrow(continuous)
  p <- ncol(continuous)
  ## Calculate number of pages
  pages <- ceiling(p / 16L)
  for (pg in seq.int(pages)) {
    ## Subset data by column
    subset_data <- continuous[, seq.int(16L * pg - 15L, min(p, 16L * pg)), with = FALSE]
    setnames(subset_data, make.names(names(subset_data)))
    n_col <- ifelse(ncol(subset_data) %% 4L, ncol(subset_data) %/% 4L + 1L, ncol(subset_data) %/% 4L)
    ## Create ggplot object
    plot <- lapply(
      seq_along(subset_data),
      function(j) {
        x <- na.omit(subset_data[, j, with = FALSE])
        ggplot(x, aes_string(x = names(x))) +
          geom_histogram(bins = 30L, ...) +
          scale_x_continuous(labels = comma) +
          scale_y_continuous(labels = comma) +
          ylab("Frequency") +
          ggtheme +
          do.call(theme, theme_config)
      }
    )
    ## Print plot object
    if (pages > 1) {
      suppressWarnings(do.call(grid.arrange, c(plot, ncol = n_col, nrow = 4L, top = title, bottom = paste("Page", pg))))
    } else {
      suppressWarnings(do.call(grid.arrange, c(plot, top = title)))
    }
  }
}

HistogramContinuous <- function(data, title = NULL, ...) {
  .Deprecated("plot_histogram")
  plot_histogram(data = data, title = title, ...)
}

#' Create correlation heatmap for discrete features
#'
#' This function creates a correlation heatmap for all discrete categories.
#' @param data input data
#' @param type column type to be included in correlation calculation. "all" for all columns, "discrete" for discrete features, "continuous" for continuous features.
#' @param maxcat maximum categories allowed for each discrete feature. The default is 20.
#' @param title plot title
#' @param ggtheme complete ggplot2 themes. The default is \link{theme_gray}.
#' @param theme_config a list of configurations to be passed to \link{theme}.
#' @param \dots other arguments to be passed to \link{cor}.
#' @keywords plot_correlation
#' @aliases CorrelationDiscrete CorrelationContinuous
#' @details For discrete features, the function first dummifies all categories, then calculates the correlation matrix (see \link{cor}) and plots it.
#' @details To change default font family and size, you may pass \code{base_size} and \code{base_family} to \code{ggtheme} options, e.g., \code{ggtheme = theme_gray(base_size = 15, base_family = "serif")}
#' @details \code{theme_config} argument expects all inputs to be wrapped in a list object, e.g., to change the text color: \code{theme_config = list("text" = element_text(color = "blue"))}
#' @import data.table
#' @import ggplot2
#' @importFrom stats cor
#' @export plot_correlation CorrelationDiscrete CorrelationContinuous
#' @examples
#' # Load diamonds dataset from ggplot2
#' data("diamonds", package = "ggplot2")
#'
#' # Plot correlation heatmap
#' plot_correlation(diamonds)
#' plot_correlation(diamonds, maxcat = 5)
#' plot_correlation(diamonds, type = "c")
#' plot_correlation(diamonds, type = "d")

plot_correlation <- function(data, type = c("all", "discrete", "continuous"), maxcat = 20L, title = NULL, ggtheme = theme_gray(), theme_config = list("legend.position" = "bottom", "axis.text.x" = element_text(angle = 90)), ...) {
  ## Declare variable first to pass R CMD check
  Var1 <- Var2 <- value <- NULL
  ## Set data to data.table
  if (!is.data.table(data)) data <- data.table(data)
  ## Split data
  split_data <- split_columns(data)
  ## Match column type and raise appropriate alerts if necessary
  col_type <- match.arg(type)
  if (col_type == "continuous") {
    if (split_data$num_continuous == 0) stop("Not enough continuous features!")
    final_data <- split_data$continuous
  }

  if (col_type == "discrete") {
    if (split_data$num_discrete == 0) stop("No discrete features found!")
    final_data <- split_columns(dummify(split_data$discrete, maxcat = maxcat))$continuous
  }

  if (col_type == "all") {
    if (split_data$num_discrete == 0) {
      final_data <- data
    } else {
      final_data <- split_columns(dummify(data, maxcat = maxcat))$continuous
    }
  }

  ## Calculate correlation and melt into tidy data format
  plot_data <- reshape2::melt(cor(final_data, ...))
  ## Create ggplot object
  plot <- ggplot(plot_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2("Correlation Meter", limits = c(-1, 1), low = "#0571b0", high = "#ca0020", space = "Lab") +
    xlab("Features") + ylab("Features") +
    ggtitle(label = title) +
    ggtheme +
    do.call(theme, theme_config)
  if (ncol(final_data) <= 20) {
    plot <- plot + geom_text(aes(label = round(value, 2)))
  }
  ## Print plot object
  print(plot)
}

CorrelationDiscrete <- function(data, maxcat = 20, title = NULL, ...) {
  .Deprecated("plot_correlation")
  plot_correlation(data = data, type = "discrete", maxcat = maxcat, title = title, ...)
}

CorrelationContinuous <- function(data, title = NULL, ...) {
  .Deprecated("plot_correlation")
  plot_correlation(data = data, type = "continuous", title = title, ...)
}
#---------------------------------------------------#
# Some miscellaneous functions 
#---------------------------------------------------#

see_installed_packages <- function(p) {
  ip <- as.data.frame(installed.packages()[,c(1,3:4)])
  rownames(ip) <- NULL
  ip <- ip[is.na(ip$Priority),1:2,drop=FALSE]
  if (p == TRUE) {
    print(ip, row.names=FALSE)
  }
  return ip
}  

# Function to see the installed packages
seeIp <- fucntion(p = FALSE) {
  see_installed_packages()
}
