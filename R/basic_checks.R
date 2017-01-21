#' Check whether all values are numeric.
#'
#' To be used with \code{\link{col_check}}.
#' @param x the data that enters the function (the column specified in
#'   \code{\link{col_check}})
#'
#' @export

numeric_check <- function(x){
  !is.na(suppressWarnings(as.numeric(x)))
}



#' Check whether all values are either character strings. Blanks ("") and NA
#' values are not permitted.
#'
#' To be used with \code{\link{col_check}}.
#' @param x the data that enters the function (the column specified in
#'   \code{\link{col_check}})
#'
#' @export

character_blanks_check <- function(x){
  !(x == "" | is.na(suppressWarnings(as.character(x))) | is.na(x))
}



#' Check whether all values are character strings or blanks (""). NA values are
#' not permitted.
#'
#' To be used with \code{\link{col_check}}.
#' @param x the data that enters the function (the column specified in
#'   \code{\link{col_check}})
#'
#' @export

character_check <- function(x){
  !(is.na(x) | is.na(suppressWarnings(as.character(x))))
}



#' Check whether all values fall within a date range. NA values are also
#' accepted.
#'
#' To be used with \code{\link{col_check}}.
#'
#' @param x the data that enters the function (the column specified in
#'   \code{\link{col_check}})
#' @param begin the beginning acceptable date.
#' @param end the last acceptable date.
#' @param format a character string specifying the date format.
#'
#' @export

date_check <- function(x, begin, end, format = "%m/%d/%Y", ...){
  is.na(x) |
    as.Date(x, format = format) >= as.Date(begin, format = format) &
    as.Date(x, format = format) <= as.Date(end, format = format)
}



#' Check whether all values in the column fall within a set of user-defined
#' values.
#'
#' To be used with \code{\link{col_check}}.
#'
#' @param x the data that enters the function (the column specified in
#'   \code{\link{col_check}})
#' @param values contains a value or vector of values that contain the
#'   acceptable value(s) that may be found in the column. These values may be
#'   any data type - character strings, numeric values, etc.
#'
#' @export

val_check <- function(x, values, ...){
  x %in% values
}



# #' Check whether values in column one fall within a set of user-defined values and
# #'
# #' To be used with \code{\link{two_col_check}}.
# #'
# #' @param x the data that enters the function (the column specified in
# #'   \code{\link{col_check}})
# #' @param begin the beginning acceptable date.
# #' @param end the last acceptable date.
# #' @param format a character string specifying the date format.
#
# val_check_twocol <- function(col1, col2){
#   col1 %in% c("ELA", "M", "Sci", "SS") | (is.na(col1) & col2 == 9)
# }



#' Check whether values in column one are less than their corresponding values
#' in the second column.
#'
#' To be used with \code{\link{two_col_check}}.
#'
#' @param col1 the first column of data, specified in \code{\link{col_check}})
#' @param col2 the second column of data, specified in \code{\link{col_check}})
#'
#' @export

less_than <- function(col1, col2){
  col1 < col2
}


#' Check whether values in column one are less than or equal to their
#' corresponding values in the second column.
#'
#' To be used with \code{\link{two_col_check}}.
#'
#' @param col1 the first column of data, specified in \code{\link{col_check}})
#' @param col2 the second column of data, specified in \code{\link{col_check}})
#'
#' @export

less_than_equalto <- function(col1, col2){
  col1 <= col2
}



#' Check whether values in column one are greater than their corresponding values
#' in the second column.
#'
#' To be used with \code{\link{two_col_check}}.
#'
#' @param col1 the first column of data, specified in \code{\link{col_check}})
#' @param col2 the second column of data, specified in \code{\link{col_check}})
#'
#' @export

greater_than <- function(col1, col2){
  col1 > col2
}



#' Check whether values in column one are greater than or equal to their
#' corresponding values in the second column.
#'
#' To be used with \code{\link{two_col_check}}.
#'
#' @param col1 the first column of data, specified in \code{\link{col_check}})
#' @param col2 the second column of data, specified in \code{\link{col_check}})
#'
#' @export

greater_than_equalto <- function(col1, col2){
  col1 >= col2
}


band_check <- function(col1, col2, col3){
  col1 %in% c("Band 1", "Band 2", "Band 3", "Foundational") |
    (is.na(col1) & col3 == "SS" & col2 %in% c(1:4, 9)) |
    (is.na(col1) & is.na(col3) & col2 == 9)
}
