#' Check a single column for data fidelity.
#'
#' To be used with user-defined functions or with built-in check functions such
#' as \code{\link{numeric_check}}, \code{\link{character_check}},
#' \code{\link{character_blanks_check}}, \code{\link{date_check}}, and
#' \code{\link{val_check}}.
#'
#' @param colname character string specifying the name of the column within your
#'   dataframe.
#' @param data the dataframe containing the data.
#' @param fun the check function you'd like to apply to the data.
#' @param output logical. If FALSE, the function returns a dataframe containing
#'   only records that failed the specified check. If TRUE, invisibly prints an
#'   excel output file containing only the records that failed the specified
#'   check.
#' @param stage An optional character string that can be used to specify the
#'   stage of the checking process in which the check is occurring. Only useful
#'   if output = TRUE. If a value is specified, a that value is prefixed to the
#'   output file; if no value is given, no stage prefix is attached.
#' @param ... arguments to be passed through to the function specified in
#'   \code{fun}
#'
#' @return col_check(output = FALSE) returns a dataframe containing only records
#'   that failed the specified check.
#' @return col_check(output = TRUE) invisibly prints an excel output file
#'   containing only records that failed the specified check.
#'
#' @examples
#' col_check(colname = "ID_var", data = dataset, fun = numeric_check,
#' output = TRUE, stage = "1-Reasonableness")
#'
#' col_check(colname = "FName", data = dataset, fun = character_check,
#' output = FALSE)
#'
#' @export

col_check <- function(colname, data, fun, output = FALSE, stage = NULL, ...) {
  check_name <- paste0(colname, "_check")
  data[,check_name] <- apply(data[colname], 1, FUN = fun, ...)

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    temp <- temp[, !names(temp) == check_name, drop = FALSE]
    check_return(errors = temp, output = output, stage = stage, check_name =
                   check_name)
  }
}


#' Check a column for data fidelity using criteria related to a second column.
#'
#' To be used with user-defined functions or with built-in check functions such
#' as \code{\link{less_than}}, \code{\link{less_than_equalto}},
#' \code{\link{greater_than}}, and \code{\link{greater_than_equalto}}.
#'
#' @param colname1 character string specifying the name of the column within
#'   your dataframe that will be the subject of the checks.
#' @param colname2 character string specifying the name of a second column to be
#'   used in the check criteria.
#' @param data the dataframe containing the data.
#' @param fun the check function you'd like to apply to the data.
#' @param output logical. If FALSE, the function returns a dataframe containing
#'   only records that failed the specified check. If TRUE, invisibly prints an
#'   excel output file containing only the records that failed the specified
#'   check.
#' @param stage An optional character string that can be used to specify the
#'   stage of the checking process in which the check is occurring. Only useful
#'   if output = TRUE. If a value is specified, a that value is prefixed to the
#'   output file; if no value is given, no stage prefix is attached.
#' @param ... arguments to be passed through to the function specified in
#'   \code{fun}
#'
#' @return col_check(output = FALSE) returns a dataframe containing only records
#'   that failed the specified check.
#' @return col_check(output = TRUE) invisibly prints an excel output file
#'   containing only records that failed the specified check.
#'
#' @examples
#' two_col_check("Var1", "Var2", dataset, less_than_equalto, output = FALSE)
#'
#' two_col_check("Var2", "Var1", dataset, greater_than, output = TRUE,
#'    stage = "1-Reasonableness")
#'
#' @export

two_col_check <- function(colname1, colname2, data, fun, output = FALSE,
                          stage = NULL, ...){
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    temp <- temp[, !names(temp) == check_name, drop = FALSE]
    check_return(errors = temp, output = output, stage = stage, check_name =
                   check_name)
  }
}



#' Check a column for data fidelity using criteria related to two additional
#' columns.
#'
#' To be used with user-defined functions. The example below illustrates an
#' example of a user-defined function that could be defined to use with this
#' function.
#'
#' @param colname1 character string specifying the name of the column within
#'   your dataframe that will be the subject of the checks.
#' @param colname2 character string specifying the name of a second column to be
#'   used in the check criteria.
#' @param colname3 character string specifying the name of a third column to be
#'   used in the check criteria.
#' @param data the dataframe containing the data.
#' @param fun the check function you'd like to apply to the data.
#' @param output logical. If FALSE, the function returns a dataframe containing
#'   only records that failed the specified check. If TRUE, invisibly prints an
#'   excel output file containing only the records that failed the specified
#'   check.
#' @param stage An optional character string that can be used to specify the
#'   stage of the checking process in which the check is occurring. Only useful
#'   if output = TRUE. If a value is specified, a that value is prefixed to the
#'   output file; if no value is given, no stage prefix is attached.
#' @param ... arguments to be passed through to the function specified in
#'   \code{fun}
#'
#' @return col_check(output = FALSE) returns a dataframe containing only records
#'   that failed the specified check.
#' @return col_check(output = TRUE) invisibly prints an excel output file
#'   containing only records that failed the specified check.
#'
#' @examples
#' three_col_check(colname1 = "Perf_Lvl", colname2 = "Var1", colname3 = "Var2",
#' data = dataset, fun = function(col1, col2, col3){
#'    col1 %in% c("Basic", "Intermediate", "Advanced") |
#'    (is.na(col1) & (col3 %% 2 ==0) & (col2 %% 2 ==1 ))
#'    })
#' @export

three_col_check <- function(colname1, colname2, colname3, data = data, fun,
                            output = FALSE, stage = NULL, ...){
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2],
                              data[colname3])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    temp <- temp[, !names(temp) == check_name, drop = FALSE]
    check_return(errors = temp, output = output, stage = stage, check_name =
                   check_name)
  }
}

