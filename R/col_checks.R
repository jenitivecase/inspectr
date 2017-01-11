check_return <- function(errors, output, stage){
  date <- format.Date(Sys.Date(), "%Y%m%d")
  if(output == FALSE){
    return(errors)
  } else if (output == TRUE){
    if(is.null(stage)){
      write.xlsx(errors, paste0(check_name, "_errors_",
                              date, ".xlsx"))
    } else {
      write.xlsx(errors, paste0(stage, "_", check_name, "_errors_",
                              date, ".xlsx"))
    }
  }
}

#' Check a single column for data fidelity.
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
#'
#' @return col_check(output = FALSE) returns a dataframe containing only records
#'   that failed the specified check.
#' @return col_check(output = TRUE) invisibly prints an excel output file
#'   containing only records that failed the specified check.
#' @examples
#' col_check(colname = "ID_var", data = data, fun = numeric_check,
#' output = TRUE, stage = "1-Reasonableness")
#'
#' col_check(colname = "Name", data = data, fun = character_check,
#' output = FALSE)


col_check <- function(colname, data, fun, output = FALSE, stage = NULL) {
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname, "_check")
  data[,check_name] <- vapply(data[colname], FUN = fun, FUN.VALUE =
                                vector(length = nrow(data[colname])))

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    check_return(errors = temp, output = output, stage = stage)
  }
}

two_col_check <- function(colname1, colname2, data, fun, output = FALSE,
                          stage = NULL){
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    check_return(errors = temp, output = output, stage = stage)
  }
}

three_col_check <- function(colname1, colname2, colname3, data = GRF, fun,
                            output = FALSE, stage = NULL){
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2],
                              data[colname3])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    check_return(errors = temp, output = output, stage = stage)
  }
}

