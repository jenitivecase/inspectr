col_check <- function(colname, data, fun, output = FALSE, stage = NULL) {
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname, "_check")
  data[,check_name] <- vapply(data[colname], FUN = fun, FUN.VALUE = vector(length = nrow(data[colname])))

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    if(output == FALSE){
      return(temp)
    } else if (output == TRUE){
      if(is.null(stage)){
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      } else {
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      }
    }
  }
}

two_col_check <- function(colname1, colname2, data, fun, output = FALSE, stage = NULL){
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    if(output == FALSE){
      return(temp)
    } else if (output == TRUE){
      if(is.null(stage)){
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      } else {
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      }
    }
  }
}

three_col_check <- function(colname1, colname2, colname3, data = GRF, fun, output = FALSE, stage = NULL){
  if(!dir.exists(stage)){ dir.create(stage) }
  check_name <- paste0(colname1, "_check")
  data[,check_name] <- mapply(FUN = fun, data[colname1], data[colname2], data[colname3])

  if(sum(data[,check_name]) != nrow(data)){
    temp <- data[which(data[,check_name] != TRUE),]
    if(output == FALSE){
      return(temp)
    } else if (output == TRUE){
      if(is.null(stage)){
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      } else {
        write.xlsx(temp, paste0("./", stage, "/", check_name, "_errors_", date, ".xlsx"))
      }
    }
  }
}

