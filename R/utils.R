check_return <- function(errors, output, stage, check_name = check_name){
  date <- format.Date(Sys.Date(), "%Y%m%d")
  if(output == FALSE){
    return(errors)
  } else if (output == TRUE){
    if(is.null(stage)){
      openxlsx::write.xlsx(errors, paste0(check_name, "_errors_",
                                          date, ".xlsx"))
    } else {
      openxlsx::write.xlsx(errors, paste0(stage, "_", check_name, "_errors_",
                                          date, ".xlsx"))
    }
  }
}
