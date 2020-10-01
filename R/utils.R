check_return <-
  function(errors, output, loc, check_name = check_name) {
    date <- format.Date(Sys.Date(), "%Y%m%d")
    if (output == FALSE) {
      return(errors)
    } else if (output == TRUE & is.null(loc)) {
      openxlsx::write.xlsx(errors, paste0(check_name, "_errors_",
                                          date, ".xlsx"))
    } else if (output == TRUE & !is.null(loc)) {
      openxlsx::write.xlsx(errors,
                           paste0(loc, "/", check_name, "_errors_",
                                  date, ".xlsx"))
    }
  }
