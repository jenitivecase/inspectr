numeric_check <- function(x){
  !is.na(suppressWarnings(as.numeric(x)))
}

character_blanks_check <- function(x){
  !(x == "" | is.na(suppressWarnings(as.character(x))))
}

character_check <- function(x){
  is.na(x) | !is.na(suppressWarnings(as.character(x)))
}

date_check <- function(x, begin, end){
  is.na(x) |
    as.Date(x, format="%m/%d/%Y") > as.Date(begin, format="%m/%d/%Y") &
    as.Date(x, format="%m/%d/%Y") < as.Date(end, format="%m/%d/%Y")
}

subj_check <- function(col1, col2){
  col1 %in% c("ELA", "M", "Sci", "SS") | (is.na(col1) & col2 == 9)
}

less_than <- function(col1, col2){
  col1 < col2
}

less_than_equalto <- function(col1, col2){
  col1 <= col2
}

greater_than <- function(col1, col2){
  col1 > col2
}

greater_than_equalto <- function(col1, col2){
  col1 >= col2
}

band_check <- function(col1, col2, col3){
  col1 %in% c("Band 1", "Band 2", "Band 3", "Foundational") |
    (is.na(col1) & col3 == "SS" & col2 %in% c(1:4, 9)) |
    (is.na(col1) & is.na(col3) & col2 == 9)
}
