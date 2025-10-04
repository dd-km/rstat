#' Interaktivno učitavanje CSV, Excel ili ODS datoteka iz sheet = 1
#'
#' @return Data frame s učitanim podacima.
#' @examples
#' \dontrun{
#' df <- import_data2()
#' }
#' @export
import_data2 <- function() {
  if(!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if(!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if(!requireNamespace("readODS", quietly = TRUE)) install.packages("readODS")
  library(readr)
  library(readxl)
  library(readODS)
  
  options_type <- c("Excel (.xls/.xlsx)", "ODS (.ods)", "CSV(.csv)")
  file_type <- select.list(options_type, title = "Odaberite tip datoteke za uvoz:")

  select_csv_sep <- function() {
    sep_options <- c("Tocka-zarez (;)" = ";" , "Zarez (,)" = "," )
    sep <- select.list(names(sep_options), title = "Odaberite separator:")
    return(sep_options[sep])
  }
  df <- switch(file_type,
               "Excel (.xls/.xlsx)" = {
                 path <- file.choose()
                 read_excel(path, sheet = 1)
               },
               "ODS (.ods)" = {
                 path <- file.choose()
                 read_ods(path, sheet = 1)
               },
                "CSV(.csv)" = {
                 path <- file.choose()
                 sep <- select_csv_sep()
                 read_csv2_or_csv <- if(sep == ",") read_csv else read_csv2
                 read_csv2_or_csv(path)
               }
  )
  return(df)
}