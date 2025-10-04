#' Transformacije podataka
#'
#' Funkcija transformaciju numeričkih varijabli u Z-value, T-value ili L-value. 
#' Omogućuje odabir normalno i obrnuto skaliranih varijabli. 
#'
#' @param df DataFrame koji sadrži numeričke varijable. 
#'
#' @return Lista s dva elementa:
#' \item{table}{DataFrame s transformiranim vrijednostima}
#' \item{method}{Odabrana metoda transformacije}
#'
#' @details
#' Normalne varijable se standardiziraju (Z-score), a obrnuto skalirane varijable
#' se množe s -1. T-value transformacija: T = Z * 10 + 50. L-value transformacija: L = Z * 0.83 + 3
#'
#' @examples
#' # result <- trans_value(df)
#' 
#' @export
trans_value <- function(df){
  num_vars <- names(df)[sapply(df, is.numeric)]
  
  nvars <- select.list(num_vars, multiple = TRUE,
                       title = "Odaberi normalno skalirane varijable:")
  ivars <- select.list(num_vars, multiple = TRUE,
                       title = "Odaberi obrnuto skalirane varijable:")
  
  entiteti <- as.character(df[[1]]) 
  options <- c("Z-value", "T-value", "L-value")
  choice <- utils::select.list(options, title = "Odaberi način transformacije")
  
  if ((length(nvars) + length(ivars)) == 0 || choice == "") {
    stop("Niste odabrali način transformacije ili dataframe nema numeričkih varijabli.")
  }
  
  Zn <- if (length(nvars) > 0) scale(df[nvars]) else NULL
  Zi <- if (length(ivars) > 0) scale(df[ivars]) * -1 else NULL
  
  Z <- cbind(Zn, Zi)
  rownames(Z) <- entiteti
  
  res <- switch(choice,
                "Z-value" = round(Z, 2),
                "T-value" = round(Z*10+50, 2),
                "L-value" = round(Z*0.83+3, 2)
  )
  
  return(list(
    table = as.data.frame(res),
    method = choice
    )
  )
  
}
