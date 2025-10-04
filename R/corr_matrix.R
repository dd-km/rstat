
#' Izračunavanje korelacijske matrice sa p-vrijednostima
#'
#' Funkcija računa korelaciju između numeričkih varijabli u data frame-u koristeći
#' paket \code{Hmisc}. Vraća listu koja sadrži matricu korelacija i matricu p-vrijednosti.
#' Korisnik interaktivno bira koje varijable uključiti i koju vrstu korelacije koristiti.
#'
#' @param df Data frame koji sadrži numeričke kolone.
#'
#' @return Lista s tri elementa:
#' \describe{
#'   \item{r}{Matrica koeficijenata korelacije.}
#'   \item{p}{Matrica p-vrijednosti.}
#'   \item{type}{Vrsta korištene korelacije: "pearson", "spearman" ili "kendall".}
#' }
#'
#' @details
#' Funkcija koristi \code{Hmisc::rcorr} za izračun korelacije.  
#' Podržane vrste korelacije: \code{"pearson"}, \code{"spearman"}, \code{"kendall"}.  
#' Ako data frame ne sadrži barem dvije numeričke kolone ili korisnik ne odabere barem dvije varijable, funkcija zaustavlja izvršenje.
#'
#' @examples
#' \dontrun{
#' # Primjer sa ugrađenim datasetom mtcars
#' result <- corr_matrix(mtcars)
#' result$r   # matrica korelacija
#' result$p   # matrica p-vrijednosti
#' result$type # vrsta korelacije
#' }
#'
#' @importFrom Hmisc rcorr
#' @export
corr_matrix <- function(df) {
  if(!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")
  library(Hmisc)

  num_vars <- names(df)[sapply(df, function(x) is.numeric(x) || is.integer(x))]
  if(length(num_vars) < 2) stop("U data frame-u nema dovoljno numeričkih varijabli")
  svars <- select.list(num_vars,
                       multiple = TRUE,
                       title = "Izaberi numeričke varijable")
  if(length(svars) < 2) stop("Morate izabrati najmanje dvije varijable!")
  vrsta <- select.list(c("pearson", "spearman"),
                       multiple = FALSE,
                       title = "Izaberi koeficijent korelacije")
  x <- df[, svars, drop = FALSE]
  res <- Hmisc::rcorr(as.matrix(x), type = vrsta)

  return(list(r = round(res$r, 3), p = round(res$P, 4), type = vrsta ))
}
