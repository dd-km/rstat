#' Kanonička korelacijska analiza (CCA)
#'
#' Funkcija izvršava kanoničku korelacijsku analizu između dva skupa numeričkih varijabli.
#' Vraća kanoničke korelacije, strukturu kanoničkih faktora i kanoničke faktore.
#'
#' @param df Data frame koji sadrži numeričke varijable za analizu.
#' @param vars1 Vektor imena varijabli koje pripadaju prvom skupu. Ako NULL i interactive = TRUE, varijable se odabiru interaktivno.
#' @param vars2 Vektor imena varijabli koje pripadaju drugom skupu. Ako NULL i interactive = TRUE, varijable se odabiru interaktivno.
#' @param interactive Logička vrijednost; ako TRUE i vars1/vars2 nisu definirani, otvara se interaktivni odabir varijabli.
#'
#' @return Lista sa sljedećim elementima:
#' \describe{
#'   \item{RC}{Data frame kanoničkih korelacija i pripadajućih statistika (chi-square test, df, p-vrijednost).}
#'   \item{F1}{Data frame strukturalnih korelacija za prvi skup varijabli.}
#'   \item{F2}{Data frame strukturalnih korelacija za drugi skup varijabli.}
#'   \item{CF1}{Data frame kanoničkih varijabli za prvi skup varijabli.}
#'   \item{CF2}{Data frame kanoničkih varijabli za drugi skup varijabli.}
#' }
#'
#' @details
#' Funkcija koristi paket \code{yacca} za izvođenje kanoničke korelacijske analize.
#' Funkcija automatski uklanja retke s \code{NA} vrijednostima i poravnava redove oba skupa.
#' Redoslijed skupova se automatski zamjenjuje ako drugi skup ima više varijabli od prvog.
#'
#' @examples
#' # Interaktivni odabir varijabli
#' # result <- can_analysis(df)
#'
#' # Fiksni odabir varijabli
#' # result <- can_analysis(df, vars1 = c("var1", "var2"), vars2 = c("var3", "var4"), interactive = FALSE)
#'
#' @export
can_analysis <- function(df, vars1 = NULL, vars2 = NULL, interactive = TRUE) {
  library(yacca)
  
  # === Odabir varijabli ===
  num_vars <- names(df)[sapply(df, is.numeric)]
  
  if (interactive && (is.null(vars1) || is.null(vars2))) {
    vars1 <- select.list(num_vars, multiple = TRUE, title = "Odaberite 1. skup varijabli:")
    vars2 <- select.list(num_vars, multiple = TRUE, title = "Odaberite 2. skup varijabli:")
  } else {
    if (is.null(vars1) || is.null(vars2)) stop("Morate definirati vars1 i vars2 ili omogućiti interactive = TRUE")
  }
  
  # === Provjera odabira ===
  if (length(vars1) == 0 | length(vars2) == 0) {
    stop("Morate odabrati barem jednu varijablu u svakom skupu.")
  }
  
  # === Uklanjanje redova s NA vrijednostima ===
  df <- na.omit(df[, unique(c(vars1, vars2))])
  B1 <- df[, vars1]
  B2 <- df[, vars2]
  
  # === Određivanje swap logike ===
  m1 <- ncol(B1)
  m2 <- ncol(B2)
  swap <- m1 < m2
  
  # === Kanonička analiza ===
  if (swap) {
    rcca <- cca(B2, B1, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)
  } else {
    rcca <- cca(B1, B2, xscale = TRUE, yscale = TRUE, standardize.scores = TRUE)
  }
  
  # === Izračuni ===
  kk <- min(m1, m2)
  fnames <- paste0("CF", 1:kk)
  p <- pchisq(rcca$chisq, rcca$df, lower.tail = FALSE)
  
  # === Rezultati korelacija ===
  RC <- data.frame(
    CF = fnames,
    Rc = round(rcca$corr, 3),
    Chi_sq = round(rcca$chisq, 3),
    df = rcca$df,
    p_value = round(p, 3)
  )
  
  # === Strukturalne korelacije i kanoničke varijable ===
  F1 <- if (swap) round(rcca$ystructcorr, 3) else round(rcca$xstructcorr, 3)
  F2 <- if (swap) round(rcca$xstructcorr, 3) else round(rcca$ystructcorr, 3)
  CF1 <- if (swap) round(rcca$canvary, 3) else round(rcca$canvarx, 3)
  CF2 <- if (swap) round(rcca$canvarx, 3) else round(rcca$canvary, 3)
  
  # Pretvaranje u data frame
  F1 <- as.data.frame(F1)
  F2 <- as.data.frame(F2)
  CF1 <- as.data.frame(CF1)
  CF2 <- as.data.frame(CF2)
  
  # Dodavanje imena kolona
  if (ncol(F1) == kk) colnames(F1) <- fnames
  if (ncol(F2) == kk) colnames(F2) <- fnames
  if (ncol(CF1) == kk) colnames(CF1) <- fnames
  if (ncol(CF2) == kk) colnames(CF2) <- fnames
  
  # === Povrat vrijednosti ===
  return(list(
    RC = RC,
    F1 = F1,
    F2 = F2,
    CF1 = CF1,
    CF2 = CF2
  ))
}
