#' Diskriminantna analiza
#'
#' Ova funkcija izvodi diskriminantnu analizu s interaktivnim izborom kvalitativne (grupne) i kvantitativnih varijabli
#'
#' @param df data.frame koji sadrži podatke za analizu
#' @return Lista koja sadrži:
#'   \item{df_res}{Rezultati kanoničke diskriminantne analize}
#'   \item{structure}{Struktura diskriminantnih varijabli}
#'   \item{centroids}{Centroidi grupa s frekvencijama}
#'   \item{freq_table}{Tablica frekvencija LDA klasifikacije}
#'   \item{prop_table}{Tablica postotaka LDA klasifikacije (po redcima)}
#' @examples
#' \dontrun{
#' rezultati <- pokreni_diskriminant(df)
#' print(rezultati$df_res)
#' print(rezultati$structure)
#' print(rezultati$centroids)
#' print(rezultati$freq_table)
#' print(rezultati$prop_table)
#' }
#' @export
discra_analysis<- function(df) {

  # --- Provjera i instalacija paketa ---
  required_pkgs <- c("candisc", "MASS", "gmodels")
  for(pkg in required_pkgs) {
    if(!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  # --- 1. Odabir varijabli ---
  cat_vars <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
  cvar <- select.list(cat_vars, multiple = FALSE, title = "Odaberi kvalitativnu varijablu:")
  df[[cvar]] <- as.factor(df[[cvar]])
  
  num_vars <- names(df)[sapply(df, is.numeric)]
  nvars <- select.list(num_vars, multiple = TRUE, title = "Odaberi kvantitativne varijable:")
  
  # --- 2. Kreiranje formula ---
  formula_lm <- as.formula(paste("cbind(", paste(nvars, collapse = ", "), ") ~", cvar))
  formula_lda <- as.formula(paste(cvar, "~", paste(nvars, collapse = " + ")))
  
  # --- 3. Diskriminantna analiza ---
  var_disc <- lm(formula_lm, data = df)
  cd_candisc <- candisc(var_disc, data = df)
  w <- Wilks(cd_candisc)
  
  k <- cd_candisc$ndim
  m <- length(nvars)
  fnames <- paste0("DF", 1:k)
  
  Eigenval <- round(cd_candisc$eigenvalues, digits = 3)
  Canonical_corr <- round(sqrt((cd_candisc$canrsq)), digits = 3)
  Wilks_lam <- round(w$`LR test stat`, digits = 3)
  F_aprox <- round(w$`approx F`, digits = 3)
  df1 <- w$numDF
  df2 <- w$denDF
  p_value <- round(w$`Pr(> F)`, digits = 3)
  
  df_res <- cbind(fnames, Eigenval, Canonical_corr, Wilks_lam, F_aprox, df1, df2, p_value)
  df_res <- data.frame(df_res)
  colnames(df_res) <- c("DF", "Eigenvalue", "Canonical R", "Wilks' Lambda","aprox. F", "df1", "df2", "p-level")
  df_res <- df_res[1:k,]
  
  dfnames <- paste0("DF", 1:m)
  
  # --- 4. Struktura i centroidi ---
  sdf <- round(cd_candisc$structure, digits = 3)
  sdf <- data.frame(sdf)
  colnames(sdf) <- dfnames[1:k]
  
  f <- table(cd_candisc$factors)
  cg <- round(cd_candisc$means, digits = 3)
  cg <- data.frame(cg)
  cg <- cbind.data.frame(f, cg)
  colnames(cg) <- c(cvar, "FREQ.", dfnames[1:k])
  
  # --- 5. LDA ---
  lda_model <- lda(formula_lda, data = df)
  predict_class <- predict(lda_model)$class
  
  ctf <- CrossTable(predict_class, df[[cvar]])
  ctf <- as.data.frame.matrix(t(ctf$t))
  ctf$Total = rowSums(ctf[,])
  
  ctfp <- round(CrossTable(predict_class, df[[cvar]])$prop.col * 100, 2)
  ctfp <- as.data.frame.matrix(t(ctfp))
  ctfp$Total = rowSums(ctfp[,])
  
  # --- 6. Rezultati ---
  list(
    df_res = df_res,
    structure = sdf,
    centroids = cg,
    freq_table = ctf,
    prop_table = ctfp
  )
}
