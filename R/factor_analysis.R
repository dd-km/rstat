#' Faktorska analiza s interaktivnim odabirom varijabli
#'
#' Funkcija provodi **eksploratornu faktorsku analizu (EFA)** na zadanoj tablici podataka,
#' omogućuje interaktivni odabir numeričkih varijabli, broja faktora i tipa rotacije,
#' te vraća matrice faktorskih opterećenja, strukture, korelacijske matrice, faktorske scoreve s imenima entiteta,
#' eigenvalue, kao i interaktivne grafove GK i PB kriterija te faktorskih opterećenja.
#'
#' @param df Data frame koji sadrži podatke. Prvi stupac treba sadržavati imena entiteta.
#'
#' @return Lista koja sadrži:
#' \describe{
#'   \item{Loadings}{Pattern matrica faktorskih opterećenja (s kompleksnošću).}
#'   \item{Structure}{Structure matrica faktora s komunalitetima.}
#'   \item{Correlations}{Korelacijska matrica faktorskih scoreva.}
#'   \item{Scores}{Faktorski scorevi za svaki entitet (prvi stupac df koristi se za imena entiteta).}
#'   \item{Eigenvalues}{Data frame s eigenvalue, kumulativnim eigenvalue, postotcima i kumulativnim postotcima.}
#'   \item{GK}{Broj faktora prema Guttman-Kaiser kriteriju (Eigenvalue ≥ 1).}
#'   \item{PB}{Broj faktora prema Stalec-Momirovic kriteriju.}
#'   \item{SSMC}{Zbroj kvadrata multiple korelacije.}
#'   \item{FigGK}{Interaktivni Plotly graf eigenvalue s GK kriterijem.}
#'   \item{FigPB}{Interaktivni Plotly graf kumulativnih eigenvalue s PB kriterijem.}
#'   \item{FigFac}{Stacked bar graf faktorskih opterećenja po faktorima.}
#'   \item{FigVar}{Stacked bar graf strukture po varijablama.}
#' }
#'
#' @details
#' Funkcija automatski provjerava potrebne pakete (`rstat`, `plotly`, `psych`, `reshape2`) i instalira ih ako nedostaju.  
#' Redovi s nedostajućim podacima u odabranim numeričkim varijablama se uklanjaju.  
#' Faktorski scorevi uključuju imena entiteta iz prvog stupca ulaznog data frame-a.
#'
#' @examples
#' \dontrun{
#' # Učitavanje podataka
#' df <- read.csv("mojapodatci.csv")
#' 
#' # Pokretanje faktorske analize
#' rezultati <- factor_analysis(df)
#' 
#' # Pregled faktorskih scoreva s imenima entiteta
#' head(rezultati$Scores)
#' 
#' # Prikaz interaktivnog grafa faktorskih opterećenja
#' rezultati$FigFac
#' }
#'
#' @export
factor_analysis <- function(df) {
  # -------------------------------
  # Provjera i učitavanje paketa
  # -------------------------------
  if (!require("rstat")) install.packages("rstat"); library(rstat)
  if (!require("plotly")) install.packages("plotly"); library(plotly)
  if (!require("psych")) install.packages("psych"); library(psych)
  if (!require("reshape2")) install.packages("reshape2"); library(reshape2)
  
  # Isključi upozorenja
  old_warn <- getOption("warn")
  options(warn = -1)
  
  # -------------------------------
  # Odabir numeričkih varijabli
  # -------------------------------
  num_vars <- names(df)[sapply(df, is.numeric)]
  nvars <- select.list(num_vars, multiple = TRUE, title = "Odaberite varijable:")
  B <- na.omit(df[, nvars])
  
  # -------------------------------
  # Faktorska analiza (inicijalna)
  # -------------------------------
  fit <- principal(B)
  m <- ncol(B)
  Eigenvalue <- fit$values
  Cum.Eign <- cumsum(Eigenvalue)
  Percentage <- Eigenvalue / sum(Eigenvalue) * 100
  Cum.Per <- cumsum(Percentage)
  Lamda <- data.frame(Eigenvalue, Cum.Eign, Percentage, Cum.Per)
  rownames(Lamda) <- 1:m
  
  R <- cor(B)
  SMC <- tryCatch({
    1 - 1 / diag(solve(as.matrix(R), tol = 1e-10))
  }, error = function(e) rep(NA, m))
  ssmc <- sum(SMC, na.rm = TRUE)
  
  gk <- sum(Eigenvalue >= 1)
  pb <- sum(Cum.Eign < ssmc)
  
  # -------------------------------
  # Plotly grafovi: GK i PB kriteriji
  # -------------------------------
  evdf <- data.frame(Component = 1:m, Eigenvalue, CumEigen = Cum.Eign)
  
  fig_gk <- plot_ly(evdf, x = ~Component, y = ~Eigenvalue, type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8), line = list(width = 2), name = "Eigenvalue") %>%
    add_trace(y = rep(1, m), mode = "lines", line = list(dash = "dash", color = "red", width = 1),
              name = "GK-Criterion") %>%
    layout(title = "GK-Criterion", xaxis = list(title = "Component"), yaxis = list(title = "Eigenvalue"))
  
  fig_pb <- plot_ly(evdf, x = ~Component, y = ~CumEigen, type = 'scatter', mode = 'lines+markers',
                    marker = list(size = 8), line = list(width = 2), name = "Cum. Eigenvalue") %>%
    add_trace(y = rep(ssmc, m), mode = "lines", line = list(dash = "dash", color = "red", width = 1),
              name = "PB-Criterion") %>%
    layout(title = "PB-Criterion", xaxis = list(title = "Component", dtick = 1),
           yaxis = list(title = "Cum. Eigenvalue"))
  
  # -------------------------------
  # Broj faktora i rotacija
  # -------------------------------
  choices_factors <- as.character(1:m)
  sel_factors <- select.list(choices_factors, multiple = FALSE, title = "Broj faktora:")
  num_factors <- ifelse(sel_factors == "", gk, as.integer(sel_factors))
  
  rotation_type <- select.list(c("none", "varimax", "oblimin", "promax"),
                               title = "Tip rotacije:")
  
  message("Korišten broj faktora: ", num_factors, " | Rotacija: ", rotation_type)
  
  fit <- principal(B, nfactors = num_factors, rotate = rotation_type)
  
  # -------------------------------
  # Matrice i rezultati
  # -------------------------------
  Amat <- as.data.frame(unclass(fit$loadings))
  Amat <- cbind(Amat, Complexity = fit$complexity)
  colnames(Amat)[1:num_factors] <- paste0("F", 1:num_factors)
  
  Fmat <- fit$Structure
  colnames(Fmat)[1:num_factors] <- paste0("F", 1:num_factors)
  Fmat <- cbind(Fmat, Communality = fit$communality)
  
  Mmat <- cor(fit$scores)
  colnames(Mmat)[1:num_factors] <- paste0("F", 1:num_factors)
  rownames(Mmat)[1:num_factors] <- paste0("F", 1:num_factors)
  
  # -------------------------------
  # Faktorski rezultati + entiteti iz prvog stupca df
  # -------------------------------
  Fscores <- as.data.frame(fit$scores)
  colnames(Fscores)[1:num_factors] <- paste0("F", 1:num_factors)
  
  valid_rows <- complete.cases(df[, nvars])
  Fscores$Entitet <- df[valid_rows, 1, drop = TRUE]
  Fscores <- Fscores[, c("Entitet", paste0("F", 1:num_factors))]
  
  # -------------------------------
  # Grafovi faktorskih opterećenja
  # -------------------------------
  FF <- Fmat[, 1:num_factors]^2
  F_long <- melt(as.matrix(FF))
  colnames(F_long) <- c("Variable", "Factor", "Loading")
  
  fig_F <- plot_ly(F_long, x = ~Loading, y = ~Factor, color = ~Variable, type = "bar") %>%
    layout(barmode = "stack", title = "Stacked Loadings per Factor",
           xaxis = list(title = "Loading"), yaxis = list(title = "Factor"))
  
  fig_V <- plot_ly(F_long, x = ~Loading, y = ~Variable, color = ~Factor, type = "bar") %>%
    layout(barmode = "stack", title = "Stacked Structure per Variable",
           xaxis = list(title = "Loading"), yaxis = list(title = "Variable"))
  
  # Vrati upozorenja
  options(warn = old_warn)
  
  # Povrat rezultata
  list(
    Loadings = Amat,
    Structure = Fmat,
    Correlations = Mmat,
    Scores = Fscores,
    Eigenvalues = Lamda,
    GK = gk,
    PB = pb,
    SSMC = ssmc,
    FigGK = fig_gk,
    FigPB = fig_pb,
    FigFac = fig_F,
    FigVar = fig_V
  )
}
