#' Višestruka regresijska analiza
#'
#' Funkcija izvodi višestruku regresijsku analizu na tablici podataka.
#'
#' @param df Data frame koji sadrži numeričke varijable za regresijsku analizu.
#' @return Lista s elementima:
#' \item{results_table}{Data frame koji sadrži regresijske koeficijente, SE, Beta, Part_R, R, P, Tolerance, t-vrijednosti i p-vrijednosti.}
#' \item{plot}{Plotly bar graf standardiziranih koeficijenata.}
#' \item{R_squared}{Koeficijent determinacije (R^2).}
#' \item{SEE}{Standardnu pogrešku prognoze.}
#' \item{F_value}{F statistika modela.}
#' \item{F_p_value}{p-vrijednost F statistike.}
#' \item{Durbin_Watson}{Rezultat Durbin-Watson testa za autokorelaciju reziduala.}
#' @examples
#' \dontrun{
#'   output <- regression_analysis_manual(df)
#'   output$results_table
#'   output$plot
#' }
#' @export
multi_reg <- function(df) {
  if(!requireNamespace("car", quietly = TRUE)) install.packages("car")
  if(!requireNamespace("plotly", quietly = TRUE)) install.packages("plotly")
  library(car)
  library(plotly)

  num_vars <- names(df)[sapply(df, is.numeric)]
  in_var <- select.list(num_vars, multiple = TRUE, title = "Odaberi nezavisne varijable:")
  de_var <- select.list(num_vars, multiple = FALSE, title = "Odaberi zavisnu varijablu:")
  
  X <- as.matrix(df[, in_var, drop = FALSE])
  Y <- as.matrix(df[de_var])
  n <- nrow(X)
  m <- ncol(X) + 1
  B0 <- rep(1, n)
  Xi <- cbind(B0, X)
 
  XTXi <- t(Xi) %*% Xi
  XTY <- t(Xi) %*% Y
  b <- solve(XTXi) %*% XTY
  Yp <- Xi %*% b
  e <- Y - Yp
  se <- round(sqrt(sum(e^2)/(n-m)), 2)

  pss <- sum((Yp - mean(Yp))^2)
  rss <- sum(e^2)
  ro2 <- pss / (pss + rss)
  ro <- round(sqrt(ro2), 2)
  dfp <- m - 1
  dfe <- n - m
  Fval <- (pss / dfp) / (rss / dfe)
  pf_val <- pf(Fval, dfp, dfe, lower.tail = FALSE)
  
df <- data.frame(Y = as.numeric(Y), X)
dw <- car::durbinWatsonTest(lm(Y ~ ., data = df))
  
  XY <- cbind(X,Y)
  Z <- scale(X)
  K <- scale(Y)
  ZK <- round(cbind(Z,K), digits = 2)
  Rxy <- cor(XY)
  R <- cor(X)
  r1 <- (t(Z) %*% K)/(n-1)
  r <- rbind(0, r1)
  beta1 <- solve(R) %*% r1
  beta <- rbind(0,beta1)
  S2 <- 1/diag(solve(R))
  s2 <- data.frame(S2)
  s2<- rbind(0,s2)
  S2xy <- 1/diag(solve(Rxy))
  Sxy <- sqrt(S2xy)
  Pr <- diag(Sxy) %*% solve(Rxy) %*% diag(Sxy)
  pr <- (Pr[,m]*-1)
  pr <- pr[1:m-1]
  pr <- data.frame(pr)
  pr <- rbind(0,pr)
  p <- beta*r
  se <- round(sqrt(sum(e*e)/(n-m)), digits = 2)
  wjj <- solve(XTXi)
  diagwjj <- diag(wjj)
  seb <- se*sqrt(diagwjj)
  tb <- abs(b/seb)
  df <- n-m
  pval <- c()
  for (a in 1:m) {pval[a] <- c(2*(pt(abs(tb[a,1]), df, lower.tail = FALSE)))}
  tb <- data.frame(tb)
  pval <- data.frame(pval)
  rez <- cbind(b, seb, beta, pr, r, p, s2, tb, pval)
  colnames(rez) <- c("B","SE(B)", "Beta", "Part_R", "R","P", "Tolerance", "t", "p")
  print(round(rez, 3))
    
  # Plotly graf
  fig <- plot_ly(
    data = round(rez[-1,], 2),
    x = ~rownames(rez[-1,]),
    y = ~P,
    type = 'bar',
    text = ~P,
    textposition = 'outside'
  ) %>% layout(xaxis = list(title = "Varijable"))
  
  # Povratni objekti
  return(list(
    results_table = rez,
    plot = fig,
    R_squared = ro2,
    SEE = se,
    F_value = Fval,
    F_p_value = pf_val,
    Durbin_Watson = dw
  ))
}
