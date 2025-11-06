#' PMF calculation fo the sigend rank wilcox test for small sample sizes
#'
#' @param n Sample Size
#' @param lower_percentile the lower percentile for the significance level (here 0.025 as default for 0.05/2
#' @param upper_percentile the upper percentile for the significance level (here 0.975 as default for 100-(0.05/2)
#'
#' @returns a list of pmf with critical values
#' @export
#'
#' @examples
#' out = dwilcox_signed_rank(10)
#'
dwilcox_signed_rank <- function(n,lower_percentile = 0.025,upper_percentile = 0.975) {
  # Generiere alle 2^n möglichen Vorzeichenkombinationen (0=positiv, 1=negativ)
  signs <- as.matrix(do.call(expand.grid, rep(list(c(0, 1)), n)))
  W_counts <- rep(0, n * (n + 1) / 2 + 1)  # Zähler für jede mögliche W

  for (i in 1:nrow(signs)) {
    # Berechne die Ränge der absoluten Differenzen (1:n als Platzhalter)
    ranks <- rank(abs(1:n - stats::median(1:n)))
    # Summe der Ränge, wo das Vorzeichen "positiv" ist (signs[i,] == 0)
    W <- sum(ranks[signs[i, ] == 0])
    W_counts[W + 1] <- W_counts[W + 1] + 1  # +1 wegen 0-basiertem Index
  }

  # PMF = Anzahl der Konfigurationen / Gesamtzahl (2^n)
  W_values <- 0:(n * (n + 1) / 2)
  pmf <- data.frame(W = W_values, density = W_counts[W_values + 1] / 2^n)

  # Berechne kritische Werte für alpha=0.05 (zweiseitig)
  cumulative <- cumsum(pmf$density)
  lower_crit <- min(pmf$W[cumulative >= lower_percentile])
  upper_crit <- max(pmf$W[cumulative <= upper_percentile])

  # Gib PMF und kritische Werte zurück
  list(pmf = pmf, lower = lower_crit, upper = upper_crit)
}
