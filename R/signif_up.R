#' round up to most significant digit
#'
#' _round up_ a number to the most meaningful digits per scale of the number
#'
#' @param x numeric
#'
#' @return numeric
#' @export
#'
#' @examples
#' signif_up(1320.65) # 1400
#' signif_up(132.5) # 140
#' signif_up(32.6)  # 40
#' signif_up(3.6)   # 4
#' signif_up(0.0654)  # 0.07
#' signif_up(c(1320.654, 3675.675, 5.7, 0.76))
signif_up = function(x) {
  num_string <- format(x, scientific = TRUE)
  n <- strsplit(num_string, "e")
  n1 <- sapply(n, function(x)
    as.numeric(x[1]))
  n2 <- sapply(n, function(x)
    as.numeric(x[2]))
  digits = sapply(x, function(x)
    if (x > 100) {
      digits = 1
    } else {
      digits = 0
    })
  ceiling(n1 * 10 ^ digits) * 10 ^ (n2 - digits)
}
