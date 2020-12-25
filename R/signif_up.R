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
