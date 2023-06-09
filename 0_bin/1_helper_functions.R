
# function to shorten data paths
get_data <- function(path) {
  paste0("../../3_data/RMC/1_data/", path)
}

# pretty print decimal places
specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

add_stars <-
  function(estimate,
           p.value,
           digits = 3,
           levels = c(0.1, 0.05, 0.01),
           symbols = c("\\mbox{*}", "\\mbox{**}", "\\mbox{***}")) {
    
    inds <- order(levels)
    symbols <- c(symbols[inds], "")
    
    sig <- cut(p.value, breaks = c(0, levels[inds], 1), right = FALSE)
    
    paste0(specd(estimate, digits), symbols[as.numeric(sig)])
  }


colSds <- function(x, na.rm=TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x)) # thanks @flodel
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
  return(sqrt(colVar * n/(n-1)))
}