
make_indicate_na <- function(x, prefix = "") {
  if (any(is.na(x))) {
    tags <- sort(unique(na_tag(x)))
    
    if (any(is.na(x) & is.na(na_tag(x)))) {
      tags <- c(tags, "o")
    }
    
    d <- outer(x, tags, function(x, tag)
      1L * ((is.na(x) & !is.na(na_tag(x)) & na_tag(x) == tag) |
              (is.na(x) & is.na(na_tag(x)) & tag == "o"))
    )
    d <- cbind(d, as.numeric(rowSums(d) > 0))
    colnames(d) <- paste0(prefix, "_NA_", c(tags, "any"))
    d
  } else {
    NULL
  }
}

# function for making dummy variables
make_dummies <- function(x, prefix = "") {
	lvls <- sort(unique(x))
	d <- outer(x, lvls, function(x, lvl) 1L * (x == lvl))
	colnames(d) <- paste0(prefix, "_", lvls)
	d
}

make_transformations <- function(x, 
                                 prefix = "",
                                 trans = list(
                                   "sqrt" = function(x) sqrt(x),
                                   "cubert" = function(x) x^(1/3),
                                   "poly1" = function(x) x,
                                   "poly2" = function(x) x^2,
                                   "poly3" = function(x) x^3,
                                   "log" = function(x) log(x),
                                   "exp" = function(x) exp(x)
                                 )) {
  if (min(x, na.rm = TRUE) == 0) {
    xt <- lapply(trans, function(f) f(x + 1))
  } else {
    xt <- lapply(trans, function(f) f(x))
  }
  
  names(xt) <- paste0(prefix, "_", names(trans))
  
  bind_cols(xt)
}
