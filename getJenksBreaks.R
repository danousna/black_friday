getJenksBreaks <- function(var, k, subset = NULL) {
  k <- k - 1;
  
  #if more breaks than unique values, segfault, so avoid
  if (k > length(unique(var))) {
    k <- length(unique(var));
  }
  brks <- rep(1, k + 1);
  
  #if requested, regularly sample subset values
  if (!is.null(subset)) {
    if (length(var) > subset) {
      ind <- c(seq(from=1, to=length(var), by=floor(length(var)/subset)), length(var));
      var <- var[ind];
    }
  }
  
  d <- sort(var);
  length_d <- length(d);
  
  out <- NULL
  out$d <- as.double(d)
  out$k <- as.integer(k)
  out$length_d <- as.integer(length_d)
  out$brks <- as.double(brks)
}