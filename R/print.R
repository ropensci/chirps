#' @method print chirps_df
#' @export
print.chirps_df <- function(x, ...){
  
  x <- as.data.frame(x, stringAsFactor = FALSE)
  
  classes <- lapply(x, function(y){
    class(y)
  })
  
  classes <- as.vector(unlist(classes))
  class_abb <- c(list = "<list>", integer = "<int>", numeric = "<dbl>", 
                character = "<chr>", Date = "<date>", complex = "<cpl>", 
                factor = "<fct>", POSIXct = "<POSc>", logical = "<lgl>", 
                IDate = "<IDat>", integer64 = "<i64>", raw = "<raw>", 
                expression = "<expr>", ordered = "<ord>")
  
  abbs <- unname(class_abb[classes])
  
  nc <- dim(x)[[2]]
  nr <- dim(x)[[1]]
  
  dbl <- abbs %in% "<dbl>"
  
  x[dbl] <- lapply(x[dbl], function(y){
    format(round(y, 2), nsmall = 2)
  })
  
  x[1:nc] <- lapply(x, as.character)
  
  if (nr <= 10L) {
    
    toprint <- rbind(abbs, x)
    
    rownames(toprint) <- c("", paste0(row.names(x), ":"))
    
  }
  
  if (nr > 10L) {
    
    he <- .head(x)
    
    ta <- .tail(x)
    
    toprint <- rbind(abbs, 
                     he, 
                     rep("", dim(x)[[2]]), 
                     ta)
    
    rownames(toprint) <- c("",
                           paste0(row.names(he), ":"),
                           "---",
                           paste0(row.names(ta), ":"))
    
  }
  
  print(toprint)
  
}


.tail <- function(x, n = 5L, addrownums = TRUE, ...) {
  stopifnot(length(n) == 1L)
  nrx <- nrow(x)
  n <- if (n < 0L) 
    max(nrx + n, 0L)
  else min(n, nrx)
  sel <- as.integer(seq.int(to = nrx, length.out = n))
  ans <- x[sel, , drop = FALSE]
  if (addrownums && is.null(rownames(x))) 
    rownames(ans) <- format(sprintf("[%d,]", sel), justify = "right")
  ans
}


.head <- function (x, n = 5L, ...) {
  stopifnot(length(n) == 1L)
  n <- if (n < 0L) 
    max(nrow(x) + n, 0L)
  else min(n, nrow(x))
  x[seq_len(n), , drop = FALSE]
}
