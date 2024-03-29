#' @export
topological.binary.default=function(x,B,alpha=0.05,critical.value){
  
  check(test=6,x=x,B=B,alpha=alpha,critical.value=critical.value)
  suppressWarnings({
    res.tbl=topological.binary.main(x=x,B=B,alpha=alpha,critical.value=critical.value)
  })
  res.tbl$call = match.call()
  class(res.tbl) = c("topological.binary","CryptRndTest")
  res.tbl
}