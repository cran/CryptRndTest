#' @export
GCD.test=function(x,B=32,KS=TRUE,CSQ=TRUE,AD=TRUE,JB=TRUE,test.k=TRUE,test.g=TRUE,mu,sd,alpha=0.05) UseMethod("GCD.test")