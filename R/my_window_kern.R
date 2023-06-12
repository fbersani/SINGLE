my_window_kern <-
function(a,b, radius){
  return(as.numeric(abs(a-b)<radius))
}
