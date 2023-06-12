my_exp_kern <-
function(a,b,radius=1){
  exp(-(a-b)*(a-b)/(2*radius*radius))
}
