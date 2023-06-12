plotSINGLE <-
function (object, index, x.axis = NULL, col.names = NULL, n.row = NULL, 
    fix.axis = FALSE) 
{
    if (!is.array(object$P_)) {
        stop("Incorrect object entered.")
    }
    if (length(index) == 1) {
        if (is.na(index)) {
            index = seq(dim(object$P_)[1])
        }
    }
    if (length(index) > 5) {
        cat("Warning - margins may be too large. Try exporting directly as PDF/PNG\n")
    }
    if (class(x.axis) == "NULL") {
        x.axis = seq(1, dim(object$P_)[3])
    }
    if (class(col.names) == "NULL") {
        col.names = index
    }
    if (class(n.row) == "NULL") {
        n.row = 2
    }
    for (i in 1:dim(object$P_)[3]) {
        d = sqrt(diag(object$P_[, , i]))
        object$P_[, , i] = -1 * object$P_[, , i]/(d %*% t(d)) + 
            diag(nrow(object$P_[, , i]))
    }
    par(mfrow = c(n.row, ceiling(choose(length(index), 2)/n.row)))
    if (fix.axis == TRUE) {
        y.lim = range(object$P_[index, index, ])
        for (i in 1:length(index)) {
            for (j in i:length(index)) {
                if (i == j) {
                }
                else {
                  plot(x.axis, object$P_[index[i], index[j], 
                    ], type = "l", ylim = y.lim, ylab = "", xlab = "", 
                    main = paste("PC: ", col.names[i], col.names[j]))
                }
            }
        }
    }
    else {
        for (i in 1:length(index)) {
            for (j in i:length(index)) {
                if (i == j) {
                }
                else {
                  plot(x.axis, object$P_[index[i], index[j], 
                    ], type = "l", ylab = "", xlab = "", main = paste("PC: ", 
                    col.names[i], col.names[j]))
                }
            }
        }
    }
}

# function(object, index, x.axis, col.names, n.row, fix.axis=FALSE){
#   # Function to plot partial correlations as calculated by the SINGLE algorithm
#   # INPUT:
#   #     - object: a SINGLE object such that object$P_ is an array of partial correlations
#   #     - index: a subset of partial correlations to plot
#   #     - x.axis: values for xaxis
#   #     - col.names: names for columns
#   #     - n.row: number of rows of plots
#   #     - fix.axis: boolean, if TRUE then all axis will be the same on the resulting plots, otherwise each plot will have its own axis
#   
#   # OUTPUT:
#   #     - N by N plots of partial correlations
#   
#   if (!is.array(object$P_)){
#     stop('Incorrect object entered.')
#   }
#   
#   if (length(index)==1){
#     if (is.na(index)){
#       index = seq(dim(object$P_)[1])
#     }
#   }
#   
#   if (length(index)>5){
#     cat("Warning - margins may be too large. Try exporting directly as PDF/PNG\n")
#   }
#   
#   # normalise everything to a matrix of partial correlations
#   # to do this we divide by sqrt of variances and multiply by -1
#   
#   for (i in 1:dim(object$P_)[3]){
#     d = sqrt(diag(object$P_[,,i]))
#     object$P_[,,i] = -1* object$P_[,,i]/ (d %*% t(d)) + diag(nrow(object$P_[,,i])) # note that I have made diagonals 0, they should be 1
#   }
#   
#   #   par(mfrow=c(length(index), length(index)))
#   par(mfrow=c(n.row, ceiling(choose(length(index),2)/n.row)))
#   
#   if (fix.axis == TRUE){
#     y.lim = range(object$P_[index,index,])
#     for (i in 1:length(index)){
#       for (j in i:length(index)){
#         if (i==j){
#           #           pass
#           #           plot(x.axis, object$P_[index[i], index[j], ], type='l', ylim=y.lim, ylab = '', xlab='', main = paste('VAR: ', col.names[i]))
#         } else {
#           plot(x.axis, object$P_[index[i], index[j], ], type='l', ylim=y.lim, ylab = '', xlab='', main = paste('PC: ', col.names[i], col.names[j]))
#         }
#       }
#     }
#   } else {
#     for (i in 1:length(index)){
#       for (j in i:length(index)){
#         if (i==j){
#           #           pass
#           #           plot(x.axis, object$P_[index[i], index[j], ], type='l', ylim=y.lim, ylab = '', xlab='', main = paste('VAR: ', col.names[i]))
#         } else {
#           plot(x.axis, object$P_[index[i], index[j], ], type='l', ylab = '', xlab='', main = paste('PC: ', col.names[i], col.names[j]))
#         }
#       }
#     }    
#   }
# }
