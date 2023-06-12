### R code from vignette source 'vignette.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: vignette.Rnw:43-46
###################################################
library('SINGLE')
set.seed(1)
sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.1, str=-.6)


###################################################
### code chunk number 2: vignette.Rnw:53-54
###################################################
sim$true_cov[,,1]


###################################################
### code chunk number 3: vignette.Rnw:59-60
###################################################
S = SINGLE(data=sim$data, verbose=TRUE)


###################################################
### code chunk number 4: vignette.Rnw:65-66 (eval = FALSE)
###################################################
## S = SINGLE(data=sim$data, l1=.75, l2=.5, h=40)


###################################################
### code chunk number 5: vignette.Rnw:71-72 (eval = FALSE)
###################################################
## S = SINGLE(data=sim$data, l1=seq(.25,1, .25), l2=seq(.25,1, .25), h=c(30,40,50))


###################################################
### code chunk number 6: vignette.Rnw:77-82
###################################################
data = sim$data
h_G = choose_h(data=data, sample_size=30, kernel="gaussian", 
               h=seq(10,100,10))
h_W = choose_h(data=data, sample_size=30, kernel="window", 
               h=seq(10,100,10))


###################################################
### code chunk number 7: vignette.Rnw:87-91
###################################################
C_gaus = get_kern_cov(data=data, h=h_G, kernel="gaussian")
C_slid = get_kern_cov(data=data, h=h_W, kernel="window")
S_gaus = SINGLE(data=data, C=C_gaus, l1=.75, l2=0.5)
S_slid = SINGLE(data=data, C=C_slid, l1=.75, l2=0.5)


###################################################
### code chunk number 8: vignette.Rnw:98-101
###################################################
plotSINGLE(object=S_gaus, index=c(1,2,3,4,5), x.axis = seq(1,150), 
           n.row=2, 
           col.names=seq(1,5), fix.axis=TRUE)


###################################################
### code chunk number 9: vignette.Rnw:110-113
###################################################
result = precision_recall(true_cov=sim$true_cov, estimated_cov=S_gaus$P_)
plot(result$F1, type='l', ylim=c(0,1), ylab='', 
     main='F Score', xlab='Time') 


###################################################
### code chunk number 10: vignette.Rnw:145-147 (eval = FALSE)
###################################################
## sim_ER = generate_random_data(ROI=5, length_=50, mode='ER', seg=3, sparsity=.1)
## sim_BA = generate_random_data(ROI=5, length_=50, mode='BA', seg=3, sparsity=.1)


###################################################
### code chunk number 11: vignette.Rnw:151-154
###################################################
sim1 = generate_random_data(ROI=5, length_=25, mode='BA', seg=1, sparsity=.15)
sim2 = generate_random_data(ROI=5, length_=75, mode='BA', seg=1, sparsity=.15)
data = rbind(sim1$data, sim2$data)


###################################################
### code chunk number 12: vignette.Rnw:169-173 (eval = FALSE)
###################################################
## set.seed(1)
## sim = generate_random_data(ROI=5, length_=50, seg=3, sparsity=.1, str=-.6)
## data = sim$data
## S = SINGLE(data=data)


###################################################
### code chunk number 13: vignette.Rnw:190-191 (eval = FALSE)
###################################################
## result = precision_recall(true_cov=sim$true_cov, estimated_cov=S$P_)


###################################################
### code chunk number 14: vignette.Rnw:200-202
###################################################
plotSINGLE(object=S, index=c(1,2,3,5), x.axis = seq(1,150), n.row=2, 
           col.names=c(1,2,3,5), fix.axis=TRUE)


