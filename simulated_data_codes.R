set.seed(12)
simulated_data_codes <- function(nn,samples,case, numhub, bighub, shlb, shub,bigwgt,disconnected){
  
  ssigma <- matrix(0, nn,nn)
  diag(ssigma) <- 1
  
  covx<-matrix(0,nn,2)
  
  for (i in 1:nn) {
    covx[i,1]<-runif(1)
    covx[i,2]<-runif(1)
  }
  
  dij<-matrix(0,nn,nn)
  for (i in 1:nn) {
    for (j in 1:nn) {
      dij[i,j]<-sqrt((covx[i,1]-covx[j,1])^2+(covx[i,2]-covx[j,2])^2)
    }
  }
  
  ################################### tested so far
  ################### modify the matrix
  counts<-0
  
  yyy<-bighub
  conx <- matrix(0, nn, nn)
  for (i in 1:numhub) {
    tmp<-order(dij[i,1:nn],decreasing=F)[1:bighub] # change here to make hubs or cliques
    conx[i,tmp]<-1
    conx[tmp,i]<-1
  }
  
  #yyy<-smallhub
  
  for (i in (numhub+1):nn) {
    tmp<-order(dij[i,1:nn],decreasing=F)[shlb:shub]
    conx[i,tmp]<-1
    conx[tmp,i]<-1
  }
  
  for (xx in (nn-disconnected+1):nn){
    for (yy in 1:nn){
      conx[xx,yy]<-0
      conx[yy,xx]<-0
    }
  }
  
  diag(conx) <- 0
  temp <- matrix(1, nn, nn)
  for (ii in 1:nn){
    for (iii in 1:ii){
      temp[iii, ii] <- 0
    }
  }
  
  premat<-matrix(runif(nn*nn, 0, 1), ncol=nn)
  premat[temp==0] <- 0
  diag(premat)<-1
  premat[conx!=1] <- 0
  premat <- premat + t(premat)
  premat[premat > 0 & premat < 0.5] <- premat[premat > 0 & premat < 0.5] - 1
  diag(premat) <- apply(abs(premat), 1, sum) + 0.5
  premat1 <- sqrt(as.matrix(diag(premat))%*%t(as.matrix(diag(premat))))
  premat <- premat/premat1
  covmat<-solve(premat)
  ##find sqaure root of a matrix
  eigencov<-eigen(covmat)
  D<-matrix(0,nn,nn)
  diag(D)<-sqrt(eigencov$values)
  covmats<-eigencov$vectors %*%  D %*% t(eigencov$vectors)
  #print("d")
  bighubnum<-1
  counts<- (counts+1)
  
  simudat<-matrix(0,samples,nn)   # change n = 300, keep p = 100
  for (i in 1:samples){    
    if (i <= numhub){
      simudat[i,]<-covmats %*% rnorm(nn)         
    }
    else {
      simudat[i,]<-covmats %*% rnorm(nn)           
    }
  }
  ##############################################################################################################################################
  
  temp<-c()
  haha<-simudat # right before cases
  
  #case1
  if (case ==1){
    
    print(dim(simudat))
    
    simudat[,1:numhub] <- simudat[,1:numhub] * bigwgt   # ((bigwgt-1) *3 +1)
  }
  else if (case ==2){
    #case2
    simudat[,1:floor(numhub/2)] <- simudat[,1:floor(numhub/2)] * bigwgt   # ((bigwgt-1) *3 +1)
    simudat[,(numhub+1):(numhub+numhub-floor(numhub/2))] <- simudat[,(numhub+1):(numhub+numhub-floor(numhub/2))] * bigwgt  # ((bigwgt-1) *3 +1)
  }
  else if (case ==3){
    #case3
    simudat[,(numhub+1):(numhub+numhub)] <- simudat[,(numhub+1):(numhub+numhub)] * bigwgt  # ((bigwgt-1) *3 +1)
    #simudat[,1:floor(numhub/2)] <- simudat[,1:floor(numhub/2)] * bigwgt           # ((bigwgt-1) *3 +1)
    #simudat[,(numhub+1):(numhub+floor((nn-numhub)/2))] <- simudat[,(numhub+1):(numhub+floor((nn-numhub)/2))] * bigwgt    # ((bigwgt-1) *3 +1)
  }
  else if (case ==4){  #random
    temp<-sample(nn,numhub)
    simudat[,temp] <- simudat[,temp] * bigwgt      # ((bigwgt-1) *3 +1)
  }
  else if (case ==5){
    simudat[,sample(nn,(nn/2))] <- simudat[,sample(nn,nn/2)] * bigwgt   #((bigwgt-1) *3 +1)
  }
  
  ##########
  #print('jjj')
  s<-(simudat)
  #print(class(s))
  simufilename=paste("simudata-","nn",nn,"sp",samples,"nh",numhub,"bighub",bighub,"shlb",shlb,"shub",shub,"bigwgt",bigwgt,".txt", sep='')
  #write.table(s,simufilename)
  conxfilename=paste("originconx","nn",nn,"sp",samples,"nh",numhub,"bighub",bighub,"shlb",shlb,"shub",shub,"bigwgt",bigwgt,".txt", sep='')
  #write.table(conx,conxfilename)
  newList <- list("simudata" = s, "originconx" = conx, "rawsimudata"=haha)
  write(conx,'origin.txt',ncol=nn)
  return(newList)
}