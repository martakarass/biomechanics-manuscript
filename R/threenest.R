##########################################################################################
## Function for fitting N3 model based on the input data
## smooth.y: if TRUE, conduct smoothing on the raw data 
## Y: raw data; the data is stored in a list of length Q where each element is of dimension q by n and q is part of the whole data dimension
##########################################################################################

#' Coefficient of intra-class correlation  (ICC) 
#' 
#' Estimating the coefficient of intra-class correlation (ICC). 
#'
#' @param Y 
#' @param L1 
#' @param L2 
#' @param L3 
#' @param smooth.h 
#' @param smooth.y 
#' @param alg.y 
#' @param bf 
#' @param Q 
#'
#' @return
#' @export
#'
#' @examples
threenest <- function(Y, L1, L2, L3, smooth.h = FALSE, 
                      smooth.y = TRUE, alg.y = 'smoothspline', bf = 5, Q = NULL, J = NULL){
  
  # MK added -- Dec 30, 2021
  if (is.null(Q)) Q <- L1 
  if (is.null(J)) J <- L2 

  I_index=rep(1:L1,each=L2*L3)
  n=length(I_index)
  IJ_index=rep(1:(L1*L2),each=L3)
  
  I=max(I_index)
  n_I0=table(I_index)
  IJ=max(IJ_index)
  n_IJ=table(IJ_index)
  
  Z <- Reduce(rbind,Y); 
  Z= t(Z)
  
  p=dim(Z)[1]
  #sigma.hat<- sqrt(mean(apply(Z,2,var)))
  
  ###STEP 1+: smooth the raw data
  if(smooth.y){
    Y_new <-list()
    
    Y_new<-lapply(1:n, function(i) {m<-smooth.spline(as.numeric(Z[,i]));  return(predict(m)$y)}	)
    Y <-Reduce(cbind,Y_new)
  }
  
  sigma.hat<- sqrt(mean((Y-Z)^2))
  
  #Obtain U/S matrices from SVD
  UUt <- t(Y)%*%Y #lapply(1:Q,function(q) t(Y[[q]])%*%Y[[q]])  #t(Y)%*%Y
  #UUt <- Reduce('+',UUt)
  U=svd(UUt)$u; S=svd(UUt)$d
  
  k1=sum(n_IJ^2)
  U_IJ=rowsum(U,IJ_index)
  
  k2=sum(n_I0^2)
  U_I=rowsum(U,I_index)

  ##step 1 obtain covariance matrix in the reduced dimension of U
  HW=(t(U)%*%diag(n_IJ[IJ_index])%*%U-t(U_IJ)%*%(U_IJ))*2/(k1-n)
  HU=(t(U)%*%diag(n_I0[I_index])%*%U-t(U_I)%*%(U_I)-(k1-n)/2*HW)*2/(k2-k1)
  HX=(n*t(U)%*%U-colSums(U)%*%t(colSums(U))-(k1-n)/2*HW-(k2-k1)/2*HU)*2/(n^2-k2)
  
  SHUS<-t(t(sqrt(S)*HU)*sqrt(S))
  SHXS<-t(t(sqrt(S)*HX)*sqrt(S))
  SHWS<-t(t(sqrt(S)*HW)*sqrt(S))
  
  
  ##Step 2+  SMOOTHING THE COVARIANCE OPERATOR
  if(smooth.h){
    hw.temp <- SHWS; diag(hw.temp) <- rep(NA, n)
    hu.temp <- SHUS; diag(hu.temp) <- rep(NA, n)
    hx.temp <- SHXS; diag(hx.temp) <- rep(NA, n)
    
    data.cov <- data.frame(hU=as.vector(hu.temp),hW=as.vector(hw.temp),hX=as.vector(hx.temp),x1=rep(seq(0,1,length=n), n), x2=rep(seq(0,1,length=n), each=n))
    attach(data.cov)
    
    fitU<- gam(hU ~ te(x1, x2)); newdata <- data.frame(x1=x1,x2=x2); predU <- predict(fitU,newdata)
    fitX<- gam(hX ~ te(x1, x2)); predX <- predict(fitX,newdata)
    fitW<- gam(hW ~ te(x1, x2)); predW <- predict(fitW,newdata)
    
    
    ### obtain the smoothed covariance functions Gw(s,t) and Gb(s,t)
    sigma.hat<- max(0,mean( c(diag(SHWS) - diag(matrix(predW,n)), diag(SHUS) - diag(matrix(predU,n)), diag(SHXS) - diag(matrix(predX,n)))))/2
    
    SHWS<-SHWS-2*var.noise[m]*diag(rep(1,n)); SHUS<-SHUS-2*var.noise[m]*diag(rep(1,n));SHXS<-SHXS-2*var.noise[m]*diag(rep(1,n))
    
    #s.hW <-matrix(predW,n); SHWS <- (s.hW + t(s.hW) )/2 ; s.hU<- matrix(predU, n); SHUS <- (s.hU + t(s.hU))/2
    #s.hX <-matrix(predX,n); SHXS <- (s.hX + t(s.hX))/2
  }
  
  SKuWS=SHWS/2
  SKuUS=(SHUS-SHWS)/2
  SKuXS=(SHXS-SHUS)/2	
  
  
  A_U = svd(SKuUS)$u
  Sigma_U=svd(SKuUS)$d
  A_X = svd(SKuXS)$u
  Sigma_X=svd(SKuXS)$d
  A_W = svd(SKuWS)$u
  Sigma_W=svd(SKuWS)$d
  #get the eigen values
  # end of Step 2 
  
  # Step 3 
  Phi_U<-Phi_X<-Phi_W<-numeric(0)
  # for(q in 1:Q){
  #  Cs = t(U)%*%t(Y[[q]])
  # Phi_U = cbind(Phi_U,t(A_U/sqrt(S))%*%Cs)
  # Phi_X = cbind(Phi_X,t(A_X/sqrt(S))%*%Cs)
  # Phi_W = cbind(Phi_W,t(A_W/sqrt(S))%*%Cs)
  #}
  
  Cs = t(U)%*%t(Y)
  Phi_U = t(A_U/sqrt(S))%*%Cs
  Phi_X = t(A_X/sqrt(S))%*%Cs
  Phi_W = t(A_W/sqrt(S))%*%Cs
  
  N1= 20
  N2= 20
  N3= 20
  
  # end of Step 3 
  AN1_X = matrix(0,nrow=n, ncol=N1)
  AN2_U = matrix(0,nrow=n, ncol=N2)
  AN3_W = matrix(0,nrow=n, ncol=N3)
  
  
  # control the sign of the eigenvector
  for(k in 1:N1){
    AN1_X[,k] = A_X[,k]
  }
  
  
  #   if ( Phi_X[k,] %*% phi1[,k]  < 0) 
  #   {
  #     Phi_X[k,]= - Phi_X[k,]
  #     AN1_X[,k] = -AN1_X[,k]
  #   }
  # }
  lambda1e = Sigma_X[1:N1]
  phi1e = t(Phi_X[1:N1,])
  
  
  for( k in 1:N2){
    AN2_U[,k] = A_U[,k]
  }
  
  
  lambda2e=Sigma_U[1:N2]
  phi2e = t(Phi_U[1:N2,])
  
  for( k in 1:N3){
    AN3_W[,k] = A_W[,k]
  }
  
  
  # if ( Phi_W[k,] %*% phi3[,k]  < 0) 
  # {
  #   Phi_W[k,] = -Phi_W[k,]
  #   AN3_W[,k] = -AN3_W[,k]
  # }
  
  lambda3e=Sigma_W[1:N3]
  phi3e = t(Phi_W[1:N3,])
  
  phi.hat<-list(phi1e,phi2e,phi3e)
  lambda.hat <-list(lambda1e,lambda2e,lambda3e)
  
  
  # Step 4: get scores ##
  dij = rep(1,L3)
  dJ=rep(1,L2)
  di=rep(1,L2*L3)
  C_XU = t(AN1_X)%*% AN2_U
  C_XW = t(AN1_X)%*% AN3_W
  C_UW = t(AN2_U)%*% AN3_W
  
  D11 = n_I0[1]*diag(rep(1,N1))
  D12 = kronecker(t(dJ),L3*C_XU)
  D13 = kronecker(t(di),C_XW)
  D21 = t(D12)
  D22= kronecker(diag(rep(1,J)),L3*diag(rep(1,N2)))
  D23= kronecker(diag(rep(1,J)),kronecker(t(dij),C_UW))
  D31=t(D13)
  D32=t(D23)
  D33 = diag(rep(1,L2*L3*N3))
  
  D = rbind(cbind(D11,D12,D13),cbind(D21,D22,D23),cbind(D31,D32,D33))
  
  ##get the AY part 
  # ps_X = matrix(0,N1,I)
  # ps_U = matrix(0,L2*N2,I)  
  # ps_W = matrix(0,L2*L3*N3,I)
  
  ps_X = matrix(0,N1,I)
  ps_U = matrix(0,L2*N2,I)  
  ps_W = matrix(0,L2*L3*N1,I)
  
  
  
  # for (i in 1:I){
  #   rs_i = numeric(0)
  #   rsb_i = t(AN1_X) %*% sqrt(diag(S))%*% U_I[i,]
  #   rs_i = c(rs_i, rsb_i)
  #   for (j in 1:J){
  #     rsu_ij <- t(AN2_U) %*% sqrt(diag(S)) %*% U_IJ[(L2*(i-1)+j),]
  #     rs_i = c(rs_i, rsu_ij) 
  #   } 
  #   
  #   rsw_ijk=c(t(AN3_W) %*% sqrt(diag(S)) %*% t(U[((L2*L3*(i-1)+1):(L2*L3*i)),]))
  #   rs_i=c(rs_i,rsw_ijk)
  # 
  #   svdd=svd(D)
  #   ps_i = svdd$v%*%diag(1/svdd$d*(svdd$d>0.0001))%*%t(svdd$u) %*% rs_i
  #   ps_X[,i] = ps_i[1:N1,]
  #   ps_U[,i] = ps_i[(N1+1):(N1+N2*L2),]
  #   ps_W[,i] = ps_i[(N1+N2*L2+1):(N1+N2*L2+L2*L3*N3),]
  # }
  
  
  
  
  for (i in 1:I){ # i <- 1
    rs_i = numeric(0)
    rsb_i = t(AN1_X) %*% sqrt(diag(S))%*% U_I[i,]
    rs_i = c(rs_i, rsb_i)
    for (j in 1:J){
      rsu_ij <- t(AN2_U) %*% sqrt(diag(S)) %*% U_IJ[(L2*(i-1)+j),]
      rs_i = c(rs_i, rsu_ij) 
    } 
    
    rsw_ijk=c(t(AN3_W) %*% sqrt(diag(S)) %*% t(U[((L2*L3*(i-1)+1):(L2*L3*i)),]))
    rs_i=c(rs_i,rsw_ijk)
    
    svdd=svd(D)
    ps_i = svdd$v%*%diag(1/svdd$d*(svdd$d>0.0001))%*%t(svdd$u) %*% rs_i
    # @MK added 2022-01-30
    # dim(ps_i)
    # [1] 860   1
    ps_X[,i] = ps_i[1:N1,]
    ps_U[,i] = ps_i[(N1+1):(N1+N2*L2),]
    ps_W[,i] = ps_i[(N1+N2+1):(N1+N2+L2*L3*N1),]
    # @MK added 2022-01-30
    # range((N1+N2+1):(N1+N2+L2*L3*N1))
    # 41 840
  }
  
  # if ( Phi_U[k,] %*% phi2[,k]  < 0) 
  # {
  #   Phi_U[k,]= - Phi_U[k,]
  #   AN2_U[,k] = -AN2_U[,k]
  #}
  
  #for (j in 1:J)
  #for(k in 1:L3)
  #{ 
  # rsw_ijk=t(AN3_W) %*% sqrt(diag(S)) %*% t(U[(L2*L3*(i-1)+L2*(j-1)+k),])
  xi.hat<-list(psx=ps_X,psu=ps_U,psw=ps_W)
  
  ### return results ###
  results <- list(phi = phi.hat, sigma = sigma.hat, lambda = lambda.hat, xi = xi.hat)
  
  ICC=sum(lambda.hat[[1]])/(sum(lambda.hat[[1]])+sum(lambda.hat[[2]])+sum(lambda.hat[[3]]))
  
  return(list("results"=results,"ICC"= ICC))
}
