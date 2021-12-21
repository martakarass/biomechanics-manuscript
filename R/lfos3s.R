#' Fit longitudinal function-on-scalar regression
#' 
#' Fit longitudinal function-on-scalar regression using the proposed 3-step approach
#' proposed and implemented in Cui et al. (2021). Minor edits 
#' (non-parallel handling, message prining, other) introduced by Marcos Matabuena. 
#'  
#' @param formula two-sided formula object in lmer() format, except that the response is a matrix  
#' @param data data frame containing variables in formula
#' @param family GLM family of the response
#' @param argvals locations of observations on the functional domain
#' @param var whether to estimate variance. By default FALSE
#' @param analytic whether to use the analytic inference approach
#' @param parallel whether to run parallel computing
#' @param silent whether to show descriptions of each step
#' @param B_boot number of bootstrap repetitions in step 3 (bootstrap inference case)
#' @references
#' Erjia Cui, Andrew Leroux, Ekaterina Smirnova, Ciprian M. Crainiceanu (2021) 
#' Fast Univariate Inference for Longitudinal Functional Models, 
#' Journal of Computational and Graphical Statistics, 
#' DOI: 10.1080/10618600.2021.1950006
#' @export
#' @return a list containing estimated beta(s)

lfosr3s <- function(formula, data, family = "gaussian", argvals = NULL, var = FALSE, 
                    analytic = TRUE, parallel = FALSE, silent = FALSE, B_boot = 100){
  
  require(lme4) ## mixed models
  require(refund) ## fpca.face
  require(dplyr) ## organize lapply results
  require(progress) ## display progress bar
  require(mgcv) ## smoothing in step 2
  require(mvtnorm) ## joint CI
  require(parallel) ## mcapply
  
  if(family != "gaussian") analytic <- FALSE ## bootstrap inference for non-Gaussian family
  
  ## Organize the input
  model_formula <- as.character(formula)
  stopifnot(model_formula[1] == "~" & length(model_formula) == 3)
  
  
  ##########################################################################################
  ## Step 1
  ##########################################################################################
  if(silent == FALSE) print("Step 1: Massive Univariate Mixed Models")
  
  L <- ncol(data[,model_formula[2]]) ## number of observations on the functional domain
  if(is.null(argvals)) argvals <- 1:L
  
  ## function "unimm" fit univariate mixed model at location l
  unimm <- function(l){
    data$Yl <- unclass(data[,model_formula[2]][,l])
    if(family == "gaussian"){
      fit_uni <- suppressMessages(lmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                       data = data, control = lmerControl(optimizer = "bobyqa")))
    }else{
      fit_uni <- suppressMessages(glmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                        data = data, family = family, control = glmerControl(optimizer = "bobyqa")))
    }
    betaTilde <- lme4::fixef(fit_uni)
    if(analytic == TRUE){
      varcorr <- as.data.frame(VarCorr(fit_uni))
      var_random <- varcorr[,4] ## variance/covariance estimates
      ind_var <- which(is.na(varcorr[,3]) & varcorr[,1] != "Residual") ## variance of random components
      names(var_random)[ind_var] <- paste0("var.",varcorr[ind_var,1],".",varcorr[ind_var,2])
      names(var_random)[which(varcorr[,1] == "Residual")] <- "var.Residual"
      names(var_random)[which(!is.na(as.data.frame(VarCorr(fit_uni))[,3]))] <- "cov"
      return(list(betaTilde = betaTilde, var_random = var_random, group = varcorr[1,1]))
    }else{
      return(list(betaTilde = betaTilde, group = as.data.frame(VarCorr(fit_uni))[1,1]))
    }
  }
  
  ## fit massive univariate mixed models
  if(parallel == TRUE){
    massmm <- mclapply(argvals, unimm, mc.cores = detectCores() - 1)
  }else{
    if(!(silent)) print(argvals)
    massmm= 1:length(argvals) 
    massmm= as.list(massmm)
    for(i in 1:length(argvals)){
      massmm[[i]]= unimm(i)
      print(massmm[[i]])
    }
    # massmm <- lapply(argvals, unimm)
  }
  
  ## obtain betaTilde
  betaTilde <- t(lapply(massmm, '[[', 1) %>% bind_rows())
  colnames(betaTilde) <- argvals
  
  ## obtain variance estimates of random part when necessary
  if(analytic == TRUE){
    var_random <- t(lapply(massmm, '[[', 2) %>% bind_rows())
    sigmaesqHat <- var_random["var.Residual",,drop = FALSE]
    sigmausqHat <- var_random[which(rownames(var_random) != "var.Residual"),,drop = FALSE]
    rm(var_random)
    ## fit a placeholder model to obtain design matrix
    data$Yl <- unclass(data[,model_formula[2]][,1])
    if(family == "gaussian"){
      fit_uni <- suppressMessages(lmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                       data = data, control = lmerControl(optimizer = "bobyqa")))
    }else{
      fit_uni <- suppressMessages(glmer(formula = as.formula(paste0("Yl ~ ", model_formula[3])), 
                                        data = data, family = family, control = glmerControl(optimizer = "bobyqa")))
    }
    designmat <- model.matrix(fit_uni) ## model design matrix, necessary for analytic inference
    name_random <- as.data.frame(VarCorr(fit_uni))[which(!is.na(as.data.frame(VarCorr(fit_uni))[,3])),3]
    rm(fit_uni)
  }
  
  
  ##########################################################################################
  ## Step 2
  ##########################################################################################
  if(silent == FALSE) print("Step 2: Smoothing")
  
  nknots <- min(round(L/4), 35) ## number of knots for penalized splines smoothing
  if(analytic == TRUE){ ## we need smoothing parameter, spline basis and penalty matrix for analytic inference
    p <- nrow(betaTilde) ## number of fixed effects parameters
    betaHat <- matrix(NA, nrow = p, ncol = L)
    lambda <- rep(NA, p) ## smoothing parameter
    for(r in 1:p){
      fit_smooth <- gam(betaTilde[r,] ~ s(argvals, bs = "cr", k = (nknots + 1)), method = "REML")
      betaHat[r,] <- fit_smooth$fitted.values
      lambda[r] <- fit_smooth$sp ## get smoothing parameter
    }
    sm <- smoothCon(s(argvals, bs = "cr", k = (nknots + 1)), data=data.frame(argvals=argvals), absorb.cons=TRUE)
    S <- sm[[1]]$S[[1]] ## penalty matrix
    B <- sm[[1]]$X ## basis functions
    
    rm(fit_smooth, sm)
  }else{
    betaHat <- t(apply(betaTilde, 1, function(x) gam(x ~ s(argvals, bs = "cr", k = (nknots + 1)), method = "REML")$fitted.values))
  }
  rownames(betaHat) <- rownames(betaTilde)
  colnames(betaHat) <- 1:L
  
  
  ##########################################################################################
  ## Step 3
  ##########################################################################################
  if(var == TRUE){
    if(analytic == TRUE){ ## analytic inference
      
      ##########################################################################################
      ## Analytic Inference
      ##########################################################################################
      if(silent == FALSE) print("Step 3: Analytic Inference")
      
      ################ Preparation
      if(silent == FALSE) print("Step 3.1: Preparation")
      
      if(silent == FALSE) print("Step 3.1.1: derive variance estimates of random components (\hat{H}(s), \hat{R}(s))")
      ## 1. derive variance estimates of random components (\hat{H}(s), \hat{R}(s))
      ### smooth raw estimates
      #HHat <- t(apply(sigmausqHat, 1, function(b) smooth.spline(x = argvals, y = b)$y))
      HHat= matrix(0,nrow=dim(sigmausqHat)[1],ncol= dim(sigmausqHat)[2])
      for(r in 1:dim(HHat)[1]){
        HHat[r,]=  smooth.spline(x=argvals,y= sigmausqHat[r,])$y
      }
      # print("--------------------------------------")
      # print(HHat)
      HHat[which(grepl("var", rownames(HHat)) == TRUE),][which(HHat[which(grepl("var", rownames(HHat)) == TRUE),] < 0)] <- 0
      # print("--------------------------------------")
      # print(HHat)
      RHat= matrix(0,nrow=dim(sigmaesqHat)[1],ncol= dim(sigmaesqHat)[2])
      for(r in 1:dim(RHat)[1]){
        RHat[r,]=  smooth.spline(x=argvals,y= sigmaesqHat[r,])$y
      }
      # RHat <- t(apply(sigmaesqHat, 1, function(b) smooth.spline(x = argvals, y = b)$y))
      RHat[which(RHat < 0)] <- 0
      
      if(silent == FALSE) print("Step 3.1.2: derive covariance estimates of random components (\hat{G}(s1, s2))")
      ## 2. derive covariance estimates of random components (\hat{G}(s1,s2))
      if(length(which(rownames(HHat) == "cov")) == 0){
        GTilde <- matrix(NA, nrow = L, ncol = L)
        for(i in 1:L){
          for(j in 1:L){
            GTilde[i,j] <- cov(data[,model_formula[2]][,i], data[,model_formula[2]][,j], use = "pairwise.complete.obs") -
              t(betaHat[,i]) %*% var(designmat) %*% betaHat[,j]
          }
        }
        diag(GTilde) <- HHat[1,]
        GHat <- fbps(GTilde)$Yhat ## fast bivariate smoother
        diag(GHat)[which(diag(GHat) < 0)] <- diag(GTilde)[which(diag(GHat) < 0)]
      }else{ ## fit a simple regression model between each of location pairs
        GTilde <- array(NA, dim = c(nrow(HHat), L, L))
        for(i in 1:L){
          for(j in i:L){
            data_cov <- data.frame(Y = (data[,model_formula[2]][,i] - designmat %*% matrix(betaHat[,i], ncol = 1)) *
                                     (data[,model_formula[2]][,j] - designmat %*% matrix(betaHat[,j], ncol = 1)),
                                   twoZ = 2 * data[,name_random], Zsq = data[,name_random]^2)
            fit_cov <- lm(Y ~ Zsq + twoZ, data = data_cov)
            GTilde[,i,j] <- GTilde[,j,i] <- fit_cov$coefficients
          }
        }
        rm(data_cov, fit_cov)
        GHat <- GTilde
        for(r in 1:nrow(HHat)){
          GHat[r,,] <- fbps(GTilde[r,,])$Yhat
        }
      }
      
      if(silent == FALSE) print("Step 3.1.3: derive variance estimates at each location (V(s))")
      ## 3. variance estimates at each location (V(s))
      obs.ind <- list()
      group <- massmm[[1]]$group ## group name in the data
      for(id in unique(data[,group])){
        obs.ind[[as.character(id)]] <- which(data[,group] == id) ## the corresponding rows of each subject
      }
      V.inv.all <- list()
      for(s in 1:L){
        V.subj.inv <- list()
        ## we first do inverse of each block matrix then combine them
        if(!length(which(rownames(HHat) == "cov")) == 0){
          cov.raw <- matrix(c(HHat[1,s], HHat[3,s], HHat[3,s], HHat[2,s]), nrow = 2, ncol = 2)
          edcomp <- eigen(cov.raw) ## trim non-positive eigenvalues to ensure positive semidefinite
          eigen.positive <- which(edcomp$values > 0)
          if(length(eigen.positive) == 2){
            cov.trimmed <- cov.raw
          }else{
            cov.trimmed <- matrix(edcomp$vectors[,1], ncol = 1) %*% edcomp$values[1] %*% matrix(edcomp$vectors[,1], nrow = 1)
          }
        }
        for(id in unique(data[,group])){
          if(length(which(rownames(HHat) == "cov")) == 0){
            Ji <- length(obs.ind[[as.character(id)]])
            V.subj <- matrix(1, nrow = Ji, ncol = 1) %*% HHat[1,s] %*% matrix(1, nrow = 1, ncol = Ji) + diag(RHat[s], Ji)
          }else{
            subj.ind <- obs.ind[[as.character(id)]]
            V.subj <- cbind(rep(1, length(subj.ind)), data[subj.ind, name_random]) %*% cov.trimmed %*%
              t(cbind(rep(1, length(subj.ind)), data[subj.ind, name_random])) + diag(RHat[s], length(subj.ind))
          }
          V.subj.inv[[as.character(id)]] <- solve(V.subj) ## calculate inverse of each block
        }
        V.inv.all[[s]] <- V.subj.inv
      }
      
      ## 4. covariance between any two pairs on the functional domain (V(s_1, s_2))
      if(silent == FALSE) print("Step 3.1.4: derive covariance between any two pairs on the functional domain (V(s_1, s_2))")
      V.cov.all <- list()
      for(i in 1:L){
        for(j in i:L){
          V.cov.subj <- list()
          if(!length(which(rownames(HHat) == "cov")) == 0){
            cov.raw <- matrix(c(GHat[1,i,j], GHat[3,i,j], GHat[3,i,j], GHat[2,i,j]), nrow = 2, ncol = 2)
            edcomp <- eigen(cov.raw)
            eigen.positive <- which(edcomp$values > 0)
            if(length(eigen.positive) == 2){
              cov.trimmed <- cov.raw
            }else{
              cov.trimmed <- matrix(edcomp$vectors[,1], ncol = 1) %*% edcomp$values[1] %*% matrix(edcomp$vectors[,1], nrow = 1)
            }
          }
          for(id in unique(data[,group])){
            if(length(which(rownames(HHat) == "cov")) == 0){
              Ji <- length(obs.ind[[as.character(id)]])
              V.cov.subj[[as.character(id)]] <- matrix(1, nrow = Ji, ncol = 1) %*% GHat[i,j] %*% matrix(1, nrow = 1, ncol = Ji)
            }else{
              subj.ind <- obs.ind[[as.character(id)]]
              V.cov.subj[[as.character(id)]] <- cbind(rep(1, length(subj.ind)), data[subj.ind, name_random]) %*% cov.trimmed %*%
                t(cbind(rep(1, length(subj.ind)), data[subj.ind, name_random]))
            }
          }
          V.cov.all[[L*(i-1)+j]] <- V.cov.subj
        }
      }
      rm(V.subj, V.subj.inv, V.cov.subj)
      
      ################ First step
      if(silent == FALSE) print("Step 3.2")

      if(silent == FALSE) print("Step 3.2.1: derive intra-location variance (Var(\Tilde{\beta}(s)))")
      ## 1. intra-location variance (Var(\Tilde{\beta}(s)))
      var.beta.tilde.theo <- array(NA, dim = c(p,p,L))
      for(s in 1:L){
        tmp <- matrix(0, nrow = p, ncol = p)
        for(id in unique(data[,group])){
          tmp <- tmp + t(matrix(designmat[obs.ind[[as.character(id)]],], ncol = p)) %*%
            V.inv.all[[s]][[as.character(id)]] %*% matrix(designmat[obs.ind[[as.character(id)]],], ncol = p)
        }
        var.beta.tilde.theo[,,s] <- solve(tmp)
      }
      
      if(silent == FALSE) print("Step 3.2.2: inter-location covariance (Cov(\Tilde{\beta}(s_1), \Tilde{\beta}(s_2)))")
      ## 2. inter-location covariance (Cov(\Tilde{\beta}(s_1), \Tilde{\beta}(s_2)))
      cov.beta.tilde.theo <- array(NA, dim = c(p,p,L,L))
      for(i in 1:L){
        for(j in i:L){
          tmp <- matrix(0, nrow = p, ncol = p)
          for(id in unique(data[,group])){
            tmp <- tmp + t(matrix(designmat[obs.ind[[as.character(id)]],], ncol = p)) %*% V.inv.all[[i]][[as.character(id)]] %*%
              V.cov.all[[L*(i-1)+j]][[as.character(id)]] %*% V.inv.all[[j]][[as.character(id)]] %*% matrix(designmat[obs.ind[[as.character(id)]],], ncol = p)
          }
          cov.beta.tilde.theo[,,i,j] <- var.beta.tilde.theo[,,i] %*% tmp %*% var.beta.tilde.theo[,,j]
        }
      }
      rm(V.cov.all, V.inv.all)
      
      ################ Second step
      if(silent == FALSE) print("Step 3.3: Second step")
      
      if(silent == FALSE) print("Step 3.3.1: intermediate step for covariance estimate")
      ## 1. intermediate step for covariance estimate
      var.beta.tilde.s <- array(NA, dim = c(L,L,p))
      for(j in 1:p){
        for(r in 1:L){
          for(t in 1:L){
            if(t == r){
              var.beta.tilde.s[r,t,j] <- var.beta.tilde.theo[j,j,r]
            }else{
              var.beta.tilde.s[r,t,j] <- cov.beta.tilde.theo[j,j,min(r,t),max(r,t)]
            }
          }
        }
      }
      
      if(silent == FALSE) print("Step 3.3.2: final variance estimate of betaHat (Cov(\hat{\beta}(s_1), \hat{\beta}(s_2)))")
      ## 2. final variance estimate of betaHat (Cov(\hat{\beta}(s_1), \hat{\beta}(s_2)))
      var.beta.hat <- array(NA, dim = c(L,L,p))
      for(r in 1:p){
        M <- B %*% solve(t(B)%*%B + lambda[r]*S) %*% t(B) + matrix(1/L, nrow = L, ncol = L)
        var.raw <- M %*% var.beta.tilde.s[,,r] %*% t(M)
        ## trim eigenvalues to make final variance matrix positive-semidefinite
        edcomp <- eigen(var.raw)
        eigen.positive <- which(edcomp$values > 0)
        var.beta.hat[,,r] <- edcomp$vectors[,eigen.positive] %*% diag(edcomp$values[eigen.positive]) %*% t(edcomp$vectors[,eigen.positive])
      }
      betaHat.var <- var.beta.hat ## final variance estimate
      
      ## obtain qn to construct joint CI
      qn <- rep(0, length = nrow(betaHat))
      N <- 10000 ## sample size in simulation-based approach
      for(i in 1:length(qn)){
        Sigma <- betaHat.var[,,i]
        x_sample <- rmvnorm(N, mean = betaHat[i,], sigma = Sigma)
        un <- rep(NA, N) 
        for(j in 1:N){
          un[j] <- max(abs((x_sample[j,] - betaHat[i,])/sqrt(diag(Sigma))))
        }
        qn[i] <- quantile(un, 0.95)
      }
      
      return(list(betaHat = betaHat, betaHat.var = betaHat.var, qn = qn))
      
    }else{ ## bootstrap inference
      
      ##########################################################################################
      ## Bootstrap Inference
      ##########################################################################################
      if(silent == FALSE) print("Step 3: Bootstrap Inference")
      
      # B <- 100
      betaHat_boot <- array(NA, dim = c(nrow(betaHat), ncol(betaHat), B_boot))
      group <- massmm[[1]]$group
      ID.number <- unique(data[,group])
      pb <- progress_bar$new(total = B_boot)
      for(boots in 1:B_boot){
        pb$tick()
        sample.ind <- sample(1:length(ID.number), size = length(ID.number), replace = TRUE)
        dat.ind <- c()
        for(i in 1:length(ID.number)){
          dat.ind <- c(dat.ind, which(data[,group] == ID.number[sample.ind[i]])) ## subject-level bootstrap
        }
        fit_boot <- lfosr3s(formula = formula, data = data[dat.ind,], family = family, var = TRUE, 
                            parallel = parallel, silent = TRUE)
        betaHat_boot[,,boots] <- fit_boot$betaHat
      }
      
      ## obtain bootstrap variance
      betaHat.var <- array(NA, dim = c(L,L,nrow(betaHat)))
      for(r in 1:nrow(betaHat)){
        betaHat.var[,,r] <- 1.2*var(t(betaHat_boot[r,,])) ## account for within-subject correlation
      }
      
      ## obtain qn to construct joint CI using the fast approach
      qn <- rep(0, length = nrow(betaHat))
      N <- length(unique(data$V1)) ## sample size in simulation-based approach
      for(i in 1:length(qn)){
        est_bs <- t(betaHat_boot[i,,])
        fit_fpca <- fpca.face(est_bs)
        ## extract estimated eigenfunctions/eigenvalues
        phi <- fit_fpca$efunctions
        lambda <- fit_fpca$evalues
        K <- length(fit_fpca$evalues)
        ## simulate random coefficients 
        theta <- matrix(rnorm(N*K), nrow=N, ncol=K) # generate independent standard normals 
        theta <- theta %*% diag(sqrt(lambda)) # scale to have appropriate variance
        X_new <- theta %*% t(phi) # simulate new functions
        x_sample <- X_new + t(fit_fpca$mu %o% rep(1,N)) # add back in the mean function
        Sigma <- apply(x_sample, 2, var)
        x_mean <- colMeans(est_bs)
        un <- rep(NA, N) 
        for(j in 1:N){
          un[j] <- max(abs((x_sample[j,] - x_mean)/sqrt(Sigma)))
        }
        qn[i] <- quantile(un, 0.95)
      }
      
      return(list(betaHat = betaHat, betaHat.var = betaHat.var, qn = qn))
    }
    
  }else{
    return(list(betaHat = betaHat))
  }
  
}
