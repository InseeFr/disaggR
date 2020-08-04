# this function multiplies by omega_inv_sqrt without
# having to create the matrix (it is helpful if there are a lot of obs)
omega_inv_sqrt <- function(x,rho) {
  if (is.null(dim(x))) {
    res <- c(sqrt(1-rho^2)*x[1],
             x[-1]-rho*x[-length(x)])
  }
  else {
    res <- rbind(sqrt(1-rho^2)*x[1,],
                 x[-1,,drop=FALSE]-rho*x[-nrow(x),,drop=FALSE])
  }
  res <- ts(res,start=start(x),frequency=frequency(x))
  return(res)
}



praislm <- function(X,y,includerho,includedifferenciation,set_coefficients,cl) {
  modellist <- list(X=X,
                    y=y,
                    include.rho=includerho,
                    include.differenciation=includedifferenciation,
                    set.coefficients=set_coefficients)
  if ( !is.ts(X) || !is.ts(y) ) stop("Not a ts object")
  if (is.null(dim(X))) stop("Not a matrix object")
  if (any(is.na(X))) stop("The high frequency serie must have values in the full coefficients calculation window")
  if (includedifferenciation) {
    X <- diff(X)
    y <- diff(y)
  }

  if (length(set_coefficients)==0) names(set_coefficients) <- character()
  else if (is.null(names(set_coefficients))) stop("The coefficient setter must be empty or have names")
  
  coefficients <- rep(NA_real_,ncol(X))
  names(coefficients) <- colnames(X)
  match_set <- match(names(set_coefficients),colnames(X))
  if (any(is.na(set_coefficients))) stop("A coefficient can't be set to NA")
  if (any(is.na(match_set))) stop("The names of the set coefficients must be a column name of hfserie")
  coefficients[match_set] <- set_coefficients
  match_notset <- which(is.na(coefficients))
  
  offset <- ts(X[,match_set,drop=FALSE] %*% set_coefficients,start=start(X),frequency = frequency(X))
  
  y <- y-offset
  X <- X[,match_notset,drop=FALSE]

  rho <- 0
  df_residual <- nrow(X) - ncol(X)
  
  if (ncol(X) != 0) {
    PQR <- qr(X)
    if (PQR$rank !=ncol(X))  stop("The regressed series should have a perfect rank")
    
    calculated_coefficients <- qr.coef(PQR,y)
  
    if (includerho) {
      drho <- 1
      rho_prec <- 0
      i_max = 50L
      i <- 1L
      
      while (drho>0.001 && i<i_max) {
        residuals <- y - X %*% calculated_coefficients
        residuals_centered <- residuals-mean(residuals)
        tailresc <- residuals_centered[-1]
        headresc <- residuals_centered[-length(residuals_centered)]
        rho <- as.numeric(crossprod(tailresc,headresc)/
                            sqrt(crossprod(tailresc)) /
                            sqrt(crossprod(headresc)))
        drho <- abs(rho-rho_prec)
        rho_prec <- rho
        
        y_star <- omega_inv_sqrt(y,rho)
        X_star <- omega_inv_sqrt(X,rho)
        
        PQR <- qr(X_star)
        calculated_coefficients <- qr.coef(PQR,y_star)
      }
      if (i >= i_max) warning("Maximum iterations without convergence")
      i <- i+1
    }
    fitted <- ts(X %*% calculated_coefficients + offset,start=start(X),frequency = frequency(X))
    residuals <- y-fitted + offset
    residuals_decor <- omega_inv_sqrt(residuals,rho)
    resvar <- sum(residuals_decor^2) / df_residual
    R <- chol2inv(PQR$qr[1:ncol(X), 1:ncol(X), drop = FALSE])
    calculated_se <- sqrt(diag(R) * resvar)
  }
  else {
    calculated_coefficients <- numeric()
    fitted <- offset
    residuals <- y
    residuals_decor <- y
    calculated_se <- numeric()
  }
  
  se <- rep(NA_real_,length(coefficients))
  names(se) <- names(coefficients)
  se[match_notset] <- calculated_se
  coefficients[match_notset] <- calculated_coefficients
  
  res <- list(coefficients=coefficients,
              residuals=drop(residuals),
              fitted.values=drop(fitted),
              se=se,
              df.residual=df_residual,
              rho=rho,
              residuals.decorrelated=drop(residuals_decor),
              fitted.values.decorrelated=drop(omega_inv_sqrt(fitted,rho)),
              model.list=modellist,
              call=cl)
  class(res) <- "praislm"
  return(res)
}