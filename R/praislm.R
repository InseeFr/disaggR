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

autocor <- function(x) {
  x_center <- x-mean(x)
  tailxc <- x_center[-1]
  headxc <- x_center[-length(x_center)]
  rho <- as.numeric(crossprod(tailxc,headxc)/
                      sqrt(crossprod(tailxc)) /
                      sqrt(crossprod(headxc)))
    # Not exactly pearson but is a bit better to estimate the rho of an AR1
}

praislm_impl <- function(X,y,include.rho) {
  rho <- 0
  df_residual <- nrow(X) - ncol(X)
  
  if (ncol(X) != 0) {
    if (any(is.na(X))) stop("The high frequency serie must have values in the full coefficients calculation window", call. = FALSE)
    PQR <- qr(X)
    if (PQR$rank != ncol(X))  stop("The regressed series should have a perfect rank", call. = FALSE)
    
    coefficients <- qr.coef(PQR,y)
    
    if (include.rho) {
      rho_prec <- 0
      rho <- 1
      i <- 1L
      
      while (abs(rho-rho_prec)>0.001) {
        rho_prec <- rho
        rho <- autocor(y - X %*% coefficients)
        
        y_star <- omega_inv_sqrt(y,rho)
        X_star <- omega_inv_sqrt(X,rho)
        
        PQR <- qr(X_star)
        coefficients <- qr.coef(PQR,y_star)
        if (i == 50L) {
          warning("Maximum iterations without convergence")
          break
        }
        i <- i+1
      }
    }
    fitted <- ts(X %*% coefficients,start=start(X),frequency = frequency(X))
    residuals <- y-fitted
    residuals_decor <- omega_inv_sqrt(residuals,rho)
    resvar <- sum(residuals_decor^2) / df_residual
    R <- chol2inv(PQR$qr[1:ncol(X), 1:ncol(X), drop = FALSE])
    se <- sqrt(diag(R) * resvar)
  }
  else {
    coefficients <- numeric()
    fitted <- ts(rep(0,length(y)),start=start(y),frequency=frequency(y))
    residuals <- y
    residuals_decor <- y
    se <- numeric()
  }
  return(list(coefficients = coefficients,
              residuals = residuals,
              fitted=fitted,
              df.residual=df_residual,
              se=se,
              rho=rho,
              residuals.decorrelated=residuals_decor))
}

praislm <- function(X,y,include.rho,include.differenciation,set_coefficients,cl) {
  modellist <- list(X=X,
                    y=y,
                    include.rho=include.rho,
                    include.differenciation=include.differenciation,
                    set.coefficients=set_coefficients)
  if ( !is.ts(X) || !is.ts(y) ) stop("Not a ts object", call. = FALSE)
  if (is.null(dim(X))) stop("Not a matrix object", call. = FALSE)
  
  if (include.differenciation) {
    X <- diff(X)
    y <- diff(y)
  }

  if (length(set_coefficients)==0) names(set_coefficients) <- character()
  else if (is.null(names(set_coefficients))) stop("The coefficients setter must be empty or have names", call. = FALSE)
  
  coefficients <- rep(NA_real_,ncol(X))
  names(coefficients) <- colnames(X)
  se <- rep(NA_real_,ncol(X))
  names(se) <- colnames(X)
  
  match_set <- match(names(set_coefficients),colnames(X))
  if (any(is.na(set_coefficients))) stop("A coefficient can't be set to NA", call. = FALSE)
  if (any(is.na(match_set))) stop("The names of the set coefficients must be a column name of hfserie", call. = FALSE)
  coefficients[match_set] <- set_coefficients
  match_notset <- which(is.na(coefficients))
  
  offset <- ts(X[,match_set,drop=FALSE] %*% set_coefficients,start=start(X),frequency = frequency(X))

  calculated <- praislm_impl(X[,match_notset,drop=FALSE],y-offset,include.rho)
  
  coefficients[match_notset] <- calculated$coefficients
  
  se[match_notset] <- calculated$se
  
  fitted <- calculated$fitted + offset
  
  res <- list(coefficients=coefficients,
              residuals=drop(calculated$residuals),
              fitted.values=drop(fitted),
              se=se,
              df.residual=calculated$df.residual,
              rho=calculated$rho,
              residuals.decorrelated=drop(calculated$residuals.decor),
              fitted.values.decorrelated=drop(omega_inv_sqrt(fitted,calculated$rho)),
              model.list=modellist,
              call=cl)
  class(res) <- "praislm"
  return(res)
}