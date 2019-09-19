



svyMVdecomp <- function(fit.model, classif.col){
  
  ## Start timer
  
  start.time <- proc.time()
  
  # Test if classif.col is a text string
  if ((is.character(classif.col) & length(classif.col) == 1))
    convert = TRUE else
      convert = FALSE
    
    ## Convert classification column input to string 
    
    if (convert==FALSE) {classif.col <- deparse(substitute(classif.col))}
    if (convert==FALSE) {cclassif.col <- gsub("\"", "", classif.col)}
    

  ## Initial error checking 
  
  accepted.links <- c("logit", "probit", "cauchit", "cloglog", "identity", "log", "sqrt", "1/mu^2", "inverse")
  
  if(!("svyglm" %in% class(fit.model)))                              stop("This function only supports svyglm class models")
  if(!("data" %in% names(fit.model)))                                stop("This function requires you supply svyglm with a dataset")
#  if(!(classif.col %in% names(fit.model$data)))                      stop("Cannot find group classifier column in data")
  if(!(length(unique(na.omit(fit.model$data[, classif.col]))) == 2)) stop("MV decomposition only supports two group decomosition")
  if(!(fit.model$call$family$link %in% accepted.links))              stop(paste("This function currently only supports", paste(accepted.links, collapse = ", ")))
  
  
  
  
  ## Define independent var (X)  matrix, coeficients (betas), call terms, weights and input data (in.df)
  
  X          <- fit.model$x
  betas      <- fit.model$coefficients
  call.terms <- names(fit.model$model)[names(fit.model$model) %in% names(fit.model$data)]
  weight     <- fit.model$prior.weights
  in.df      <- fit.model$data[complete.cases(fit.model$data[, call.terms]), ]
  
  
  
  
  ## Define inverse link function
  
  g <- make.link(fit.model$call$family$link)$linkinv
  
  ## Define the groups
  if(is.factor(in.df[, classif.col])){
    classif.groups <- levels(in.df[, classif.col])
    
  } else {
    
    classif.groups <- unique(in.df[, classif.col])
    
  }
    
  #classif.groups <- unique(in.df[, classif.col])
  
  indx.g1 <- which(in.df[, classif.col] == classif.groups[1])
  indx.g2 <- which(in.df[, classif.col] == classif.groups[2])
  
 # classif.col <- deparse(substitute(Gender))
  #classif.col <- gsub("\"", "", classif.col)
  #classif.groups <- levels(LSAY_2009_data[, classif.col])
  #indx.g1 <- which(LSAY_2009_data[, classif.col] == classif.groups[1])
  #indx.g2 <- which(LSAY_2009_data[, classif.col] == classif.groups[2])
  #LSAY_2009_data$Gender
  
  #classif.col <- deparse(substitute(Gender))
  #classif.col <- gsub("\"", "", classif.col)
  #classif.groups <- levels(LSAY_2006_data[, classif.col])
 # indx.g1 <- which(LSAY_2006_data[, classif.col] == classif.groups[1])
 # indx.g2 <- which(LSAY_2006_data[, classif.col] == classif.groups[2])
  #LSAY_2006_data$Gender
  
  
  
  ## Change the classification column to a character vector
  
  if(is.factor(in.df[, classif.col])){
    
    in.df[, classif.col] <- as.character(levels(in.df[, classif.col]))[in.df[, classif.col]]
    
  } else {
    
    in.df[, classif.col] <- as.character(in.df[, classif.col])
    
  }
  
  
  
  
  ## Define group numeric index vectors and cartesian product of the groups
  

  
  groups <- expand.grid(
    g1 = indx.g1, 
    g2 = indx.g2
  )
  
  weight.i   <- sum(weight[indx.g1])
  weight.j   <- sum(weight[indx.g2])
  weight.i.j <- sum(weight[groups$g1] * weight[groups$g2])
  
  
  
  
  ## Define weighted Y and fit Y function
  
  wgt.mean.y <- function(indx){
    sum(
      fit.model$y[indx] * weight[indx] / sum(weight[indx])
      )
  }
  
  wgt.mean.fit.y <- function(indx){
    sum(
      fit.model$fitted.values[indx] * weight[indx] / sum(weight[indx])
    )
  }
  
  
  
  
  ## Create summary differences in means
  
  sumry.diff <- data.frame(
    classif.groups,
    sapply(list(indx.g1, indx.g2), wgt.mean.y),
    sapply(list(indx.g1, indx.g2), wgt.mean.fit.y),
    stringsAsFactors = F
  )
  
  sumry.diff <- rbind(
    sumry.diff,
    c("Difference", 
      sumry.diff[1, 2] - sumry.diff[2, 2], 
      sumry.diff[1, 3] - sumry.diff[2, 3]
    )
  )
  
  names(sumry.diff) <- c(classif.col, "mean.y", "mean.fit.y")
  
  
  
  
  ## Define Schwiebert (2015) formula 14 
  
  form.14 <- function(i, j){
    
    (g(X[i,] %*% betas) - g(X[j,] %*% betas)) /
      ((X[i,] - X[j,]) %*% betas)
    
  }
  
  
  
  
  ## Define Schwiebert (2015) formula 13 
  
  form.13 <- function(k){
    
    
    ## Define equation within sum operators in formula 13
    
    f.13.summed.terms <-function(i, j, k, result.14){
      
      result.14 * weight[i] * weight[j] * ((X[i, k] - X[j, k]) %*% betas[k])
      
    } 
    
    
    
    
    ## Execute the equation over summed sets
    
    sum.terms <- mapply(
      FUN       = f.13.summed.terms, 
      i         = groups$g1, 
      j         = groups$g2, 
      k         = rep(k, nrow(groups)), 
      result.14 = results.14
    )
    
    
    
    ## Add summed terms and multiply by 1/(n_group1*n_group2)
    
    (1 / weight.i.j) * sum(sum.terms)
    
  }
  
  
  
  ## Execute calculation of formula 14
  
  results.14 <- sapply(1:nrow(groups), function(x) form.14(groups$g1[x], groups$g2[x]))
  
  
  
  
  ## Evaluate formula 13
  
  results.13 <- sapply(names(betas), form.13)
  
  
  
  
  ## Evaluate time elapsed and return results
  
  end.time <- proc.time() - start.time
  
  
  return(list(
    performance   = paste0("elapsed: ", end.time[3], "S"),
    group.means   = sumry.diff,
    decomposition = results.13
  ))
  
}

##### Additional functions to flip decomps

flip.decomp <- function(decomp){
  
  new.group.means     <- rbind(decomp$group.means[2,], decomp$group.means[1,])
  new.group.means[3,] <- list("Difference", new.group.means[1, 2] - new.group.means[2, 2], new.group.means[1, 3] - new.group.means[2, 3])
  
  new.decomposition   <- -1 * decomp$decomposition
  
  return(list(
    performace    = NA,
    group.means   = new.group.means,
    decomposition = new.decomposition
  ))
  
}

## This function only flips if the difference is negative

if.neg.flip.decomp <- function(decomp){
  
  if(decomp$group.means[3,3] < 0){
    
    new.group.means     <- rbind(decomp$group.means[2,], decomp$group.means[1,])
    new.group.means[3,] <- list("Difference", new.group.means[1, 2] - new.group.means[2, 2], new.group.means[1, 3] - new.group.means[2, 3])
    
    new.decomposition   <- -1 * decomp$decomposition
    
    return(list(
      performace    = NA,
      group.means   = new.group.means,
      decomposition = new.decomposition
    ))
    
  } else {
    
    return(decomp)
    
  }
  
  
}






