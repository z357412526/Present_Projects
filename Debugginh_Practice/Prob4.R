require(pryr)
set.seed(0)
y=rnorm(1000000)
x1=rnorm(1000000)
x2=rnorm(1000000)
x3=rnorm(1000000)


lm_my<-function (formula, data, subset, weights, na.action, method = "qr", 
                 model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
                 contrasts = NULL, offset, ...) 
{
  #1
  very_beginning = mem_used()
  cat("1:Memory used at the beginning of the function"
      , very_beginning/1000000, "MB","\n")
  
  ret.x <- x
  ret.y <- y
  #2
  node2 = mem_used()
  cat("2:", (node2-very_beginning)/1000000, "MB","\n")
  
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  cat("mf_size:", object_size(mf)/1000000, "MB", "\n") 
  #get the obj_size of mf
  
  #3
  node3 = mem_used()
  cat("3: memory used after creation of several objs:"
      ,(node3-node2)/1000000, "MB", "\n")
  
  if (method == "model.frame") 
    return(mf)
  else if (method != "qr") 
    warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                     method), domain = NA)
  
  #4
  node4 = mem_used()
  #print("4:"); print(node4);print(node4-node3)
  print("check 4--5")
  mt <- attr(mf, "terms"); #print(object_size(mt))
  y <- model.response(mf, "numeric")
  cat("y size:",object_size(y)/1000000, "MB", "\n")
  print("get more info of y to diagnose:")
  print(head(.Internal(inspect(y))))
  w <- as.vector(model.weights(mf)); #print(object_size(w))
  
  #5
  node5 = mem_used()
  cat("5: after the creating of mt, y, w objects:"
      , (node5-node4)/1000000, "MB", "\n")
  
  if (!is.null(w) && !is.numeric(w)) 
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  
  
  if (!is.null(offset)) {
    if (length(offset) != NROW(y)) 
      stop(
        gettextf("number of offsets is %d, should equal %d (number of observations)"
                 ,length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(
      coefficients = if (is.matrix(y)) matrix(, 0, 3) 
      else numeric(), residuals = y, fitted.values = 0 * y
      , weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 0) 
      else if (is.matrix(y)) nrow(y) 
      else length(y))
    
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
    #print(head(.Internal(inspect(z))))
  }
  else {
    #5.1
    
    x <- model.matrix(mt, mf, contrasts)
    node5.1 = mem_used()
    cat("5.1: the memory usage for creating object x"
        ,(node5.1-node5)/1000000, "MB", "\n")
    cat("x size:",object_size(x)/1000000, "MB", "\n")
    #
    print("get more info of x to diagnose:")
    print(head(.Internal(inspect(x))))
    
    cat("Memory usage before fitting model"
        ,(node5.1-very_beginning)/1000000, "MB", "\n")
    z <- if (is.null(w)) {
      #mem_fit = mem_used()
      lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
             ...)
      #mem_fit <- mem_used()
    }
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                 ...)
    print("PART A: after fitting model and creating z")
    cat("z size:",object_size(z)/1000000,"MB","\n")
    cat("memory used for creating z:"
        ,(mem_used()-node5.1)/1000000, "MB", "\n")
  }
  #6
  node6 = mem_used()
  cat("6: after fit the model"
      ,(node6-node5)/1000000,"MB","\n")
  
  class(z) <- c(if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  if (!qr) 
    z$qr <- NULL
  #7
  node7 = mem_used()
  cat("7: the rest of steps", (node7-node6)/1000000, "MB", "\n")
  z
}
ret = lm_my(y~x1+x2+x3)

#c
lm_revised<-function (formula, data, subset, weights, na.action, method = "qr", 
                      model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
                      contrasts = NULL, offset, ...) 
{
  #1
  very_beginning = mem_used()
  cat("1:Memory used at the beginning of the function"
      , very_beginning/1000000, "MB","\n")
  
  ret.x <- x
  ret.y <- y
  #2
  node2 = mem_used()
  cat("2:", (node2-very_beginning)/1000000, "MB","\n")
  
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  
  cat("mf_size:", object_size(mf)/1000000, "MB", "\n") 
  #get the obj_size of mf
  
  #3
  node3 = mem_used()
  cat("3: memory used after creation of several objs:"
      ,(node3-node2)/1000000, "MB", "\n")
  
  if (method == "model.frame") 
    return(mf)
  else if (method != "qr") 
    warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                     method), domain = NA)
  
  #4
  node4 = mem_used()
  #print("4:"); print(node4);print(node4-node3)
  print("check 4--5")
  mt <- attr(mf, "terms"); #print(object_size(mt))
  y <- model.response(mf, "numeric")
  attributes(y)$names =NULL
  cat("y size:",object_size(y)/1000000, "MB", "\n")
  print("get more info of y to diagnose:")
  print(head(.Internal(inspect(y))))
  w <- as.vector(model.weights(mf)); #print(object_size(w))
  
  #5
  node5 = mem_used()
  cat("5: after the creating of mt, y, w objects:"
      , (node5-node4)/1000000, "MB", "\n")
  
  if (!is.null(w) && !is.numeric(w)) 
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  
  
  if (!is.null(offset)) {
    if (length(offset) != NROW(y)) 
      stop(gettextf(
        "number of offsets is %d, should equal %d (number of observations)", 
        length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(
      coefficients = if (is.matrix(y)) matrix(, 0, 3) 
      else numeric(), residuals = y, fitted.values = 0 * y
      ,weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 0) 
      else if (is.matrix(y)) nrow(y) 
      else length(y))
    
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
    #print(head(.Internal(inspect(z))))
  }
  else {
    #5.1
    
    x <- model.matrix(mt, mf, contrasts)
    attributes(x)$dimnames=NULL
    node5.1 = mem_used()
    cat("5.1: the memory usage for creating object x"
        ,(node5.1-node5)/1000000, "MB", "\n")
    cat("x size:",object_size(x)/1000000, "MB", "\n")
    
    #
    print("get more info of x to diagnose:")
    print(head(.Internal(inspect(x))))
    cat("Memory usage before fitting model"
        ,(node5.1-very_beginning)/1000000, "MB", "\n")
    z <- if (is.null(w)) {
      #mem_fit = mem_used()
      lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
             ...)
      #mem_fit <- mem_used()
    }
    else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                 ...)
    print("PART A: after fitting model and creating z")
    cat("z size:",object_size(z)/1000000,"MB","\n")
    cat("memory used for creating z:"
        ,(mem_used()-node5.1)/1000000, "MB", "\n")
  }
  #6
  node6 = mem_used()
  cat("6: after fit the model"
      ,(node6-node5)/1000000,"MB","\n")
  
  class(z) <- c(if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) 
    z$model <- mf
  if (ret.x) 
    z$x <- x
  if (ret.y) 
    z$y <- y
  if (!qr) 
    z$qr <- NULL
  #7
  node7 = mem_used()
  cat("7: the rest of steps"
      , (node7-node6)/1000000, "MB", "\n")
  z
}
ret = lm_revised(y~x1+x2+x3)