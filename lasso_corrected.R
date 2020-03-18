LASSOCORRECTED <- function (X, y, var, group, lambda = NULL, weight = NULL, loss = c("ls", 
                                                                                   "logit"), intercept = TRUE, ...) 
{
    library("MLGL")
    CHECKOVERLAP(X, y, var, group, lambda, weight, intercept)
    loss <- match.arg(loss)
    ord <- order(group)
    groupord <- group[ord]
    varord <- var[ord]
    groupb <- cumsum(!duplicated(groupord))
    Xb <- X[, varord]
    if (is.null(weight)) 
        weight = as.numeric(sqrt(table(groupb)))
    t1 <- proc.time()
    res <- gglasso(Xb, y, groupb, pf = weight, lambda = lambda, 
                   intercept = intercept, loss = loss, ...)
    t2 <- proc.time()
    res2 <- list()
    res2$lambda = res$lambda
    non0 = apply(res$beta, 2, FUN = function(x) {
        which(x != 0)
    })
    res2$var = lapply(non0, FUN = function(x) {
        varord[x]
    })
    res2$nVar = sapply(res2$var, FUN = function(x) {
        length(unique(x))
    })
    res2$group = lapply(non0, FUN = function(x) {
        groupb[x]
    })
    res2$nGroup = sapply(res2$group, FUN = function(x) {
        length(unique(x))
    })
    res2$beta = lapply(1:length(res$lambda), FUN = function(x) {
        res$beta[non0[[x]], x]
    })
    res2$b0 = res$b0
    res2$structure = list(group = groupb, var = varord, weight = weight)
    res2$dim = dim(X)
    res2$time = (t2 - t1)[3]
    class(res2) = "MLGL"
    return(res2)
}

CHECKOVERLAP <- function (X, y, var, group, lambda = NULL, weight = NULL, intercept = TRUE) 
{
    library("MLGL")
    if (!is.matrix(X)) 
        stop("X has to be a matrix.")
    if (any(is.na(X))) 
        stop("Missing values in X not allowed.")
    if (!is.numeric(X)) 
        stop("X has to be a matrix of real.")
    if (!is.numeric(y)) 
        stop("y has to be a matrix of real.")
    if (any(is.na(y))) 
        stop("Missing values in y not allowed.")
    if (nrow(X) != length(drop(y))) 
        stop("The length of y and the number of rows of X don't match.")
    if (!is.null(var)) {
        if (!is.numeric(var)) 
            stop("var must be a vector of positive integer.")
        if (!all(ISWHOLENUMBER(var))) 
            stop("var must be a vector of positive integer.")
        if (any(var < 0)) 
            stop("var must be a vector of positive integer.")
        if (any(var > ncol(X))) 
            stop("An element in var is greater that the number of variables")
    }
    if (!is.null(group)) {
        if (!is.numeric(group)) 
            stop("group must be a vector of positive integer.")
        if (!all(ISWHOLENUMBER(group))) 
            stop("group must be a vector of positive integer.")
        if (any(group < 0)) 
            stop("group must be a vector of positive integer.")
        if (length(var) != length(group)) 
            stop("var and group must have the same size.")
    }
    if (!is.null(lambda)) {
        if (!is.numeric(lambda)) 
            stop("lambda must be a vector of positive real.")
        if (any(lambda < 0)) 
            stop("lambda must be a vector of positive real.")
    }
    if (!is.null(weight)) {
        if (!is.numeric(weight)) 
            stop("weight must be a vector of positive real.")
        if (length(weight) != sum(!duplicated(group))) 
            stop("weight must have the same length as the number of differents groups.")
        if (any(weight < 0)) 
            stop("weight must be a vector of positive real.")
    }
    if (length(intercept) != 1) 
        stop("intercept must be a boolean.")
    if (!is.logical(intercept)) 
        stop("intercept must be a boolean.")
    invisible(return(NULL))
}

ISWHOLENUMBER<- function (x, tol = .Machine$double.eps^0.5) 
{
    abs(x - round(x)) < tol
}
