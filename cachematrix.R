##
## This module provides functions to calculate and cache inversion of given
## matrix. Caching allows to use caculated matrix repeatedly (e.g. inside
## cycle) without need to calculate inversion every time again.
##
## Written by Richard Sobota as part of assignment 2 in "R Programming" course.
##


## Creates object which holds matrix and its inverted value. Matrix should
## be given as parameter, it will be stored for later use. Inverted value is
## calculated and stored by function cacheSolve.
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }

    setsolve <- function(solved) {
        m <<- solved
    }

    getsolve <- function() {
        m
    }

    list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns a matrix that is the inverse of previously set matrix. If there is
## already calculated value, is it returned without recalculating.
## Cache matrix created by function makeCacheMatrix should be
## given as parameter.
##
cacheSolve <- function(x, ...) {
    ## try to get calculated value
    m <- x$getsolve()
    
    ## test if calculated value has been read
    if(!is.null(m)) {
        message("Getting cached inverted matrix")

        ## inverted matrix already calculated, just return it
        return(m)
    }
    
    ## get matrix to be inverted
    data <- x$get()
    
    ## calculate inverted matrix
    m <- solve(data, ...)
    
    ## store calclated value
    x$setsolve(m)
    
    m
}
