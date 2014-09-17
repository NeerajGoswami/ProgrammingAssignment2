## Creates a list of functions to set, get, set the inverse and
## get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## default value of mean set to NULL
    
    ## set matrix for which mean is to
    ## be found. Default matrix is the one created when
    ## makeCacheMatrix is called, but that can be changed using
    ## this function
    setmatrix <- function(y) {  
        x <<- y                 ## scope for x extended, so that
                                ## value is not lost on exit
                                ## from setmatrix
        inv <<- NULL    
    }
    
    getmatrix <- function() x   ## display the matrix in use
    
    setinv <- function(inverse=matrix()) inv <<- inverse
    ## scope for inv extended a level beyond function
    
    getinv <- function() inv    ## display the value of inv in use
    
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinv = setinv,
         getinv = getinv)       ## list assigned to variable
}


## Checks and returns inverse of matrix if it exists in cache.
## Otherwise, calculates and returns inverse.

cacheSolve <- function(x=matrix(),...) {
    inv <- x$getinv()           ## get current value of 'inv' 
    
    if(!is.null(inv)) {         ## If inverse exists, fetch from 
                                ## memory
        message("getting cached data")
        return(inv)
        
    }                           ## otherwise, calculate inverse
    data <- x$getmatrix()
    inv <- solve(data)
    x$setinv(inv)               ## set inv as calculated
    inv
    ## Return a matrix that is the inverse of 'x'
}