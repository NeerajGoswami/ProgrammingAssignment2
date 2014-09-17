## Creates a list of functions to set, get, set the inverse and
## get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(inverse=matrix()) inv <<- inverse
    getinv <- function() inv 
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinv = setinv,
         getinv = getinv)
}


## Checks and returns inverse of matrix if it exists in cache.
## Otherwise, calculates and returns inverse.

cacheSolve <- function(x=matrix(),...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data)
    x$setinv(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
}