## Function: makeCacheMatrix
## Description: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the inverse matrix cache
    inverse <- NULL
    
    ## Function to set the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## Function to get the matrix
    get <- function() x
    
    ## Function to get the cached inverse
    getInverse <- function() inverse
    
    ## Function to set the inverse and cache it
    setInverse <- function(inverseMatrix) {
        inverse <<- inverseMatrix
    }
    
    ## Return a list of functions
    list(set = set,
         get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}

## Function: cacheSolve
## Description: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Retrieve the cached inverse if available
    inverse <- x$getInverse()
    
    ## If the inverse is cached, return it
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    
    ## If the inverse is not cached, compute it and cache it
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}

