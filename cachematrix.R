## Together, the makeCacheMatrix and cacheSolve functions cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse as NULL
    inverse <- NULL
    
    ## Function for setting the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## Function for retrieving the matrix
    get <- function() x
    
    ## Function for setting the inverse
    setInverse <- function(i) inverse <<- i
    
    ## Function for retrieving the inverse
    getInverse <- function() inverse
    
    ## Return list containing the 4 above functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    
    ## Retrieve inverse from cache
    inverse <- x$getInverse()
    
    ## Check if inverse is not null
    if (!is.null(inverse)) {
        ## Return cached inverse
        return(inverse)
    }
    
    ## Retrieve matrix from cache
    data <- x$get()
    
    ## Compute the inverse of cache matrix
    inverse <- solve(data, ...)
    
    ## Cache the inverse
    x$setInverse(inverse)
    
    ## Return the inverse
    inverse
}