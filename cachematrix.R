## ==============================================================================
## This file contains two functions that can serve as utilities for computing
## the inverse of matrix that can save time when there is opportunity to store
## an already computed inverse in the cache
## ==============================================================================

## ==============================================================================
## Function: makeCacheMatrix
## This function takes in a matrix as argument and provides the basic get and set
## functions to retrieve and store matrices, as well as the getter and setter of 
## of the inverse of the same matrix
## ==============================================================================

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   ## Initialize the inverse as NULL
    set <- function(y) {
        x <<- y                                 ## Override the matrix when set is called
        m <<- NULL                              ## Reset the inverse to NULL
    }
    get <- function() x                            ## Return the current matrix
    setinverse <- function(inverse) m <<- inverse  ## Override the inverse
    getinverse <- function() m                     ## Return the current inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## ==============================================================================
## Function: cacheSolve
## This function takes in a matrix and checks whether there is previously 
## calculated inverse in the cache. If the inverse is in the cache, the function
## would return the cached inverse. If the inverse is not found in the cache, the
## function would calculate inverse of the matrix and store it in the cache.
## ==============================================================================

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()             ## Return a matrix that is the inverse of 'x'
    if(!is.null(m)) {               ## If the inverse is in cache, return it and exit
        message("getting cached data")
        return(m)
    }
    data <- x$get()                 ## Got here if inverse not in cache. Get the matrix.
    m <- solve(data, ...)           ## Calcualte inverse of the matrix
    x$setinverse(m)                 ## Set the inverse of matrix in the cache
    m
}

