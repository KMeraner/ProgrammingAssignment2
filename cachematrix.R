## This script creates a 'special' matrix which inverse can be cached if already calculated.
## It contains two functions: makeCacheMatrix and cacheSolve.
## Date: 07.09.2019


## This function creates the special matrix object containing four functions: set, get, 
## getinverse and setinverse. Additionally, two variables (CacheMatrix = matrix and 
## inverse = inverse of the matrix) are defined. Both variables are stored in the 
## parent environment.
makeCacheMatrix <- function(CacheMatrix = matrix()) {
    inverse <- NULL                       # When the matrix is first defined, the inverse is set to NULL
    set <- function(data) {
        CacheMatrix <<- data
        inverse <<- NULL                  # When the matrix is set, the inverse is set to NULL
    }
    get <- function() CacheMatrix
    setinverse <- function(newinverse) inverse <<- newinverse
    getinverse <- function() inverse
    list(set = set,                  # Sets the matrix
         get = get,                  # Retrieves the matrix
         setinverse = setinverse,    # Sets the inverse of the matrix
         getinverse = getinverse)    # Retrieves the inverse of the matrix
}


## Write a short comment describing this function
## This function calculates the inverse of the 'special matrix'. If the inverse has already been
## computed, then the inverse is retrieved from the cache. Otherwise, the inverse is calculated.
## Note the input matrix must be a 'special' matrix defined by makeCacheMatrix.
cacheSolve <- function(makeCacheMatrix, ...) {
    inverse <- makeCacheMatrix$getinverse()
    if(!is.null(inverse)) {                # If the matrix is not NULL (i.e., already calculated)
        message("getting cached data")     # retrieve it from the cache. 
        return(inverse)
    }
    CacheMatrix <- makeCacheMatrix$get()
    inverse <- solve(CacheMatrix, ...)
    makeCacheMatrix$setinverse(inverse)
    inverse
}
