## Put comments here that give an overall description of what your
## functions do

# My functions act to cache potentially time-consuming computations of 
# matrix inversion.

## Write a short comment describing this function

# This function stores functions that are used to store/retrive the
# inverse matrix. It creates a special matrix with caching ability: 
# once an inverse matrix is calculated, it will be stored in cache. 

makeCacheMatrix <- function(x = matrix()) {
    cashedInverse <- NULL
    
    getMatrix <- function() x
    setInverse <- function(solve) cashedInverse <<- solve
    getInverse <- function() cashedInverse
    
    list(getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

# This function computes the inverse of the special matrix returned
# by makeCacheMatrix function. If the inverse matrix has already been
# calculated and the matrix has not changed, then cacheSolve will
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    sol <- x$getInverse()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    matrix<- x$getMatrix()
    sol <- solve(matrix, ...)
    x$setInverse(sol)    
    sol
}
