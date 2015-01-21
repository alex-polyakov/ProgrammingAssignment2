## These functions allow to compute and store inverse matrix in cache.
## Once inverse matrix has been computed, a cached version is used
## rather than computed repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## The "matrix" object is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    # Set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get matrix
    get <- function() x
    
    # Set inverse matrix
    setInverse <- function(i) inv <<- i
    
    # Get inverse matrix
    getInverse <- function() inv
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Get inverse matrix from cache
    inverseMatrix <- x$getInverse()    
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    # Calculate inverse matrix ant put it to the cache
    matrix <- x$get()
    inverseMatrix <- solve(matrix, ...)
    x$setInverse(inverseMatrix)
    inverseMatrix
    
}
