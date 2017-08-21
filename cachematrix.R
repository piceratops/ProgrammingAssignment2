# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function() inv <<- solve(x)
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        #this list is used as the input to cacheSolve()
}

## cacheSolve calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                inv
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
