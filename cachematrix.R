## The objective of the functions is to extend the functionality of matrixes
## in R by enabling the possibility to cache lengthy operations.
## The first function extends the matrix with several methods which are
## required to cache the inverse of the matrix
## The second modifies the calculation of the inverse to include the
## possibility to recover the cached result

## Function that provides a list of functions to extend a matrix and
## includes support to cache the inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Function that includes new functionality to solve() in order to use
## it with a matrix with cache functionality and speed up the calculation
## of the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
