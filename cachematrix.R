## helper functions for caching inverse of matrices

## this function returns a special list with names 
## that are functions to access (get and set)
## the given matrix and it's inverse (getinverse setinverse)
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


## this function calculates and caches the inverse of the given matrix
## created with the function above. if the inverse was already calculated
## in the past it returnes it from the cache

cacheSolve <- function(x, ...) {
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
