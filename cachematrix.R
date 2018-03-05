

## This function is to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function is to compute te inverse of the special matrix made by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(matrix(data, nrow = rows, ncol = cols), ...)
        x$setInverse(inv)
        inv
}
