## In the following code, an matrix object is created
## and calculated its inverse

## Creates an matrix object

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
                x <<- y
                m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of a matrix. It retrieves the inverse
## from cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
