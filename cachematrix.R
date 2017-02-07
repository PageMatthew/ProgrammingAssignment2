## Creates a matrix that can calculate it's own inverse
## and cache the inverse to speedup future calculations

## takes a matrix and return an object with functions
## to get/set the matrix and get/set the inverse

makeCacheMatrix <- function(x = matrix()) {
    ci <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(cacheInverse) ci <<- cacheInverse 
    getInverse <- function() ci
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## return the cached inverse or calculate it if no cache
## will fail miserably if the determinant of the matrix is 0
## (matrix is not invertable)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ci<-x$getInverse()
    if(!is.null(ci)) {
        message("Getting cached data")
        return(ci) 
    }
    data <- x$get()
    ci <- solve(data,...)
    x$setInverse(ci) 
    ci
}
