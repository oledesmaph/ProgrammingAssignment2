## 2 functions to cache inverse of matrix


## Create matrix to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse property
    m <- NULL
    ## Store Matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Return sotred matrix
    get <- function() {
        x
    }
    ## Set inverse of matrix
    setInverse <- function(solve) {
        m <<- solve
    }
    ## Get Inverse of matrix
    getInverse <- function() {
        m
    }
    ## Return stored inverse
    list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}




## Use makeCacheMatrix t either get ivnerse of original matrix
## store and return it, or retrieve and return cached matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return inverse if already set
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ##Get original matrix
    data <- x$get()
    ##Get inverse
    m <- solve(data, ...)
    ##Store inverse of matrix
    x$setInverse(m)
    ##Return inverse of matrix
    m
}
