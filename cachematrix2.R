## 2 functions to cache inverse of matrix


## Create matrix to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize inverse property
    i <- NULL
    ## Set Matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    ## Make matrix
    get <- function() {
        m
    }
    ## Set inverse of matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    ## Get Inverse of matrix
    getInverse <- function() {
        i
    }
    ## Return list of functions
    list(set = set, get = get, setInverse=setInverse, getInverse = getInverse)
}




## cacheSolve gets the inverse of makeCxacheMatrix, and get inverse of matrix from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return inverse if already set
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data) %% data
    x$setInverse(m)
    m
}
