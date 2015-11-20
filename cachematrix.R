## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Cache Value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get value of the matrix
  get <- function() x
  ## Set Value for the inverse of the matrix
  setInv <- function(solve) m <<- solve
  ## Get Value for the inverse of the matrix
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  ## If inverse is cached return cached value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else get compute for inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  ## Output inverse of matrix
  m
}
