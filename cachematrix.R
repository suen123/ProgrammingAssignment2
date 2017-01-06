## A pair of funcions that cache the inverse of a matrix 

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix, and/or retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- matrix(data, ...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
