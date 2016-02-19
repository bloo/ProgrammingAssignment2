
## Creates a matrix DTO that allows users to access
## the underlying matrix and its cached inverse, if
## it exists. Setting a new underlying matrix will
## clear the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function solves the matrix wrapped in the DTO
## as created by the `makeCacheMatrix` function. It
## returns any cached inverse, and it it doesn't exist,
## is responsible for solving its inverse and storing
## it in the DTO's cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
