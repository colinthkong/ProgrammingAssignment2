## Matrix inversion is a costly computation and this pair of functions can create a custom type of matrix
## and provide a custom method for calculating the inverse of the matrix, 
## such that the inverse of the matrix can be cached after the first time of computation, 
## then the cache can be returned directly in the subsequent calls of the inverse operation,
## rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) ix <<- inverseMatrix
  getInverseMatrix <- function() ix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is returned directly from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getInverseMatrix()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setInverseMatrix(ix)
  ix
}
