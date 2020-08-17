## Matrix inversion is usually a costly computation
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below are a pair of functions that cache the inverse of a matrix
## Assumed that the matrix supplied is always invertible


## This function creates a special matrix object thatcan cache its inverse

makeCacheMatrix <- function(x=matrix()) {
  Inv <- NULL
  Set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse
       getInverse = getInverse
}


## That function computes the inverse of the special matrix returned by makeCacheMatrix here above
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve theinverse from the data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cashed data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
