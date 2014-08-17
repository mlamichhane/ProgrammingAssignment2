## The pair of functions below cache the result of inverse of a matrix.
## Since matrix inversion operation is considered as an expensive
## operation, it is recommended that the result of previous calculation
## is cached rather then computing it again.

## This function creates a matrix object that can cache it's inverse.
## It has 4 properties to set and get the matrix as well as it's inverse.
## This function also assumes that the matrix suppiled is always invertible.
##
## Usage: a <- makeCacheMatrix(matrix(1:4, 2,2))
##
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  set_inv_mat <- function(mat) inv_mat <<- mat
  get_inv_mat <- function() inv_mat
  list(set = set, get = get, setmat = set_inv_mat, getmat = get_inv_mat)
}


## This function computes the inverse of matrix returned by makeCacheMatrix function
## above. If the inverse has already been calculated for the given matrix, 
## then the cachesolve should retrieve the inverse from the cache.
##
## Usage: cacheSolve(a)
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getmat()
  if(!is.null(invmat)) {
    message("Getting result from cache")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setmat(invmat)
  invmat
}
