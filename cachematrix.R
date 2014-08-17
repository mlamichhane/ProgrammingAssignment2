## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getmat()
  if(!is.null(invmat)) {
    message("Getting from cache")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setmat(invmat)
  invmat
}
