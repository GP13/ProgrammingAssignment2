## The makeCacheMatrix function is responsible for creating a special matrix object.
## The cacheSolve function is responsible for calculating the inverse of the matrix.
## Once the inverse has been calculated, if the inverse has been solved previously, 
## the cacheSolve function will return it from the cache instead of calculating it 
## again.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  set_inv<- function(inverse) invx <<-inverse
  get_inv <- function() invx
list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$get_inv()
  if (!is.null(invx)) {
    message("getting cached inverse matrix")
    return(invx)
  } else {
    invx <- solve(x$get())
    x$set_inv(invx)
    return(invx)
  }
}
