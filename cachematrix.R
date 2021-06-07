## Functions used to cache the inverse of a matrix

## creates a list containing functions which set the value of the matrix, get the value of the 
# matrix, set the value of the inverse in the cache and get the value of the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inv <<- solve
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse
  )
}


## calculates the inverse of the matrix from the first function but only if the inverse is not
# in the cache already

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(x)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}
