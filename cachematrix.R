
## The first function creates a special matrix object that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse if the matrix changes
  }
  get <- function() x  # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Store the inverse
  getInverse <- function() inv  # Return the cached inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}

