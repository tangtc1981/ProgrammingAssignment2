## makeCacheMatrix creates a matrix object 
## that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse<- function(inverse) inv <<-inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## cacheSolve returns the inverse of the matrix.

## If the matrix inverse has been calculated, it will find it from
## cache instead of recalculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if (!is.null(inv)) {
        message("Cached inverse matrix")
        return(inv)
      } else {
        inv <- solve(x$get())
        x$setinverse(inv)
        return(inv)
      }
}
