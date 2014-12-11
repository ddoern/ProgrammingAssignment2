#cacheSolve works with the matrices defined by makeCacheMatrix to
#solve for and return the inverse of the matrix if it has not yet been calculated, or
#return the cached value of the matrix if it has already been calculated

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}