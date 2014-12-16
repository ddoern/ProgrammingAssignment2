#makeCacheMatrix does 4 things:
#Sets the value of a matrix
#Gets the value of a matrix
#Sets the value of the inverse of the matrix
#Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


#cacheSolve works with the matrices defined by makeCacheMatrix to
#solve for and return the inverse of the matrix if it has not yet been calculated, or
#return the cached value of the matrix if it has already been calculated

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}