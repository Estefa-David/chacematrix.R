##As matrix inversion is usually a costly computation, the idea is cache the inverse of matrix with the aim of save time or 
##memory. 

## This function creates a special "matrix" object that can cache its inverse, which is
##really a list containing a function to

#1.  set the value of the vector
#2.  get the value of the vector
#3.  set the value of the inverse
#4.  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv<<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by the function:makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it function (cacheSolve) should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv) ) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
