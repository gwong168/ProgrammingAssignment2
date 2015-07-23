# makeCacheMatrix is the function creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3.set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x= matrix()) {
        im <- NULL         
        set <- function(y) {
             x <<- y
             im <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) im<- inv
        getinverse <- function() im
        list( set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  If the inverse
# has already been calculated (and the matrix is not changed), then the cachesolve should retrieve the inverse from the cahce.

cacheSolve <- function(x, ...) {
       im <- x$getInverse()
       if(! is.null(im)) {
          message("getting cached data")
          return (im)
      }
      data <- x$get()
      im <- solve(data)
      x$setinverse(im)
      im
}
