
## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. This assignment is to write a 
## pair of functions that cache the inverse of a matrix.
## So this function will create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix 
  get <- function() x
  
  # set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get the value of the inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache. An assumption is taken that the
## function always get a matrix that is invertable.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

## Sample run (remove "## > " to run)
## > x <- rbind(c(1, -0.6), c(-0.6, 1))
## > m <- makeCacheMatrix(x)
## > m$get()
## output from m$get()
## [,1] [,2]
## [1,]  1.0 -0.6
## [2,] -0.6  1.0

## first cache solve run. should not hit cache.
## > cacheSolve(m)
## as we can see no message was outputted.
## [,1]   [,2]
## [1,] 1.5625 0.9375
## [2,] 0.9375 1.5625

## second cache solve run should hit cache.
## > cacheSolve(m)
## getting cached data.
## [,1]   [,2]
## [1,] 1.5625 0.9375
## [2,] 0.9375 1.5625

## as we can see the message getting cache data was printed
## thus showing that the matrix was cached.
