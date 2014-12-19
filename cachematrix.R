## Below are two functions that are used to create a special 
## object that stores a Matrix and cache's its inversion.


## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() i
  setinversion <- function(solve) i <<- solve
  getinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(i)
  i
}
