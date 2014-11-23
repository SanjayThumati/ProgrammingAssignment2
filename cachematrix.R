## R Programming | Coursera | Programming Assignment 2
## Creates the following 2 functions:
## FUNCTION 1 - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## FUNCTION 2 - cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## FUNCTION 1 - makeCacheMatrix: creates list of functions which can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## FUNCTION 2 - cacheSolve: calculates the inverse of a matrix returned by makeCacheMatrix() function
## if the inverse has already been calculated (and the matrix has not changed), it retrieves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    print ("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}
