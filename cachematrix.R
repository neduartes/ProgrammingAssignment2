## These functions define an Cache Matrix that saves the value of the
## inverse for multiple calls. Since the calculation of the inverse of 
## a matrix is computationally expensive, we want to avoid the need to
## calculate it several times in a procedure.

## This function builds an Cache Matrix object and defines the four
## functions necessary to use the Cache Matrix
## Function: 
##   set()        writes a new value to the Cache Matrix and erases 
##                the inverse cache.
##
##   get()        returns the current value of the matrix
##
##   setinverse() saves the value of the inverse of the
##                        matrix
##   getinverse() returns the value of the saved value of the inverse
##                of the matrix. If no value is available it returns NULL
##

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  set <- function(y) {
    mat <<- y
    inverse <<- NULL
  }
  get <- function() mat
  setinverse <- function(mat) inverse <<- solve(mat)
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of a Cache Matrix
## First a cached value of the inverse is searched, and if it is available
## it is returned directly.
## If the inverse of the matrix has not been previously saved, it calculates
## it, and saves it in the Cache Matrix for future calls.

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getinverse()
  if(!is.null(inverse)) { # If cached value available, return it
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get() # No cache value availabe, the calculate it
  inverse <- solve(data)
  matrix$setinverse(inverse) # Save inverse for future calls
  inverse
}
