## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(matrix, ...) {
  inverse <- matrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrix$get()
  inverse <- solve(data)
  matrix$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
