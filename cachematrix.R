## Here are two functions to provide an ability of caching already solved matrixes (inverse)

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## initializing empty variables for matrix inverse
  matrixInverse <- NULL
  
  ## creating a setter for the matrix itslef
  set <- function(matrix) {
    x <<- matrix
    matrixInverse <<- NULL
  }
  ## simple getter of the matrix
  get <- function() x
  ## setter for the inverse matrix
  setInverse <- function(matrix) matrixInverse <<- matrix
  ## simple getter of the inverse matrix
  getInverse <- function() matrixInverse
  
  ## list of the methods provided by the function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns cached inverse of the special "matrix" returned by makeCacheMatrix 
## or computes it and cache for later usage
cacheSolve <- function(x) {
  ## trying to reach cached inverse
  matrixInverse <- x$getInverse()
  
  ## if it was cached, inverse shouldn't be a null
  ## printing a message and returning a cached value
  if (!is.null(matrixInverse)) {
    message("Getting cached inverse...")
    return(matrixInverse)
  }
  
  ## here is a computing part of the inverse, in case when inverse wasn't cached
  ## grabbing a matrix
  matrix <- x$get()
  ## solving it
  matrixInverse <- solve(matrix)
  ## and caching an inverse
  x$setInverse(matrixInverse)
  
  ## Return a matrix that is the inverse of 'x'
  matrixInverse
}
