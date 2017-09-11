## I first create a function that creates a special matrix that caches values, then the second function calculates the inverse of that matrix

## This function creates a special matrix where I can store values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #set the value of the matrix
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse <- function(solve) m <<- solve #set value of inverse
  getinverse <- function() m #get value of inverse
  matrix(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates inverse of above matrix, but checks to see if inverse has already been calculated

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
