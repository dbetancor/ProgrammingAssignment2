## Put comments here that give an overall description of what your
## functions do

## In this function we take a matrix and we do it cacheable the inverse, could be
# extended for more cacheable characteristics

makeCacheMatrix <- function(x = matrix()) {
  # When we call the function te first time we are sure to NULL
  #the inverse matrix, that is we make a matrix with 0 column and rows
  i <- matrix(nrow=0, ncol=0)

  set <- function(y) {
    x <<- y
    i <<- matrix(nrow=0, ncol=0)  #when ser we need to zero the inverse (because should be different)
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  #Is the inverse that we have in cache a good one
  if(dim(i)[1] != 0 || dim(i)[2] != 0) {
    message("getting cached data")
    return(i)
  }
  #O no!, let's solve the inverse and put it in cache..
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  #and return the inverse
}
