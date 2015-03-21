## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function receives a matrix by argument and creates a list, 
# which contains a four functions that: set the matrix, get the matrix, 
# set the inverse matrix and get the inverse matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
#This function receives a list by argument and checks if the matrix inversion
#has already been made. If so, it gets the matrix inversion from the cache and skips 
#the inversion step. Otherwise, it calculates the matrix inversion and sets 
#the result in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
