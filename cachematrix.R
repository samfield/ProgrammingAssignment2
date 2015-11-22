## Put comments here that give an overall description of what your
## functions do

###The following functions handle the calculation and caching of an inverse matrix
###Only requirement is that the matrix is a "square" matrix ie. nbr of cols = nbr of rows
###1.)Both should be sourced to the work environment
###2.)The objective matrix should be entered in makeCacheMatrix which should be assigned
###For example a <- makeCacheMatrix(b) where b is the matrix
###3.) the assigned object should be called with cacheSolve
###4.) Cachesolve returns the solution either from cache or by solving&caching


## Write a short comment describing this function
###This function is a so called closure function
###It encloses the environment of the parent function and provides access to all of the listed functions 
###It creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get= get,
       setinv= setinv,
       getinv = getinv)
}


## Write a short comment describing this function
###cacheSolve provides a check if the matrix was already solved for its inverse and provides the result
###Result is either provided by solving the provided matrix or by using the matrix that was already solved for its inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ##Checking if the inverse has already been calculated  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
  inv
  }

