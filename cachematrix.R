## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
  inv
    ## Return a matrix that is the inverse of 'x'
  }
  
b=matrix(c(1,4,2,8,4,9,1,3,5), nrow=3, ncol=3)
solve(b)

a <- makeCacheMatrix(b)
cacheSolve(a)

