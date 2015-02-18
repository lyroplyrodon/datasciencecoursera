## Caching data allows retrieving this data without having to recompute it which can
## advantageous when calculations take up a lot of computing resources. The functions
## are making use of R's scoping rules by using the <<- operator and assigning variables
## not the the local environment in which the functions are defined but to another environment.

## This function allows to create a matrix, get its values, calculate the inverse 
## of a given matrix and retrieve the inverse matrix. By not assigning x and m to 
## the local enrivonment, but using the <<- operator, the calculatd inversion of 
## the matrix is cache'd.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}



## This function calculates the inverse of the matrix that was generated using makeCacheMatrix.
## It first checks whether m, which is assigned to return the inverse, has been cache'ed before 
## and then returns the cache'ed value for m instead of recomputing the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
