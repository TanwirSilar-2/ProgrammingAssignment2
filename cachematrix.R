## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Used the example to write the two functions 
## However the logic behind it is simple 
## For the make Cache Matrix we expect output a list of functioins
## Which are Get,Set, Set inverse, Get inverse
## but then <<- operator is used to change the value of matrix 'x' and 
## inverse of matrix x 'xinv' in an envi other than current envi
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function() xinv <<- solve
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This cache solve function is called, when the inv of matrix given is req
## Checking whether the inverse is calculated before, if yes then using 
## get func of make cache matrix it is retrieved else
## using solve inv is calculated and set to xinv and then returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinverse(xinv)
  xinv
}
