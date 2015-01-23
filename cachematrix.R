## Coursera: R-Programming: Project 2
## bjurban, 2015-01-23
## These functions cache the solution to a matrix inversion

## This function is a special matrix object that can cache its inverse solution.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL    # creates empty solution variable, waiting to be set
  set <- function(y){
    x <<- y    # defines the matrix
    s <<- NULL # clears the solution when the matrix is changed
  }
  get <- function() x
  setsol <- function(sol) s <<- sol 
  getsol <- function() s
  list(set = set, get = get, 
       setsol = setsol,
       getsol=getsol)
}

## This function solves the matrix 'x' or returns the cached solution if the 
## matrix has already been solved, but not modified.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsol()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsol(s)
  x
}

## example code:
z = matrix(c(1,1,2,-1,-3,-3,2,2,2), nrow=3)
m <- makeCacheMatrix(z)
g <- cacheSolve(m)
g$getsol()