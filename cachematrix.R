## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## 


## 

## makeCacheMatrix creates a special "Matrix", which is really a list containing a function to:

## set(x) Set the value of the matrix
##  get() Get the value of the matrix
##  setinv() Set the value of the inverse of the matrix
##  getinv() Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. 
## Althoug, Cachesolve first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, 
## it calculates the inverse  of the matrix and sets the value of the inverse matrix
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
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
