

## Programming Assignment 2
##
## Caching the Inverse of a Matrix
##
## Create a function makeCacheMatrix 
## makeCacheMatrix declares a function that works like a object type for
##     storing a matrix value and potientially its inverse value
## operators set, get, setinv, getinv
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Create a function cacheSolve 
## cacheSolve takes an object of type makeCacheMatrix and finds the 
## inverse of the matrix stored in the makeCacheMatrix object
## if the inverse of the matrix has been previously calculated and cached
## then the cached value is returned otherwise the inverse matrix
## is calculated, stored in the cache of the makeCacheMatrix object,and returned
cacheSolve <- function(cacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'cacheMatrix'
  m <- cacheMatrix$getinv()
  if (!is.null(m)) {
    message("returning cached data")
  }   
  else {    
    message("calculating matrix inverse")
    data <- cacheMatrix$get()
    m <- solve(data, ...)
    cacheMatrix$setinv(m)
  }
  m
}

## Example run
## create an object m of type makeCacheMatrix 
m <-makeCacheMatrix(matrix(1:4, 2, 2))
## Use get method to show matrix value
m$get()

## call cacheSolve with m 
## Note you will get:  Error in solve.default(data, ...) : 
##  Lapack routine dgesv: system is exactly singular...
## Means the matrix passed to solve is singular and cannot be inverted
##
cacheSolve(m)
## Second run of cacheSolve with same m paramter should produce "getting 
cacheSolve(m)
