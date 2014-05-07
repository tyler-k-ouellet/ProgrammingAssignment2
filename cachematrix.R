## These functions are used to build a speical "matrix" list that gets the inverse and caches its inverse.
## The makeCacheMatrix function builds a special "matrix" object that caches its inverse.
## The cacheSolve function solves the matrixs inverse or retrieves the cached inverse.
## the inputed data is assumed to be a matrix with an inverse.

## Note: all code adopted from Coursera coursework on R Programming through John Hopkins University.

## The makeCacheMatrix function builds a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes the special "matrix" object and either solves for the inverse or returns 
## the cached invese in the "matrix" object.

cacheSolve <- function(x, ...) { 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
