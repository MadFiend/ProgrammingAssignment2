## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an object to cache the inverse of the matrix. 
## The function contains a list of functions get or set the value of the matrix
## and get and set the inverse of a matrix.
## This function assumes that any matrix it encounters is invertible.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(set = set,
       get = get, 
       getInverse = getInverse,
       setInverse = setInverse)
}


## cachSolve calculates the inverse of  a matrix by first 
## checking if the calculation has been done and the matrix is cached.
## And calculating the inverse of a matrix if it has not already been done and 
## sets the value with the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$setInverse(i)
  i
}

##  Test results
##  myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))

##  > myMatrix$getInverse()
##  NULL

##  > myMatrix$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4

##  > cacheSolve(myMatrix)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##  > myMatrix$getInverse()
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

##  myMatrix$set(matrix(c(1, 4, 2, 4), 2, 2))

##  > myMatrix$get()
##       [,1] [,2]
##  [1,]    1    2
##  [2,]    4    4

##  > myMatrix$getInverse()
##  NULL

##  > cacheSolve(myMatrix)
##       [,1]  [,2]
##  [1,]   -1  0.50
##  [2,]    1 -0.25

##  > cacheSolve(myMatrix)
##  Getting cached data...
##       [,1]  [,2]
##  [1,]   -1  0.50
##  [2,]    1 -0.25