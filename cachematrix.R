
# This program (cachematrix.R) has two functions, makeCacheMatrix and CacheSolve.
# The program is used to compute and display the inverse of a square matrix.
# If the inverse of that matrix has already been computed, the result is taken from the cached value and displayed.
# Else the inverse is calculated,cached and displayed.


## makeCacheMatrix is a function that creates a list with  4 functions that:
##  1. set the value of the matrix 2. get the value of the matrix
##   2. get the inverse of the matrix  3. set the inverse of a matrix

makeCacheMatrix <- function(x = numeric()) {
    minv <- NULL                                  # set minv to NULL.
    set <- function(y) {                          # sets x to the argument y,and minv to NULL.
        x <<- y                        
        minv <<- NULL                  
        }
    get <- function() x                           # returns the argument (x) of the makeCacheMatrix.
    setinverse <- function(solve) minv <<- solve  # sets minv in makeCacheMatrix to solve.
    getinverse <- function() minv                 # returns the value of minv from makeCacheMatrix.
    list(set = set, get = get,                    # returns a labeled list of functions  set, get getinv, setinv.
         setinverse = setinverse,
         getinverse = getinverse)
  
    }
## cacheSolve function calculates the inverse of the matrix created with the makeCacheMatrix function.
## The function first checks to see if the inverse has already been calculated. 
##  If yes, it gets the inverse from the cache and skips the computation.
## Else,it calculates the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()             # attempts to get the inverse from x.
    if(!is.null(minv)) {               # if not null, the cached value is returned in minv with message.
        message("getting cached data")
        return(minv)
        }
    data <- x$get()                    # since there is no cached value,get matrix from makeCacheMatrix.
    minv <- solve(data, ...)           # calculate inverse of the matrix data.
    x$setinverse(minv)                 # cache minv using setinverse function of makeCacheMatrix.
    minv                               # returns inverse of the matrix.
    }

