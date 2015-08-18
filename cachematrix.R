## Put comments here that give an overall description of what your
## functions do

## These functions return the inverse of a matrix...
## If the inverse has been previously calculated and cached,
## the cached result is returned.
## If the inverse has not been calculated and cached,
## the inverse is calculated, cached and returned.

## Write a short comment describing this function

## The makeCacheMatrix function...
## 1. sets the value of the matrix, 
## 2. gets the value of the matrix,
## 3. sets the value of the inverse of the matrix,
## 4. and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        my_inverse <- NULL
        set <- function(y){
                x <<- y
                my_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) my_inverse <<- solve
        getinverse <- function() my_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function...
## 1. Determines if the inverse of the matrix is cached...
## 2. If the inverse is cached, it returns the cached result. 
## 3. If the inverse is not cached, it calculates the inverse,
##    caches the result, and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inverse <- x$getinverse()
        if(!is.null(my_inverse)){
                message("getting cached data")
                return(my_inverse)
        }
        my_data <- x$get()
        my_inverse <- solve(my_data, ...)
        x$setinverse(my_inverse)
        my_inverse
}
