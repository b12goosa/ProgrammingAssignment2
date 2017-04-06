## Put comments here that give an overall description of what your
## functions do

## What this function do?
## Well, Matrix inversion use to be an high cost in the running computation. So, here it is
## the caching of the inverse of a matrix to avoid repeatedly run the inverse operation.

## In this first function, it is created a special "MATRIX" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ##set the matrix to the cache
                x <<- y
                i <<- NULL
        }
        get <- function() x ##get the matrix from the cache
        setinverse <- function(solve) i <<- solve ##set the inverse matrix to the cache
        getinverse <- function() i ##get the inverse matrix from the cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) ##listing objects from cache
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){ ##checking if the inverse matrix of x is saved in cache
                message("getting cached data")
                return(i)
        }
        data <- x$get() ##getting 'X' from cache
        i <- solve(data, ...) ##inverse of the matrix (x) from cache
        x$setinverse(i) ##save inverse into the cache
        
        
        i ## Return a matrix that is the inverse of 'x'
}
