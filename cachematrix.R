## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## This pair of functions cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. The "matrix" is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 3. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invm <<-inverse
        getinverse <- function() invm
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
        
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        invm <- x$getinverse()               ##try to get from the cache
        if(!is.null(invm)) {                     ## If not null, data in the cache
                message("Getting cached data")
                return(invm)
        }
        data <- x$get()             ##Not in cache, get data first
        invm <- solve(data)         ##compute the inverse
        x$setinverse(invm)          ##Set in the cache
        invm                ## Return a matrix that is the inverse of 'x'            

}

## Example of use:
##
##> x =rbind(c(1,0),c(0,2))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    0
##[2,]    0    2
##> cacheSolve(m)
##[,1] [,2]
##[1,]    1  0.0
##[2,]    0  0.5
##> cacheSolve(m)
##Getting cached data
##[,1] [,2]
##[1,]    1  0.0
##[2,]    0  0.5




