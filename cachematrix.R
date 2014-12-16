## Darren Freeman
## Assignment #2 
## R programming December 2014
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a matrix, calculates the invers and stores 
## it in the global environment.  The purpose of this is the reduce
## the overhead and processing time which is required to calculate 
## the inverse and speeds the calculation of downstream calculations

makeCacheMatrix <- function(x = matrix()) {
    matrixcache <- NULL
    set <- function(y) {
        x <<- y
        matrixCache <<- NA
    }
    get <- function() x
    setmatrix <- function(solve) matrixcache <<- solve
    getmatrix <- function() matrixcache
    list(set=set,get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Write a short comment describing this function
## This function checks to see if the cached matrix inverse
## is already stored in the global environment.  If it already
## exists the cached inverse is returned, if it is not cached 
## the inverse is calculated and stored in the global environment
## and then returns the value to the function caller.x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixcache <- x$getmatrix()
        if(!is.null(matrixcache)) {
            message("getting cached matrix")
            return(matrixcache)
        }
        data<-x$get()
        matrixcache<-solve(data,...)
        x$setmatrix(matrixcache)
        matrixcache
}
