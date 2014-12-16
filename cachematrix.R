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
    inversematrix <- NULL        ##  create a empty instance of the matrix cache
    set <- function(y) {         ##  defines the set function
        x <<- y                  ##  stored the matrix in the global environment
        inversematrix <<- NULL   ##  creates a null instance to store in the global environment
    }
    get <- function() x          ##  defines the get function
    setinversematrix <- function(solve) inversematrix <<- solve     ##  calculates and store the inverse of a matrix
    getinversematrix <- function() inversematrix                    ##  returns the matrix invers
    list(set=set,get=get,                                           ##  stores functions in a list
         setinversematrix=setinversematrix,
         getinversematrix=getinversematrix)
}


## Write a short comment describing this function
## This function checks to see if the cached matrix inverse
## is already stored in the global environment.  If it already
## exists the cached inverse is returned, if it is not cached 
## the inverse is calculated and stored in the global environment
## and then returns the value to the function caller

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getinversematrix()             ## set the global environmen
        if(!is.null(inversematrix)) {                     ## checks to see if global variable is set
            message("getting cached matrix")              ## returns message if using the cached inverse matrix
            return(inversematrix)                         ## returns the inverse matrix
        }
        data<-x$get()                                     ## gets a instance of the matrix
        inversematrix<-solve(data,...)                    ## calculates and stores the inverse matrix
        x$setinversematrix(inversematrix)                 ## sets the global variable to the inverse matrix
        inversematrix                                     ## returns the invere matrix
}
