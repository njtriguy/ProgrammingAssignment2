## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrixCache <- NULL
    set <- function(y) {
        x <<- y
        matrixCache <<- NA
    }
    get <- function() x
    setMatrix <- function(solve) matrixCache <<- solve
    getMatrix <- function() matrixCache
    list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixCache <- x$getMatrix()
        if(!is.null(matrixCache)&&is.matrix(matrixCache)) {
            message("getting cached matrix")
            return(matrixCache)
        }
        data<-x$get()
        matrixCache<-solve(data,...)
        x$setMatrix(matrixCache)
        matrixCache
}
