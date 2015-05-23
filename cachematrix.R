## Put comments here that give an overall description of what your
## functions do
## -----------------------------------------------------------------------------
##  The overall purpose of these two functions is to cache the inverse of a 
##  martix by creating a special matrix object that has functions which allow 
##  it to set and get the values and also call on another function which will 
##  determine if the inverse of the matrix has already been performed which it 
##  will then retrieve the cached version instead of recalculating it. This
##  can be done because of the lexical scoping that R allows you to leverage 
##  caching results which can be called upon within nested functions.
## -----------------------------------------------------------------------------

## Write a short comment describing this function
## -----------------------------------------------------------------------------
##  This function creates a "special" matrix object which allows to populate it 
##  and return either the values of the matrix or inverse of the matrix.
## -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Write a short comment describing this function
## -----------------------------------------------------------------------------
##  This function checks to see if their is an inverse of the matrix already 
##  exists and will return the cached value but if not then it will compute
##  the inverse and store it into the special matrix object and populate the
##  cached object with the inverse matrix
## -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
            message("Getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}