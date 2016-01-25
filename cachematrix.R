## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## It does so by creating a matrix 'x' and utilising fuction to set and get the matrix and to set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        CacheInv <- NULL
        
        set <- function(inputValue = matrix()) {
                x <<- inputValue
                CacheInv <<- NULL
        }
        
        get <- function() { x
        }        
        setInv <- function(invVal){
                CacheInv <<- invVal
                
        }
        
        getInv <- function() CacheInv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


## This function will check and then calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first check if exists
        invertedVal <- x$getInv()
        
        ## then check other data may already be cached and pop up a message
        if(!is.null(invertedVal) ) { 
                message("getting cached matrix")
                return(invertedVal)
        }
        
        ## if not go get matrix
        message("calculating matrix now...")
        matrixToInvert <- x$get()
        CacheInv1 <- solve(matrixToInvert)
        x$setInv(CacheInv1)
        CacheInv1
        
}
