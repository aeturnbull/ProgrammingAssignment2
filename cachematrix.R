# The functions makeCacheMatrix and cacheSolve may save time when computations require matrix inversion. 
# The 1st function generates a list of functions which set and retrive the value of a matrix and its inverse. 
# The 2nd function checks to see if the inverse of a matrix has already been calculated. 
# If not, the inverse will be calculated and cached. 


## makeCacheMatrix generates a special "matrix" containing four functions. 

makeCacheMatrix <- function(x = matrix()) {

        m<-NULL
        setMatrix<-function(y) {
                x<<- y  
                m<<- NULL 
        }
        getMatrix<-function() x 
        setInverse<- function(solve)  m<<- solve 
        getInverse<- function() m  
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, 
             getInverse = getInverse)  
}


## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix.
## If the inverse has already been computed, then the inverse is retrieved from the cache. 

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<-x$getMatrix()
        m<-solve(data,...)
        x$setInverse(m)
        m
}



