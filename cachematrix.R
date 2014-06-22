## These two functions create a Matrix which includes a cached inversion of itself and provides for calculation, 
## caching, and retrieval from cache of the matrix and inversion of the matrix


## makeCacheMatrix: creates a Matrix that also includes an attribute for the cached inverse of the Matrix



makeCacheMatrix <- function(x = numeric()) {

        inverse <- NULL
        
        setmatrix <- function(y) {
        
                x <<- y
                
                inverse <<- NULL
                
        }
        
        getmatrix <- function() x
        
        setinverse <- function(inverse) inverse <<- inverse
        
        getinverse <- function() inverse
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse, getinverse = getinverse)
             
}


## cacheSolve: returns the inverse of a Matrix created via makeCacheMatrix.  If the inverse of the base Matrix in the CacheMatrix 
## has not yet been claculated and cached ## then the cacheSolve function will calculate the inverse of the base Matrix and
## cache it in the CacheMatrix object before returning the inverse of the base Matrix. If the inverse Matrix has already been
## cached in the CacheMatrix then cacheSolve retrieves the inverse of the base Matrix and returns it.


cacheSolve <- function(x, ...) {

   inverse <- x$getinverse()
   
        if(!is.null(inverse)) {
        
                message("getting cached data")
                
                return(inverse)
                
        }
        
        data <- x$getmatrix()
        
        inverse <- solve(data, ...)
        
        x$setinverse(inverse)
        
        inverse
       
}
