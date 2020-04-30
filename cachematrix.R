## The following are two functions that create a special object which
## stores the matrix and caches its inverse 

## This is a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x 
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function () m
        list( set = set,
              get = get, 
              set_inverse = set_inverse,
              get_inverse = get_inverse)
        

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), then the cache
## solve should retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                
        }
        
        mat <- x$get()
        m <- solve(mat, ...)
        x$set_inverse(m)
        m
}
