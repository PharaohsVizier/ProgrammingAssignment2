## The two functions below work together to create and set a matrix and to solve and cache
## the inverse.

## makeCacheMatrix sets up a matrix and creates methods to set and retrieve matrix values 
## and to set and retrieve the inverse.  
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     # initial inverse is null
        
        set <- function(y) {                            # set method to initialize
                x <<- y                                 # copies the matrix from input
                inv <<- NULL                            # no solution to inverse yet
        }
        
        get <- function() x                             # get method returns matrix
        
        setinverse <- function(inverse) inv <<- inverse # manually input inverse solution
                                                        # for cache
        
        getinverse <- function() inv                    # returns inverse (may return NULL)
        
        list(set = set, get = get,                      # main function returns a list of
             setinverse = setinverse,                   # the functions created
             getinverse = getinverse)
}


## cacheSolve computers an inverse of the makeCacheMatrix, unless a cached version of the 
## solution is found in which case it will return the cached solution.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()                             # attempts to retrieve inverse
        
        if(!is.null(inv)) {                             # if inverse solution is not NULL
                message("getting cached data")          # return cached solution
                return(inv)
        }
        
        data <- x$get()                                 # else, get data from input
        inv <- solve(data, ...)                         # use solve to calculate inverse
        x$setinverse(inv)                               # cache inverse for future use
        
        inv                                             # return inverse
}
