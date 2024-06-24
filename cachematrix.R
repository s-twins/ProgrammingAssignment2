# Function to create a special matrix object that caches its inverse
makeCacheMatrix <- function(matrix = matrix()) {
    # Initialize the cache
    cache <- NULL
    
    # Setter function to set the matrix and invalidate cache
    set <- function(input) {
        matrix <<- input
        cache <<- NULL  # Invalidate cache when matrix changes
    }
    
    # Getter function to retrieve the matrix
    get <- function() {
        matrix
    }
    
    # Setter function to compute and cache the inverse of the matrix
    setinverse <- function(inverse) {
        cache <<- inverse
    }
    
    # Getter function to retrieve the cached inverse
    getinverse <- function() {
        cache
    }
    
    # Return a list of functions for matrix operations
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Function to compute the inverse of a matrix, caching if possible
cacheSolve <- function(x, ...) {
    # Retrieve cached inverse if available
    cached_inverse <- x$getinverse()
    if (!is.null(cached_inverse)) {
        message("Getting cached inverse")
        return(cached_inverse)
    }
    
    # Compute inverse and cache the result
    matrix_data <- x$get()
    inverse <- solve(matrix_data, ...)
    x$setinverse(inverse)
    inverse
}
