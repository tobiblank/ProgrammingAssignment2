## The functions makeCacheMatrix and cacheSolve provide a convenient way to compute and cache the inverse of a matrix.

## Function 'makeCacheMatrix': Creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Placeholder for the calculated inverse
    set <- function(y) { 
        x <<- y          # Assign a new matrix y to the object 
        inv <<- NULL     # Invalidate the existing cached inverse
    }
    get <- function() x  # Retrieve the current matrix value
    setinverse <- function(inverse) inv <<- inverse      # Set the cached inverse 
    getinverse <- function() inv                         # Retrieve the cached inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Function 'cacheSolve': Calculates inverse, using the cache if available

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Attempt to retrieve cached inverse
    
    # If inverse isn't cached, calculate and store it
    if (!is.null(inv)) { 
        message("Retrieving cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse  
    data <- x$get()           
    inv <- solve(data, ...)       # Calculate the inverse of the matrix
    x$setinverse(inv)         # Store the inverse in the cache
    inv                       # Return the calculated inverse
}
