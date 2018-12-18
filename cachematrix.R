## Write a function that saves time re-calculating the inverse of a matrix by
## storing and retrieving the result when it is first calculated 

# Create makeCacheMatrix to create a special "matrix" object that can 
# cache its inverse.

    makeCacheMatrix <- function(x = matrix) {
        
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        setinverse <- function(matix.inverse) m <<- matrix.inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }

# Create cacheSolve to compute the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

    cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }

###############################################################################
## Test 1
    
# create an invertible test matrix 
    x <- matrix(c(1, 3, 5, 2, 7, 9, 5, 2, 8), nrow = 3)

# feed the matrix into the functions 
    
    test = makeCacheMatrix(x)
    cacheSolve(test)
    
    # repeat cacheSolve to make sure the inverted matrix is cached
        cacheSolve(test)

## Test 2 

# build an invertible matrix directly into the function and solve
    y <- makeCacheMatrix(matrix(c(1,3,5,7,6,4,5,8,9), nrow=3))
    cacheSolve(y)

        

    
 
