# R-Programme Assignment 2: Caching the Inverse of a Matrix

# ("cacher fr.", meaning 'to hide') special functions.

# LESSON
# Study the given examples on the treatment of a vector
# Note the introduction of <<- operator

# VALUE OF LESSON
# Matrix inversion, along with some others, 
# is said to be a costly computation 
# (frequency of reference, user system time, waiting time, etc.)

# A well designed and effective function can be stored,  
# recalled and used timely and efficiently

# Example 1 Caching the Mean of a Vector
# creates a special vector of a list containing a function to
    ## a) set the value of the vector, b) get the value of the vector
    ## c) set the value of the mean and d) get the value of the mean
#			SPECIAL FUNCTION
# Now the for the special "matrix" object 
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Create a placeholder matrix
    ## INSTRUCTION: assume matrix supplied is always invertible
    iM <- NULL
    set <- function(y) {
        x <<- y
        iM <<- NULL
            }
    ## get the input 
    get <- function() x

    
    setinverse <- function(inverse) iM <<- inverse
    getinverse <- function() iM
    
    ## the matrix, its inverse obtained through 'solve'
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



# Develop a function to calculates the inverse of the special "matrix"
# First check to see if the inverse has already been computed.
# If not already done, compute the inverse and store it.

# The function
cacheSolve <- function(x, ...) {
    
    ## attempt to get the inverse of the matrix from hidding
    iM <- x$getinverse()
    
    ## check the getting result: if not a failure (i.e. iM exists)   
    if(!is.null(iM)) {
        message("inverse exists, please hold on")
        return(iM)
    }
    ## failing that, calculate it   
    
    ## Computing the inverse of a square matrix 
    ## can be done with the generic function 'solve'
    data <- x$get()
    iM <- solve(data, ...)
    
    ## Return a matrix that is the inverse of 'x' 
    x$setinverse(iM)
    iM
}
# Test drive
# > x <- rbind(c(4,3), c(3, 2))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(m)
# inverse exists, please hold on
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > 