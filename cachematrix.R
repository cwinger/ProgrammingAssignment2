## These functions simulate object oriented programming.  They create
## an "object" that can store a matrix and it's inverse, whose values
## can be accessed and manipulated using "getter" and "setter" functions
## associated with the object.

## makeCacheMatrix initializes an object of a particular matrix
## Each object created can store it's values and an inverse

makeCacheMatrix <- function(x = matrix()) {
  # When the matrix is initialized, the inverse is set to NULL
  inverse <- NULL
  
  # This "setter" function allows us to overwrite the CacheMatrix
  # "obect" with a new matrix.   
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # This "getter" function allows us to access the values of the 
  # matrix "object" indirectly.  It simply returns the matrix values.
  get <- function() x
  
  # This "setter" function allows us to reset the values of the 
  # inverse matrix
  setinverse <- function(inverse) inverse <<- inverse
  
  # This "getter" function returns the values of the inverse  
  getinverse <- function() inverse
  
  # The list below returns the actual "object" when the makeCacheVector
  # function is called. by returning a list, we can access the functions
  # that allow us to manipulate the matrix. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves the inverse of a matrix of a makeCacheInverse
## object and stores it with the object.  If the makeCacheInverse
## already has a solved inverse, it simply returns the inverse

cacheSolve <- function(x, ...) {
  # get the current inverse of the matrix object
  inverse <- x$getinverse()
  
  # Check if the inverse has been solved.  If so, return it.
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) # Exits the function
  }
  
  # If the inverse is NULL (not solved), solve it and set 
  # it to the original makeCacheMatrix object
  
  # Get the actual matrix from the makeCacheMatrix object
  data <- x$get()
  
  # Solve for the inverse of the matrix
  inverse <- solve(data, ...)
  
  # Set the inverse on the original makeCacheMatrix object
  x$setinverse(inverse)
  
  # return the inverse
  inverse
}
