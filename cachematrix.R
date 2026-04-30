makeCacheMatrix <- function(x = matrix()) {
  # Initialize the variable 'j' as NULL. 
  # This variable will hold the cached inverse of the matrix.
  j <- NULL
  
  # Define the 'set' function to assign a new matrix.
  set <- function(y) {
    # Use the '<<-' operator to assign the value 'y' to 'x' 
    # in the parent environment (lexical scoping).
    x <<- y
    # Since the matrix has changed, reset the cached inverse to NULL.
    j <<- NULL
  }
  
  # Define the 'get' function that returns the matrix 'x'.
  get <- function() x
  
  # Define the 'setInverse' function to store the calculated inverse.
  # It uses '<<-' to save the value in the defining environment.
  setInverse <- function(inverse) j <<- inverse
  
  # Define the 'getInverse' function that returns the stored value of 'j'.
  getInverse <- function() j 
  
  # Return a list containing the four functions defined above.
  # This allows access to the internal methods using the '$' operator.
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Attempt to retrieve the inverse from the cache of object 'x'
  j <- x$getInverse()
  
  # Check if the inverse has already been calculated (i.e., if 'j' is not NULL)
  if(!is.null(j)){
    # If the cache exists, notify the user and return the stored value
    message("getting cached data")
    return(j)
  }
  
  # If the cache is empty (j is NULL), proceed to calculate it:
  
  # 1. Retrieve the original matrix stored in object 'x'
  mat <- x$get()
  
  # 2. Calculate the inverse of the matrix using R's solve() function.
  # The '...' argument allows passing optional parameters to solve().
  j <- solve(mat, ...)
  
  # 3. Store the result of the calculation in the cache of object 'x'
  # so it is available for future requests.
  x$setInverse(j)
  
  # Return the calculated result
  j
}