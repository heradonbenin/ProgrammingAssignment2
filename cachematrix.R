## In cousera week3 assignement2, we need to write a function that creat a special matrix object 
##that can cache its inverse. Another function called cacheSolve that take as parameter the matrix created by 
## makeCacheMatrix and return the inverted matrix if the cache is not yet exist
##

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##variable to determine the inverted of matrix x in param
  inv_matrix <- NULL
  # Set the matrix itself but not the inverse
  set <- function(y) { 
    x <<- y 
    inv_matrix <<- NULL
  }
  
   
  # Get the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinversed <- function(inversed) inv_matrix <<- inversed
  
  # Get the inverse
  getinversed <- function() inv_matrix
  
  # return a  list of function usable for call during runtime
  list(set = set, get = get,
       setinversed = setinversed,
       getinversed = getinversed)	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversed()
   ##check if inversed exist
  if(!is.null(inv)) { 
    message("Getting cached matrix data")
    return(inv)
  }
  get_makeCacheMatrix <- x$get() 
  inv <- solve(get_makeCacheMatrix, ...) 
  x$setinverse(inv) 
  inv    
  
}
