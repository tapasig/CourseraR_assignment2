#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #x_inv has no value at the beginning
  x_inv <- NULL
  
  #set the matrix
  set <- function(y) {
    y <<- x
    x_inv <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #calulate the inverse ??
  # x_inv <- solve(x)
  
  
  #set the inverse
  #setInv() <- function(xi) x_inv <<- xi #should mention the inverse properly
  setInv <- function(inverse) x_inv <<- inverse
  
  #get the inverse
  getInv <- function() x_inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the inverse from previous function 
  inv <- x$getInv()
  
  #check whether it has the inverse   
  if(!is.null(inv)) {
    message("getting inverse from cache")
    return(inv)
  }
  
  #if not, get the data and calculate 
  data <- x$get()
  data
  inv <- solve(data)
  
  #set the inverse
  x$setInv(inv)
  inv
}
