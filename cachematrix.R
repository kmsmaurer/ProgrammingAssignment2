# makeCacheMatrix creates a list containing a function to
    ##set the value of the matrix
    ##get the value of the matrix
    ##set the value of the inverse
    ##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


# cacheSolve  calculates the inverse of the list created in 
# the makeCacheMatrix function. The function:
    ## first checks to see if the inverse has been calculated
    ## returns the inverse from the cache and skips the computation if the inverse has already been called
    ## otherwise, the function calculates the inverse 
    ## then the function sets the value of the inverse in the cache via the setinv function
    ## returns inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Check to see if computation is stored in cache
  m <- x$getinv()
  
  #If inverse is stored in cache, print "getting cached data" and return inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # call the get function
  data <- x$get()
  
  #compute the inverse
  m <- solve(data)
  
  # store in cache
  x$setinv(m)
  
  # return inverse
  m
}
