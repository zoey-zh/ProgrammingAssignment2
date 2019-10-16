makeCacheMatrix <- function(x = matrix()) {
  # assign NULL to m
  m <- NULL
  set <- function(y) {
    # make x equal to y
    x <<- y
    # make m equal to NULL
    m <<- NULL
  }
  # assign x to 'get'
  get <- function() x
  # compute the inverse and assign the inverse to 'setinverse'
  setinverse <- function(solve) m <<- solve
  # assign m to 'getinverse'
  getinverse <- function() m
  # output the result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # assign 'getinverse' to m
  m <- x$getinverse()
  # if m is not NULL, print the message "getting cached data" and return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if m is NULL, assign 'get' to data
  data <- x$get()
  # compute the inverse of the data and assign the result to m
  m <- solve(data, ...)
  # cache m
  x$setsolve(m)
  m
}
