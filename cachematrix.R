
## a function to set a matrix with cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # cache
  set <- function(y) { #set a matrix
    x <<- y            #and store it in x and clear cache m
    m <<- NULL
  }
  get <- function() x #  grabs whatever matrix might be stored in x and returns it
  setinv <- function(inv) m <<- inv # it takes a matrix passed into it and stores it in m
  getinv <- function() m # return cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## a function to return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() # get a's matrix
  m <- solve(data, ...) # solve for the inverse (assume a matrix is always invertible)
  x$setinv(m) # store the inverse into cache
  m
}
