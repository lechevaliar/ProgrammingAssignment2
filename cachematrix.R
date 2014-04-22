## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # cache
  set <- function(y) { #set a vector a$set(c(1,2,3,4,5,6,7))vv
    x <<- y            #and store it in x and clear cache m
    m <<- NULL
  }
  get <- function() x #  grabs whatever vector might be stored in x and returns it
  setinv <- function(inv) m <<- inv # it takes a vector passed into it and stores it in m
  getinv <- function() m # return cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() # get a's vector
  m <- solve(data, ...) # calculate mean
  x$setinv(m) # store the neab into a's cache
  m
}
