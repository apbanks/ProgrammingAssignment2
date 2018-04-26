## Put comments here that give an overall description of what your
## functions do
###the first function (makeCacheMatrix) sets up a series of functions 
###that can be accessed by a second function (cacheSolve) which computes the inverse of a matrix 
###and writes to the cache



## Write a short comment describing this function
###set - sets the value of the vector to zero hence clearing the cache from the global environment
###get returns the input x
###setinv sets the inv to cache
###get inv returns the inv object
###list returns all of the functions as a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
###first it creats the inv object using the getinv function 
####it then checks to see if it the inv is NULL if not then it returns the data from cache if 
###it is NULL then it proceeds to calculate the inverse and write this to cache. 


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}