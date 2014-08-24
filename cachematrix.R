## This function creates a matrix variable that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # initialize the mean to NULL
  set <- function(y) {
    x <<- y # save x as y
    m <<- NULL # reset the mean
  }
  get <- function() x #return x
  setmat<-function(solve) m<<-solve # define a new matrix
  getmat<-function() m # return the inverse of the matrix
  list(set = set, get = get, # collect functions in a list
       setmat = setmat,
       getmat = getmat)
}

# The following function computes the inverse of the matrix variable 
# given by makeCacheMatrix above, if the inverse has already been 
# calculated in memory, the cachesolve finds it from the cache
cachemat <- function(x=matrix, ...) {
  m <- x$getmat()
  if(!is.null(m)) { # get the inverse
    message("getting cached data")
    return(m)  # return the inverse
  }
  matrix <- x$get()  # compute the inverse
  m <- solve(matrix, ...)
  x$setmat(m) 
  m  # return the inverse
}

