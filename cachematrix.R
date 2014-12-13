## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: Read lines below

makeCacheMatrix <- function(x = matrix()) {
  #m_inv is the matrix inverse, inittialized at null
  m_inv <- NULL
  # set is a function that initializes the matrix
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  #get is a function that gets the matrix itself
  get <- function() x
  #set_m_inv sets the inverse of the matrix
  set_m_inv <- function(mat_inv) m_inv <<- mat_inv
  #get_m_inv returs or gets the inverse of the matrix
  get_m_inv <- function() m_inv
  #this is just to list the components of the function makeCacheMatrix
  list(set = set, get = get,
       set_m_inv = set_m_inv,
       get_m_inv = get_m_inv)
}


## Write a short comment describing this function: Read lines below please
cacheSolve <- function(x,...) {
  #First we look at the cached value
  m_inv <- x$get_m_inv()
  #if there is a cahce value then return it
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  #if it did not return the cached value then find the inverse of the matrix and return it
  data <- x$get()
  m_inv <- solve(data)
  x$set_m_inv(m_inv)
  m_inv
        ## Return a matrix that is the inverse of 'x'
}
