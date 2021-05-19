## Put comments here that give an overall description of what your
## functions do

## make CacheMatrix: create a list of 4 function
##[get, set, setInvMatrix, getInvMatrix]
##this function allow to set a matrix into a cache memory and obtain it from 
##get fuction; furthermore, working in conjunction with cacheSolve function,
## Allow to calc the inverse matrix chached, and save the calculation into cache
## makeCacheMatrix wich could be accecible throgh methods: getInvMatrix...

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(solve) m <<- solve
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
  
}


## This function complement the MakeCacheMatrix: take from cache 
##(list from MakecacheMatrix).
##check if is stored an matrix inverse, else calculate the inverse of 
##the matrix cached...

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}