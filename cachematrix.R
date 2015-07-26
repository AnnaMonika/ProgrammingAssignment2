#this function creates a matrix instance represented by and accesses by 
# a list of fields given in a last row of makeCacheMatrix 
# set is a function which sets the matrix tobe inverted
##get is the function which returns the matrix to be inverted
#set is the gunction which keeps track of a current inverse 
#getInv is the function which returns the invertef matrix




makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#this function returns the inverse of the given matrix
#but first checks if there is a need to invert it
#by making sure whether the getInv field of a matrix instance is null or not
# m equal to null means that one needs to invert tha matrix
#otherwise it uses the matrix that was clculated earlier
#inverse is calculated by using 'solve' function 
#after using 'solve' 'm 'is set to this inverse matrix and no longer nulll 
 

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}





