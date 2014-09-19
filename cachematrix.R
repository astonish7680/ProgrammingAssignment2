## Author:Levi Duran Torres
## Program: cachematrix.R


# This function will create a list of functions a pair for getting and setting the reference of the matrix
# and the other pair for getting and setting the reference of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #This reference will be the value of the inverse matrix
  m <- NULL
  
  #Set function to set the value of the matriz to be inverted
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get function to get the value of the matriz to be inverted
  get <- function() x
  
  #Set function to set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  #Get function to get the inverse of the matrix
  getinverse <- function() m
  
  #Create the list wit the above functions as elements  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will calculate and send to console the inverse of the  matrix  'x' 
## it will check whether to get the inverse from cache or calculate it it hasn't calculated it before
cacheSolve <- function(x, ...) {
  
  #This will hold the reference to the inverse of the matrix
  m <- x$getinverse()
  
  #Check to see whether the inverse has alreayd been calculated or  not
  if(!is.null(m)) {
    
    #If it has been calculated return the cache inverse
    message("getting cached data")
    return(m)
    
  }
  
  #If not get the reference to the matrix and calculate its inverse (in this case we only need one argument to get the inverse)
  data <- x$get()
  m <- solve(data)
  
  #Update the reference of the matrix to its new calculated inverse
  x$setinverse(m)
  
  #Send the result to console
  m
  
}
