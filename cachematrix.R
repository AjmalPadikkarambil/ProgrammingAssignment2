## Put comments here that give an overall description of what your
## functions do

#There are two functions makeCacheMatrix and Cachesolve

#library(MASS) is used to calculate inverse for non squared as well as square matrices

library(MASS)


## Write a short comment describing this function

#makeCacheMatrix consists of set,get,setinv,getinv

makeCacheMatrix <- function(x = matrix()){
  
  #Initializing inverse as NULL  
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  #Function to get a matrix
  
  get <- function()x
  setinv <- function(inverse)inv <<- inverse
  
  #Function to obtain inverse of the matrix
  
  getinv <- function(){
    inver <- ginv(x)
    inver%*%x
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

#This is used to get the cached data

cacheSolve <- function(x, ...) #gets cache data
{
  inv <- x$getinv()
  if(!is.null(inv)){
    
    #Checking wheather inverse is null
    
    message("Getting cache data!")
    return(inv) #returns inverse value
    
  }
  data <- x$get()
  inv <- solve(data, ...) #calculates invers value
  x$setinv(inv)
  inv #return a matrix that is the inverse of 'x'
  
}
