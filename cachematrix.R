## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#takes an input matrix x and converts it into a makeCacheMatrix type object that allows for 
#calculating and caching the 
makeCacheMatrix <- function(x = matrix()) {
  i<<-NULL #setting i as NULL for a given input, this will have the inverse
  set<- function(y){
    x<<-y#setting x's value in the parent environment
    i<<-NULL#setting i value in the parent environment
  }
  get<- function() x
  setinverse<-function(solve) i<<- solve #calculates inverse
  getinverse<-function() i #returning the calculated inverse
  list(set=set,#gives the name set to set() defined above and so on for below functions
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<<-x$getinverse() #i gets its calue from the getinverse function of the MakeCacheMatrix object
  if(!is.null(i)){  # check if i is NOT a NULL value
    message("getting cached data") #lets you know data being fetched is cached
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)#sets i as the inverse value in the object x
  i #return the inverse
}

