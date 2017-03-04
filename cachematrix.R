##assignment number 2 jgm1673
#the point is to cache the inverse of a matrix so as not to have to 
#recalculate it later
## makeCacheMatrix takes a passed matrix, reset inverse, create 4 
##setter/getter functions and returns them in a list

makeCacheMatrix <- function(x = matrix()) {
#first we need to reset the inversematrix m
     m<-NULL
     #next we create the getters and setters
     set <- function(y) { #initialization of two objects, x and m, 
     #x as an empty matrix
        x<<-y
        m<<-NULL #initializing it as an object within the makeCacheMatrix() environment 
                  #to be used by later code in the function
     }
     get <- function() x  #retrieving the matrix from the parent environment
     #assign the input argument to the value of m in
     #the parent environment
     setinv <- function(isolve) m<<-isolve 
     getinv <- function () m
     list(set = set, get=get, 
          # gives the name 'set' to the set() function defined above
          # want named list so we can use the name$set form of the reference
          setinv = setinv,
          getinv = getinv)
}
## cacheSolve populates or retrieves the inverse from an object of type 
## makeCacheMatrix().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <-x$getinv()  #retrieves m and sees if it is NULL, if a valid mean exists, it returns it
    if(!is.null(m)) {
         message("getting cached data")
         return(m)
    }
    #otherwise it calculates the inverse of the input matrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
