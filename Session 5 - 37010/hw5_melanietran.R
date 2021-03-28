#Melanie Tran
#MSCA 37010 - Homework 5.7

newton.raphson <- function(f, x0, iter=1e5, epsilon=1e-10) {
  f.prime <- function(f, x = x.old, h=1e-5) {
    return((f(x + h) - f(x - h)) / (2 * h))
  }
  x.old <- x0
  x.new <- x.old - (f(x.old) / f.prime(f,x.old))
  
  while ((abs(x.new - x.old)) > epsilon) {
    # Do Newton Raphson update
    x.old <- x.new
    x.new <- x.old - (f(x.old) / f.prime(f,x.old))
  }
  return (x.new)
}


# Define some function to test
f <- function(x) {
  return(x^2 - 4 * x - 7)
}

newton.raphson(f,0)
newton.raphson(f,3)

# Define another function to test
g <- function(x) {
  return(cos(x))
}

newton.raphson(g, 1)
newton.raphson(g, 4)




