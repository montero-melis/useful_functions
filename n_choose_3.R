# n choose 3
n3 <- function(n) { fact(n) / (fact(3) * fact(n - 3)) }

# factorial
fact <- function(n) {
  if (n < 0) {
    stop("Enter a non-negative integer!")
  } else if (n == 0 | n == 1) {
    return(1)
  } else {
    out <- as.numeric(n)  # avoid integer overflow
    for (i in 2:(n - 1)) {
      out <- out * i
    }
  }
  out
}

# Examples
fact(4)
for(i in 0:20) {
  print(i)
  print(fact(i))
}

n3(5)
for (i in 10:30) {
  print(paste("for n =", i))
  print(n3(i))
}
