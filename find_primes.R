# find all prime numbers from 1 to N
find_primes <- function(N) {

	search <- function(curr_prime, candidates) {
		exclude <- seq(from = curr_prime, to = max(candidates), by = curr_prime)
		new_candidates <- candidates[! candidates %in% exclude]
		new_candidates
	}

	primes <- 2
	curr_prime <- 2
	potentials <- 2:N

	while(length(potentials) != 0) {
		potentials <- search(curr_prime, potentials)
		if (length(potentials) != 0) {
			curr_prime <- potentials[1]
			primes <- c(primes, curr_prime)
		}
	}

	primes
}


# verbose version
find_primes_verbose <- function(N) {

	search <- function(curr_prime, candidates) {
		cat("current prime:\n")
		print(curr_prime)
		cat("candidates:\n")
		print(candidates)
		exclude <- seq(from = curr_prime, to = max(candidates), by = curr_prime)
		cat("exclude:\n")
		print(exclude)
		new_candidates <- candidates[! candidates %in% exclude]
		new_candidates
	}

	primes <- 2
	curr_prime <- 2
	potentials <- 2:N

	while(length(potentials) != 0) {
		potentials <- search(curr_prime, potentials)
		if (length(potentials) != 0) {
			curr_prime <- potentials[1]
			primes <- c(primes, curr_prime)
		}
		cat("current prime list:\n")
		print(primes)
		cat("\n")
	}
	primes
}
