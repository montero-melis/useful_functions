# Calculate the percentage you lose when you exchange money back and forth.
# The variable names are thought for SEK and EUR, but works for any currency.

exchange_rate <- function (
  earn_sek = NULL,
  value_eur = NULL,
  thing_sek = NULL,
  thing_eur = NULL
  ) {
  if (is.null(earn_sek)) {
    earn_sek  <- as.numeric(readline(prompt = "How much money do you earn in SEK: "))
  }
  if (is.null(value_eur)) {
    value_eur <- as.numeric(readline(prompt = "How much EUR was put in your account: "))
  }
  if (is.null(thing_sek)) {
    thing_sek <- as.numeric(readline(prompt = "How much did a thing cost you in SEK: "))
  }
  if (is.null(thing_eur)) {
    thing_eur <- as.numeric(readline(prompt = "How much did it cost you in EUR: "))
  }
  cat(sprintf("You could have bought %.2f such things had you not changed your money.\n",
              earn_sek / thing_sek))
  cat(sprintf("But after changing you can only buy %.2f things with your salary.\n",
              value_eur / thing_eur))
  actual_value_sek <- value_eur * thing_sek / thing_eur

  cat(sprintf("Your salary of %.2f SEK is now worth %.2f SEK -- or %.2f%%.\n",
              earn_sek, actual_value_sek, 100 * actual_value_sek / earn_sek))
  cat(sprintf("You will have lost %.2f SEK if you use your full salary this month.",
              earn_sek - actual_value_sek))
}

# Run prompting use for values (or pass them as arguments):
exchange_rate()
