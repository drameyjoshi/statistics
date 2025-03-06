library(utils)
#filename <- "origin_of_species.txt"
filename <- "head.txt"
cnxn <- file(filename, open = "r")
count <- 0
words <- hashtab()

while (TRUE) {
  line <- readLines(cnxn, n = 1L)
  if (length(line) > 0) {
    words_in_line <- strsplit(line, " |-|'")[[1]]
    for (w in words_in_line) {
      w1 <-
        sub("[^[:alpha:]]+$", "", tolower(w)) # Remove trailing comma or fullstop.
      if (is.null(gethash(words, w1))) {
        sethash(words, w1, 1)
      } else {
        sethash(words, w1, gethash(words, w1) + 1)
      }
    }
    count <- count + 1
  } else {
    close(cnxn)
    break
  }
}

# Part A.
words_with_N_or_more_chars <- function(words, N) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    if (nchar(k) >= N) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_1 <- words_with_N_or_more_chars(words, 4)

words_starting_and_ending_with_same_letter <- function(words) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    if (substr(k, 1, 1) == substr(k, nchar(k), nchar(k))) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_2 <- words_starting_and_ending_with_same_letter(words)

words_2nd_letter_same_as_2nd_last <- function(words) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    if (substr(k, 2, 2) == substr(k, nchar(k) - 1, nchar(k) - 1)) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_3 <- words_2nd_letter_same_as_2nd_last(words)

words_with_freq_gt_N <- function(words, N) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    if (v > N) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_4 <- words_with_freq_gt_N(words, 20)

# Part B.
result_5 <- words_with_N_or_more_chars(words, 7)

is_vowel <-
  function(c) {
    result <- FALSE
    if (c == 'a' ||
        c == 'e' || c == 'i' || c == 'o' || c == 'u') {
      result <- TRUE
    }
    
    result
  }

vowels_at_odd_positions <- function(words) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    odd_positions <- rep(c(TRUE, FALSE), ceiling(nchar(k) / 2))
    chars_at_odd_positions <- strsplit(k, "")[[1]][odd_positions]
    if (sum(unlist(lapply(chars_at_odd_positions, is_vowel))) == length(chars_at_odd_positions)) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_6 <- vowels_at_odd_positions(words)

is_consonant <-
  function(c) {
    if (is.na(c)) {
      FALSE
    } else {
      !is_vowel(c)
    }
  }

consonants_at_even_positions <- function(words) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    even_positions <- rep(c(FALSE, TRUE), ceiling(nchar(k) / 2))
    chars_at_even_positions <- strsplit(k, "")[[1]][even_positions]
    
    if (sum(unlist(lapply(
      chars_at_even_positions, is_consonant
    ))) == length(chars_at_even_positions)) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_7 <- consonants_at_even_positions(words)

# Part C.
result_8 <- words_with_N_or_more_chars(words, 6)


in_natural_order <- function(char_vector) {
  start <- 60 # This is one less than charToRaw("a")
  result <- TRUE
  
  for (i in seq_along(char_vector)) {
    c = char_vector[[i]]
    if (is_vowel(c)) {
      if (charToRaw(c) > start) {
        start <- charToRaw(c)
      } else {
        result <- FALSE
        break
      }
    }
  }
  
  result
}

vowels_in_natural_order <- function(words) {
  result <- list()
  i <- 1
  maphash(words, function(k, v) {
    if (in_natural_order(strsplit(k, "")[[1]])) {
      result[[i]] <<- k
      i <<- i + 1
    }
  })
  
  result
}

result_9 <- vowels_in_natural_order(words)

result_10 <- words_with_freq_gt_N(words, 125)