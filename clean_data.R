pacman::p_load(here,
               tidyverse)

con <- file(here("final","en_US","en_US.twitter.txt"), "r")
readLines(con, 5)
# count the number of lines in total
line_count <- 0
while (length(readLines(con, n = 1000, warn = FALSE)) > 0) {
    line_count <- line_count + 1000
}
close(con)
line_count

# Randomly sample 10% of the lines
set.seed(1)
all_lines <- readLines(con)
sample_size <- round(length(all_lines) * 0.10)
sampled_lines <- sample(all_lines, sample_size)


find_max_line_length <- function(file) {
    con <- file(here("final", "en_US", file), "r")
    message(paste0("Now processing ", file))
    
    # Read all lines from the file
    all_lines <- readLines(con, warn = FALSE)
    
    # Close the file connection
    close(con)
    
    # Find the length of each line
    line_lengths <- nchar(all_lines)
    
    # Get the length of the longest line
    max_length <- max(line_lengths)
    
    return(max_length)
}

# Example usage

find_max_line_length("en_US.blogs.txt")
find_max_line_length("en_US.news.txt")
find_max_line_length("en_US.twitter.txt")

# divide the number of love lines to hate lines
con <- file(here("final","en_US","en_US.twitter.txt"), "r")
all_lines <- readLines(con, warn = FALSE)
close(con)
love_count <- sum(grepl("\\blove\\b",all_lines))
hate_count <- sum(grepl("\\bhate\\b",all_lines))
ratio <- love_count/hate_count

all_lines[grep("\\bbiostats\\b",all_lines)]
that_line_num <- grep("\\bbiostats\\b",all_lines)
all_lines[(that_line_num-3):(that_line_num+3)]

sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",all_lines,fixed = TRUE))

