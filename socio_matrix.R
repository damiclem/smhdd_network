# Dependencies
library(tidyverse)


# Load datasets

# Load users
users <- read_csv('data/users.csv', col_types='ncncccnclcnnn') %>%
  filter(!is.na(id)) %>%
  distinct

# Load friendship relationships
friendship <- read_csv('data/friendship.csv', col_types='cc')


# Get all available universities
uni <- users %>% 
  select(id) %>% 
  distinct %>% 
  pull
# Get universities array info
str(uni)


# Instantiate new empty matrix
socio.matrix <- matrix(0, nrow=length(uni), ncol=length(uni))

# Fill the matrix
# Loop through every row
for(i in 1:length(uni)) {
  # Get i-th university id
  i.id <- uni[i]
  # Get i-th university friends (followed by)
  i.friends <- friendship %>%
    filter(from == i.id & from != to) %>%
    select(to) %>%
    pull
  # Loop through every column
  for(j in 1:length(uni)) {
    # Get j-th university id
    j.id <- uni[j]
    # Check if i-th university follows the j-th one
    socio.matrix[i, j] <- ifelse(j.id %in% i.friends, 1, 0)
  }
}

# Rename matrix rows and columns with universities ids
colnames(socio.matrix) <- uni
rownames(socio.matrix) <- uni

# Save the socio matrix to disk
out.file <- 'data/socio_matrix.RData' # Define output file path
save(socio.matrix, file=out.file)  # Copy matrix to disk

