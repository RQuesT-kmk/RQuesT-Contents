
# About -------------------------------------------------------------------

#' Author: Kaung Myat Khant
#' Date: 17 Jan 2026
#' Data source: General social survey 2022, marrital status vs happiness results
#' What: This file is used to simulate a dataset of 10000 rows





# 1. Set seed for reproducibility
set.seed(123) 

# 2. Define sample size and variable levels
n <- 10000
sexes <- c("Female", "Male")
status <- c("Married", "Unmarried")
kids <- c("Yes", "No")
happy_levels <- c("Very Happy", "Pretty Happy", "Not Too Happy")

# 3. Simulate the independent demographic columns uniformly
df <- data.frame(
    age = sample(18:55, n, replace = TRUE),              # Sample ages 18-55
    sex = sample(sexes, n, replace = TRUE),              # Randomly assign Sex
    marital_status = sample(status, n, replace = TRUE),  # Randomly assign Marital Status
    having_children = sample(kids, n, replace = TRUE),   # Randomly assign Children status
    happiness = NA                                       # Initialize empty target column
)

# 4. Define a helper function to assign happiness based on group logic
# This keeps the code clean and avoids repetitive blocks
assign_happiness <- function(data, sex_val, mar_val, kid_val, probs) {
    # Find rows matching the specific demographic combination
    idxs <- which(data$sex == sex_val & 
                      data$marital_status == mar_val & 
                      data$having_children == kid_val)
    
    # Sample happiness for these specific rows using the provided weights
    # note: 'sample' automatically normalizes probs if they don't sum exactly to 1
    return(sample(happy_levels, length(idxs), replace = TRUE, prob = probs))
}

# 5. Apply probabilities for Married groups (Women/Men, Kids/No Kids)
# Married Women w/ Children
df$happiness[df$sex == "Female" & df$marital_status == "Married" & df$having_children == "Yes"] <- 
    assign_happiness(df, "Female", "Married", "Yes", c(39.5, 47.6, 12.9))

# Married Men w/ Children
df$happiness[df$sex == "Male" & df$marital_status == "Married" & df$having_children == "Yes"] <- 
    assign_happiness(df, "Male", "Married", "Yes", c(35.0, 49.3, 15.7))

# Married Women No Children
df$happiness[df$sex == "Female" & df$marital_status == "Married" & df$having_children == "No"] <- 
    assign_happiness(df, "Female", "Married", "No", c(24.7, 59.3, 16.0))

# Married Men No Children
df$happiness[df$sex == "Male" & df$marital_status == "Married" & df$having_children == "No"] <- 
    assign_happiness(df, "Male", "Married", "No", c(29.8, 51.1, 20.2))

# 6. Apply probabilities for Unmarried groups
# Unmarried Women No Children
df$happiness[df$sex == "Female" & df$marital_status == "Unmarried" & df$having_children == "No"] <- 
    assign_happiness(df, "Female", "Unmarried", "No", c(21.5, 53.8, 24.6))

# Unmarried Men No Children
df$happiness[df$sex == "Male" & df$marital_status == "Unmarried" & df$having_children == "No"] <- 
    assign_happiness(df, "Male", "Unmarried", "No", c(13.8, 64.3, 21.9))

# Unmarried Women w/ Children
df$happiness[df$sex == "Female" & df$marital_status == "Unmarried" & df$having_children == "Yes"] <- 
    assign_happiness(df, "Female", "Unmarried", "Yes", c(16.7, 60.6, 22.7))

# Unmarried Men w/ Children
df$happiness[df$sex == "Male" & df$marital_status == "Unmarried" & df$having_children == "Yes"] <- 
    assign_happiness(df, "Male", "Unmarried", "Yes", c(11.9, 51.4, 36.7))

# 7. Convert character columns to factors for easier statistical analysis later
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)

# View first few rows
head(df)

# Optional: Verify the distribution for one group (e.g., Married Women w/ Children)
# Result should be close to: Very Happy ~39.5%
subset_check <- subset(df, sex == "Female" & marital_status == "Married" & having_children == "Yes")
prop.table(table(subset_check$happiness))

saveRDS(df,file = "general_social_survey_MarriedvsHappy_2022_simulated.rds")
