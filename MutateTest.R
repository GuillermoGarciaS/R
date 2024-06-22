library(dplyr)

df <- data.frame(x = 1:5, y = 6:10)
print(df)

df <- mutate(df, x = x^2)
print(df)