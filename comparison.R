install.packages("readxl")
library(readxl)
# Read the first column of each specified Excel file
major_2 <- read_excel("filtered/filtered_data_major_2.xlsx")
major_2[,6]
major_4 <- read_excel("filtered/filtered_data_major_4.xlsx", range = "A1:A100")[[1]]
major_6 <- read_excel("filtered/filtered_data_major_6.xlsx", range = "A1:A100")[[1]]

major_3 <- read_excel("filtered/filtered_data_major_3.xlsx", range = "A1:A100")[[1]]
major_5 <- read_excel("filtered/filtered_data_major_5.xlsx", range = "A1:A100")[[1]]
major_7 <- read_excel("filtered/filtered_data_major_7.xlsx", range = "A1:A100")[[1]]


# Combine and find common strings 3 times and 2 times
common_3_times_major <- Reduce(intersect, list(major_2, major_4, major_6))
common_2_times_major <- unique(c(intersect(major_2, major_4), 
                                 intersect(major_2, major_6), 
                                 intersect(major_4, major_6)))
common_2_times_major <- common_2_times_major[!common_2_times_major %in% common_3_times_major]

# Repeat the process for files 3, 5, 7
common_3_times_minor <- Reduce(intersect, list(major_3, major_5, major_7))
common_2_times_minor <- unique(c(intersect(major_3, major_5), 
                                 intersect(major_3, major_7), 
                                 intersect(major_5, major_7)))
common_2_times_minor <- common_2_times_minor[!common_2_times_minor %in% common_3_times_minor]

# Print results
print("Strings matched 3 times among files 2, 4, 6:")
print(common_3_times_major)

print("Strings matched 2 times among files 2, 4, 6:")
print(common_2_times_major)

# Print results for files 3, 5, 7
print("Strings matched 3 times among files 3, 5, 7:")
print(common_3_times_minor)

print("Strings matched 2 times among files 3, 5, 7:")
print(common_2_times_minor)

# for minors
# Read the first column of each specified Excel file for the minor data
minor_2 <- read_excel("filtered/filtered_data_minor_2.xlsx", range = "A1:A100")[[1]]
minor_4 <- read_excel("filtered/filtered_data_minor_4.xlsx", range = "A1:A100")[[1]]
minor_6 <- read_excel("filtered/filtered_data_minor_6.xlsx", range = "A1:A100")[[1]]

minor_3 <- read_excel("filtered/filtered_data_minor_3.xlsx", range = "A1:A100")[[1]]
minor_5 <- read_excel("filtered/filtered_data_minor_5.xlsx", range = "A1:A100")[[1]]
minor_7 <- read_excel("filtered/filtered_data_minor_7.xlsx", range = "A1:A100")[[1]]

# Combine and find common strings 3 times among minor files 2, 4, 6 and 2 times
common_3_times_minor <- Reduce(intersect, list(minor_2, minor_4, minor_6))
common_2_times_minor <- unique(c(intersect(minor_2, minor_4), 
                                 intersect(minor_2, minor_6), 
                                 intersect(minor_4, minor_6)))
common_2_times_minor <- common_2_times_minor[!common_2_times_minor %in% common_3_times_minor]



# Repeat the process for minor files 3, 5, 7
common_3_times_minor_357 <- Reduce(intersect, list(minor_3, minor_5, minor_7))
common_2_times_minor_357 <- unique(c(intersect(minor_3, minor_5), 
                                     intersect(minor_3, minor_7), 
                                     intersect(minor_5, minor_7)))
common_2_times_minor_357 <- common_2_times_minor_357[!common_2_times_minor_357 %in% common_3_times_minor_357]


# Print results for minor data
print("Strings matched 3 times among minor files 2, 4, 6:")
print(common_3_times_minor)

print("Strings matched 2 times among minor files 2, 4, 6:")
print(common_2_times_minor)

# Print results for minor files 3, 5, 7
print("Strings matched 3 times among minor files 3, 5, 7:")
print(common_3_times_minor_357)

print("Strings matched 2 times among minor files 3, 5, 7:")
print(common_2_times_minor_357)

