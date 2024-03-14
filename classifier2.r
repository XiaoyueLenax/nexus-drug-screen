files <- c("Tecan_m1000/1_Fluo_mCherry/20240304-131356_240229MR_2.xlsx",
           "Tecan_m1000/3_Lum_Minor/20240304-140944_240229MR_2.xlsx",
           "Tecan_m1000/2_Lum_Major/20240304-132818_240229MR_2.xlsx")
# ============================ Library prep ====================================
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
library(openxlsx)
library(readxl)
library(dplyr)
library(writexl)
# ==============================================================================

# To replace: ------------------------------------------------------------------
# Control + f to replace these labels with the dataset you are currently doing
filtered_data_major_2
input_data_2
filtered_data_major_2
combined_data_2.xlsx
x_fold_2.xlsx
filtered_data_minor_2_strict.xlsx
filtered_data_major_2_strict.xlsx
# ------------------------------------------------------------------------------
# Initialize an empty list to store data frames
data_list <- list()

# Loop through the files and read each one, adding the data frame to the list
for (file in files) {
  data_list[[file]] <- read_excel(file)
}
#----------------------- Optional Adjustments ----------------------------------
# Find the maximum number of rows among all data frames
max_rows <- max(sapply(data_list, nrow))
# Adjust each data frame to have the same number of rows by adding 
# NA rows if necessary
adjusted_data_list <- lapply(data_list, function(df) {
  nr <- nrow(df)
  if(nr < max_rows) {
    # Create a data frame of NAs with the correct number of missing rows and the 
    # same column names as df
    na_df <- as.data.frame(matrix(ncol = ncol(df), nrow = max_rows - nr))
    names(na_df) <- names(df)
    # Combine df with na_df
    df <- rbind(df, na_df)
  }
  return(df)
})
#-------------------------- end of optional adjustements -----------------------

# Combine all adjusted data frames side by side
input_data_2 <- do.call(cbind, adjusted_data_list)
# Write the combined data to a new Excel file
#input_data_2 <- as.data.frame(input_data_2)
write_xlsx(input_data_2, "combined_data_2.xlsx")

input_data_2 <- read_excel("combined_data_2.xlsx", sheet = 1)
head(input_data_2[,3])
# First, calculate the CMSO ctr 
input_data_2[,11]
# First, divide the value with mcherry
input_data_2[,11][input_data_2[,11] < 0] <- NA
input_data_2[,3][input_data_2[,3] < 0] <- NA


input_data_2$calc_column_minor <-input_data_2[,7] / input_data_2[,3]
input_data_2$calc_column_major <- input_data_2[,11] / input_data_2[,3]

head(input_data_2$calc_column_minor)
head(input_data_2$calc_column_major)

# Now look at the controls: A2, D23, D24, H23, H24, I1, I2, L23,L24,M1,M2,P23,P24
input_data_2[287,] # This should return L24
A2 <- input_data_2$calc_column_minor[1,]
a2 <- input_data_2$calc_column_major[1,]

D23 <- input_data_2$calc_column_minor[94,]
D24 <- input_data_2$calc_column_minor[95,]
D <- mean(D23,D24)
d23 <- input_data_2$calc_column_major[94,]
d24 <- input_data_2$calc_column_major[95,]
d <- mean(d23,d24)

H23 <- input_data_2$calc_column_minor[190,]
H24 <- input_data_2$calc_column_minor[191,]
H <- mean(H23,H24)
h23 <- input_data_2$calc_column_major[190,]
h24 <- input_data_2$calc_column_major[191,]
h <- mean(h23,h24)

I1 <- input_data_2$calc_column_minor[192,]
I2 <- input_data_2$calc_column_minor[193,]
I <- mean(I1, I2)
i1 <- input_data_2$calc_column_major[192,]
i2 <- input_data_2$calc_column_major[193,]
i <- mean(i1, i2)

L23 <- input_data_2$calc_column_minor[286,]
L24 <- input_data_2$calc_column_minor[287,]
L <- mean(L23, L24)
l23 <- input_data_2$calc_column_major[286,]
l24 <- input_data_2$calc_column_major[287,]
l <- mean(l23, l24)

M1 <- input_data_2$calc_column_minor[288,]
M2 <- input_data_2$calc_column_minor[289,]
M <- mean(M1, M2)
m1 <- input_data_2$calc_column_major[288,]
m2 <- input_data_2$calc_column_major[289,]
m <- mean(m1, m2)

P24 <- input_data_2$calc_column_minor[383,]
P23 <- input_data_2$calc_column_minor[382,]
P <- mean(P23, P24)
p24 <- input_data_2$calc_column_minor[383,]
p23 <- input_data_2$calc_column_minor[382,]
p <- mean(p23, p24)

control_minor <- mean(A2,D,H,I,L,M,P)
control_major <- mean(a2,d,h,l,m,p)
control_major;control_minor

# Calculate x-fold , which is 1/control*calc_column_minor
head(input_data_2$calc_column_minor)
head(input_data_2$calc_column_major)

# Calculation for minor and major
input_data_2$x_fold_minor <- 1 / control_minor * input_data_2$calc_column_minor 
input_data_2$x_fold_major <- 1/ control_major * input_data_2$calc_column_major
head(input_data_2$x_fold_major)
head(input_data_2$x_fold_minor)
str(input_data_2)
# Assuming the calculations for these columns are done correctly above, 
# adjust the assignment:
input_data_2$calc_column_minor <- input_data_2$calc_column_minor[[1]]
input_data_2$calc_column_major <- input_data_2$calc_column_major[[1]]
input_data_2$x_fold_minor <- input_data_2$x_fold_minor[[1]]
input_data_2$x_fold_major <- input_data_2$x_fold_major[[1]]
# After correcting the structure, proceed to write the data frame to an Excel file
write_xlsx(input_data_2, "combined_data_2.xlsx")

# ======================== Strict parameters ===================================
filtered_data_minor_2 <- input_data_2[input_data_2$x_fold_minor <= 0.7 & 
                                        input_data_2$x_fold_major >= 1, ]
filtered_data_major_2 <- input_data_2[input_data_2$x_fold_minor >= 1 & 
                                        input_data_2$x_fold_major <= 0.5, ]

write_xlsx(filtered_data_major_2, "filtered_data_major_2_strict.xlsx")
write_xlsx(filtered_data_minor_2, "filtered_data_minor_2_strict.xlsx")
# ========================= end of strict ======================================
# Not strict criteria: fits the strict-ness in at least one dataset, and at least
# shows decrease in itself and above 1 for the other in the rest 2.
# To do this, need a combined data of all. 