## Install VROOM package
install.packages("vroom") # CRAN

# install.packages("devtools")
devtools::install_dev("vroom") # The development version from GitHub

setwd("C:/NSPP_Workshop/Data") # Set the working directory 

library(vroom) # Load the vroom package

files <- fs::dir_ls(glob = "*csv") # Read all the file 
files # Check that the appropriate files are read

all_CSV <- vroom::vroom(files)

dim(all_CSV) # Check the dimension of the read tibble
View(all_CSV)

