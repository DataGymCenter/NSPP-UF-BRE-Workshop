######
library(readxl) 
# require(readxl)
library(rio) ## install_formats()
library(dplyr)

#####
setwd("C:/NSPP_Workshop/Data")

### IMPORT MULTIPLE EXCEL SHEETS
all_oly <- import_list("olympics_multiple_sheets.xlsx", which = c(1:5), rbind = TRUE) # Numbers refer to sheet position ### , which = c(1,2,3,4,5)

dim(all_oly)
#View(all_oly)

### Sheet name reference
all_oly2 <- import_list("olympics_multiple_sheets.xlsx", which = c("Sheet1","Sheet2","Sheet3","Sheet4","Sheet5"), rbind = TRUE)

### IMPORT MULTIPLE EXCEL FILES (from readxl package)
paths = "C:/NSPP_Workshop/Data/XLSX_test/"

##
files_XL <- list.files(path = paths, pattern = "*.xlsx", full.names = T); files_XL

(ALL_EXCEL_FILES <- sapply(files_XL, read_excel, simplify=FALSE) %>% bind_rows())


###
library(ggplot2)
ggplot(ALL_EXCEL_FILES) +
  aes(x = Medal, weight = Height) +
  geom_bar(fill = "#440154") +
  #coord_flip() +
  ggthemes::theme_economist()

### IMPORT MULTIPLE CSV FILES (from readr package)
library(readr)
files_CSV <- list.files(path = paths, pattern = "*.csv", full.names = T)

(ALL_CSV_FILES <- sapply(files_CSV, read.csv, simplify=FALSE) %>% bind_rows())

#### VROOM # install.packages("vroom")
library(vroom)
path_csv = "C:/NSPP_Workshop/Data/CSV_test/"
files <- fs::dir_ls(path_csv, glob = "*csv"); files

## VROOMING it
ALL_vroomed <- vroom(files)

##
ALL_vroomed <- vroom(files, col_select = c(Sex, Age, Team, Medal, Year, Season, City,  Sport)) #%>%
#select(Sex, Age)


library(readxl)
FIFA_2018 <- read_excel("FIFA_2018.xlsx", 
                        sheet = "Sheet1")

####
soft_expt <- data.frame(
  stringsAsFactors = FALSE,
          Software = c("SAS","R","SPSS","Matlab",
                       "JASP","Jamovi","Origin","JMP","Excel","PowerPoint"),
          Expertise = c(75L, 95L, 65L, 35L, 100L, 50L, 45L, 82L, 100L, 100L)
)

###
# library
library(wordcloud2) 

# Gives a proposed palette
wordcloud2(soft_expt, size=0.7, color='random-light', backgroundColor = "black", fontFamily = "Times new roman")


library(ggplot2)
ggplot(soft_expt) +
  aes(x = Software, weight = Expertise) +
  geom_bar(fill = "#B22222") +
  labs(
    x = "\nSoftware I can use",
    y = "My level of expertise\n",
    title = "My new barplot here!\n"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 25L, hjust = 0.5),
        axis.text.x = element_text(size=16, angle = 90, vjust =0.5 , hjust = 1),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=22),
        axis.title.y = element_text(size=22))


###
ggplot(FIFA_2018) +
  aes(x = team, weight = age) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  facet_grid(vars(position), vars()) +
  theme(
    axis.text.x = element_text(size=16, angle = 90, vjust =0.5 , hjust = 1)
  )






