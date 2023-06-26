##########
tools <- data.frame(
  stringsAsFactors = FALSE,
                      Tools = c("Power BI",
                                "Tableau","Google Data Studio","Raw Graphs",
                                "MS Teams","MS Access",
                                "MS Word","MS Excel","MS SharePoint","MS PowerPoint",
                                "Streamit","Docker","Linux","SAS","R",
                                "SQL","Exploratory","Jamovi","JASP","BlueSky",
                                "SPSS","JMP","Minitab","ArcGIS","Origin",
                                "ZOOM","GraphPad Prism","SigmaPlot","WordPress",
                                "Anaconda","Google Sheets","Jupyter","Git",
                                "Mendeley","Plotly"),
                      count = c(75L,70L,50L,
                                60L,100L,50L,100L,100L,70L,100L,50L,90L,
                                100L,100L,100L,55L,100L,100L,100L,75L,60L,
                                75L,75L,76L,62L,100L,80L,65L,95L,90L,
                                55L,100L,80L,95L,80L)
         )

# library
library(wordcloud2) 

# Gives a proposed palette
wordcloud2(tools, size=0.9, color='random-light', backgroundColor = "black")
