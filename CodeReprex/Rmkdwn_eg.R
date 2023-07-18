#' ---
#' title: "Rendering a basic R script"  
#' author: "Jared Parrish"  
#' date: "September 11, 2020"  
#' output: github_document
#' ---
#'
#' When we add the special characters and select our output style we can render markdowns to GitHub.
#' #' to knit this we can select File > Knit Document or Cntl+Shift+K  
#' For this example we will use the VADeaths dataset 
#' dataset `VADeaths`. 
print(VADeaths)
## We can add comments to the code directly - Let's summarize these data.
summary(VADeaths)

#' We can use usual markdown syntax to make things
#' **bold** or *italics*. We can even make inline statments. Let's use an example`dotchart()`
#' from the `VADeaths` data to add a visual.
#' 
#+ dotchart
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")

