#' Meta -----------------------------------------------
#'
#' Project: working with ChatGPT
#' Author: Jared Parrish PhD
#' Date: 7/17/2023
#' 
#' Note: need to connect with a paid openAI API account
#'

library(chatgpt)


# assign your API key for usage in R, this can be done just for the actual session
# generate your key in your usersettings 
# notice on pricing: https://openai.com/pricing#language-models 

Sys.setenv(OPENAI_API_KEY = "yourAPIhere")


### Protecting API Key: https://help.openai.com/en/articles/5112595-best-practices-for-api-key-safety

# Ask a question:
cat(ask_chatgpt("What is the hex code for the color blue?"))

# Comment code
cat(comment_code("
OR<-function(model)
{\n
  dat.lreg.coeffs<-coef(summary(model))
  Lower<-exp(dat.lreg.coeffs[,1] - 1.96*dat.lreg.coeffs[,2])
  OR<-exp(dat.lreg.coeffs[,1])
  Upper<-exp(dat.lreg.coeffs[,1] + 1.96*dat.lreg.coeffs[,2])
  lreg.or<-cbind(Lower,OR,Upper)
  lreg.or
}\n"
))

# Generate code
cat(complete_code("# A function to calculate the odds ratio and 95% confidence intervals from a logistic regression model\nOR_regress <- function("))

# many other things...
# https://github.com/jcrodriguez1989/chatgpt


## With other packages:
# Additional sources: https://www.listendata.com/2023/05/chatgpt-in-r.html#rstudio_addin_for_chatgpt 
#' library("httr")
#' library("jsonlite")



