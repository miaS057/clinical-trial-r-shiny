# Load libraries

# For now keep just use server.R with pharmaversesdtm
# Later on we can switch it so they can import a file from user input on the UI

if (!requireNamespace("pharmaverse")) {
  install.packages("pharmaverse")
}
library(pharmaverse)

