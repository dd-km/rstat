
#remove.packages("rstat")
#install.packages(c("devtools", "roxygen2", "usethis"))

#Potrebni alati 
library(usethis)
library(devtools)

#Instalacija uz mape paketa
document()
install()
library(rstat)
help(package="rstat") 

#Instalacija sa GITHUB-a
install_github("dd-km/rstat")
library(rstat)
help(package="rstat") 



