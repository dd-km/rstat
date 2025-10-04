
remove.packages("rstat")

#Instalacija R paketa

#install.packages(c("devtools", "roxygen2", "usethis"))

library(usethis)
library(devtools)
document()
install()
library(rstat)
help(package="rstat") 

# Instalacija sa GITHUB-a
install_github("dd-km/rstat")



