# trace(utils:::unpackPkgZip, edit = TRUE) # change line 142 from " Sys.sleep(0.5)" to " Sys.sleep(2)", and click save
# trace("unpackPkgZip", where=asNamespace("utils"), edit=TRUE) # change line 142 from " Sys.sleep(0.5)" to " Sys.sleep(2)", and click save
install.packages(c("data.table","readxl","rlang","pillar","tidyverse"))
library(data.table)
library(readxl)
library(rlang)
library(pillar)
library(tidyverse)

x<- 15
y<- 12