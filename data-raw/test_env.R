library(reticulate)
use_virtualenv('/opt/my_env',T)
py_config()
rdlaiye <- import("rdlaiye")
res <-rdlaiye$api
