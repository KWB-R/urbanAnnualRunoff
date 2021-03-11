install.packages("rgee", repos = "https://cloud.r-project.org")


rgee::ee_install()


rgee::ee_Initialize(email = "michael.rustler@googlemail.com")

### Downgrade to last one supplied by R package "rgee"
reticulate::py_install('earthengine-api==0.1.248')

### Problem:
###-- rgee 1.0.8 ----------------------------------------------------------- earthengine-api 0.1.248 --
###√ email: xxxx
###√ Initializing Google Earth Engine:
###  Enter Earth Engine Authentication: xxxxx
###  Fehler in py_call_impl(callable, dots$args, dots$keywords) :
###  UnknownApiNameOrVersion: name: earthengine  version: v1alpha

### Fixed by: https://github.com/r-spatial/rgee/issues/136
reticulate::py_install("google-api-python-client==1.12.8")

# Restart R
.rs.restartR() # if you are using Rstudio

#
rgee::ee_Initialize() # it will work now!
