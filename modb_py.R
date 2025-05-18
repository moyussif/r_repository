
#Setup Python in r__________ Done Once
install.packages("reticulate")
library(reticulate)
#Create a new environment
Version <- "3.13.3"
install_python(version = Version)
virtualenv_create("modib-python", python_version = Version)

use_virtualenv("modib-python", required = TRUE)
virtualenv_install(envname = "modib-python", "numpy", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "panda", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "matplotlib", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "seaborn", ignore_installed = FALSE, pip_options = character())
# Machine Learning
virtualenv_install(envname = "modib-python", "Scikit-Learn", ignore_installed = FALSE, pip_options = character())

#  #  #
_________________________________________________________________________________________________________________
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#To use Python run the codes below
library(reticulate)
use_virtualenv("modib-python", required = TRUE)
# install packages
virtualenv_install(envname = "modib-python", "numpy", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "panda", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "matplotlib", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "seaborn", ignore_installed = FALSE, pip_options = character())

#import data for analysis