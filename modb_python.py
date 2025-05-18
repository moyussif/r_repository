#To use Python run the codes below
library(reticulate)
use_virtualenv("modib-python", required = TRUE)
# install packages
virtualenv_install(envname = "modib-python", "numpy", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "panda", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "matplotlib", ignore_installed = FALSE, pip_options = character())
virtualenv_install(envname = "modib-python", "seaborn", ignore_installed = FALSE, pip_options = character())

# import data for analysis
