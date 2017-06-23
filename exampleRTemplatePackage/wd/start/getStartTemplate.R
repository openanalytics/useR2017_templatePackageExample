# This bit of code gets the path of the start template contained in the 'exampleRTemplatePackage'
# and copy it to the working directory
###############################################################################

# Install package 
# Note: if you want to make use of the templates you might need some or all of the 'suggests' packages as well
# (depending on the requests)
# Load package with template
library(exampleRTemplatePackage)

# Copy child template from the installed package to the working directory
file.copy(from = getPathStartTemplate(), to = "./")

# You can know work/modify the 'startTemplate' file directory towards
# the parameters specific (input parameters/YAML header) of your dataset



