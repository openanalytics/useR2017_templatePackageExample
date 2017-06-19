# This bit of code get the start template contained in the 'exampleRTemplatePackage'
# and copy it to the working directory
###############################################################################

# load package with template
library(exampleRTemplatePackage)

# copy child template from the installed package to the working directory
file.copy(from = getPathStartTemplate(), to = "./")

# you can know work/modify the 'startTemplate' file directory towards
# the parameters specific (input parameters/YAML header) of your dataset



