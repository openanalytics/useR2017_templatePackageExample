#' get path of a file contained in the 'templates' folder of the \code{exampleRTemplatePackage} library
#' @param file string, name of the template file
#' @param userType string, use type, either:
#' \itemize{
#' \item{'user': }{the template path is extracted from the installed \code{exampleRTemplatePackage} library}
#' \item{'developer:' }{the name of the template is returned, assuming that the developer works
#' in the 'inst/templates' folder of the package directly
#' }
#' }
#' @return string with output path(s) of the specified file
#' @author Laure Cougnaud and Kirsten Van Hoorde
#' @export
getPathFile <- function(file, userType = c("user", "developer")){
	userType <- match.arg(userType)
	switch(userType,
		'user' =  system.file(file.path("templates", file), package = "exampleRTemplatePackage"),
		'developer' = file
	)
}


#' get path of template
#' @param template string, name of the template file
#' @inherit getPathFile params return
#' @author Laure Cougnaud and Kirsten Van Hoorde
#' @export
getPathTemplate <- function(template, userType = c("user", "developer")){
	getPathFile(file = template, userType = userType)
}


#' get path of start template contained in the \code{exampleRTemplatePackage} package
#' @return path of start template, from the installed \code{exampleRTemplatePackage} library
#' @author Laure Cougnaud and Kirsten Van Hoorde
#' @export
getPathStartTemplate <- function(){
	system.file("templates/start/startTemplate.Rmd", package = "exampleRTemplatePackage")
}

#' Function to create collapse with button in html output
#' @param input the text or function (see type) that should be printed
#' @param id the id of the environment
#' @param titleButton the title of the button 
#' @param type whether the input is a function or text
#' @param color the color of the text in the button 
#' @param borderColor the color of the border of the button
#' @return html code with input in a collapsible environment
#' @author Kirsten Van Hoorde
#' @export
collapseInHtml <- function(input, id, titleButton, type = c("function", "text"), color = "#479ace", borderColor = "#479ace"){
	
	id <- gsub("[(]|[)]|[.]| ", "",id) 
	
	cat(paste0("<button type=\"button\" class=\"btn\" style=\"color:", color, " !important;border-color:",
					borderColor, " !important; background-color: white !important\" id=\"buttonId\" data-toggle=\"collapse\" title=\"", paste0("Click to show or hide ", tolower(titleButton)),"\"  
							data-target=\"#", id, "\">", titleButton, " &nbsp;", "<span class=\"caret\"></span>", "</button>")) 
	cat(paste0("<div id=\"", id, "\" class=\"collapse buttonArrow\">"))
	res <- switch(type, 
			'function' = input(),
			"text" = cat(input))
	cat("\n\n")
	cat("</div>")
	
	if(type == "function")	return(res)	
	
}

#' format the sessionInfo output for markdown
#' (sort packages for each slot)
#' @param order, string, either 'alphabetically' or 'original', 
#' depending if the strings in each slot (e.g. the packages in 'attached base packages') should be
#' sorted alphabetically or if no sorting should be done
#' @param addVersionBioconductor logical, if TRUE (FALSE by default) print also Bioconductor version (BiocInstaller)
#' @return no returned value, the reformatted output of sessionInfo is printed in the current console
#' @importFrom utils capture.output packageDescription packageVersion
#' @author Laure Cougnaud
#' @export
printSessionInfoMarkdown <- function(order = c("alphabetically", "original"), addVersionBioconductor = TRUE){
	
	order <- match.arg(order)
	
	# get ourput of sessionInfo
	sessionInfo <- capture.output(print(sessionInfo()))
	idxEmpty <- which(sessionInfo == "")
	sessionInfo <- sessionInfo[!(1:length(sessionInfo)) %in% idxEmpty]
	
	# idx of elements to paste intop one string
	idxToPaste <- which(grepl("\\[[[:digit:]]{1,}\\]", sessionInfo))
	idxSep <- c(0, which(diff(idxToPaste) != 1), length(idxToPaste))
	# idx of elements to paste
	idxToPasteSplit <- sapply(1:(length(idxSep)-1), function(i) idxToPaste[(idxSep[i]+1):(idxSep[i+1])])
	# paste the elements with ', '
	elPaste <- sapply(idxToPasteSplit, function(i){
		res <- gsub("^ *|\\[[[:digit:]]{1,}\\]| *$", "", sessionInfo[i])
		res2 <- c(sapply(res, function(x){
			res1 <- strsplit(x, split = " ")[[1]]; res1[res1!=""]#paste(res1[res1!=""], collapse = ", ")
		}))
		res2Vector <- unlist(res2)
		paste(
			switch(order, 'alphabetically' = sort(res2Vector), 'original' = res2Vector), 
			collapse = ", ")
	})
	
	# idx of elements to keep from sessionInfo
	idxKept <- which(!(1:length(sessionInfo)) %in% idxToPaste) 
	
	# create the final output
	idxAddedInit <- c(which(diff(idxKept) > 1), length(idxKept)) + 1
	# idx of pasted elements
	idxAdded <- idxAddedInit + 0:(length(idxAddedInit)-1)
	resFinal <- rep("", length(idxKept) + length(idxAdded))
	resFinal[idxAdded] <- elPaste
	# idx of elements kept from the sessionInfo
	resFinal[resFinal == ""] <- sessionInfo[idxKept]
	
	# add list in markdown
	idxList <- idxAdded-1
	resFinal[idxList] <- paste0("* ", resFinal[idxAdded-1], "\n")
	idxNotList <- !(1:length(resFinal)) %in% idxList
	resFinal[idxNotList] <- paste0(resFinal[idxNotList], "\n\n")
	
	# print the result into the console
	cat(resFinal)
	
	
	if(addVersionBioconductor){
		if(!requireNamespace("BiocInstaller", quietly = TRUE)){
			cat("* Bioconductor (BiocInstaller) version:", 
					as.character(packageVersion("BiocInstaller")))
		}
	}

}

#' get prefix for section in markdown ('#') for a number of level
#' @param level integer, level of section, e.g. 2
#' @return string with prefix for section, e.g. '##'
#' @author Laure Cougnaud
#' @export
getPrefixSection <- function(level){
  paste(rep("#", level), collapse = "")
}

#' capitalize the first letter of a word, from the help of the 'toupper' function
#' @param x string
#' @param onlyFirst logical, if TRUE (FALSE by default)
#' capitalize the first letter of the first forward only
#' @param rev logical, if TRUE (FALSE by default), set first letter to lower case (otherwise upper case)
#' @return string with first letter capitalized
#' @author author of the 'toupper' function?
#' @export
simpleCap <- function(x, onlyFirst = FALSE, rev = FALSE) {
  
  paste0c <- function(...) paste(..., sep = "", collapse = " ")
  
  fctToUse <- get(ifelse(rev, "tolower", "toupper"))
  
  simpleCap1 <- function(s) paste0c(fctToUse(substring(s, 1, 1)), substring(s, 2))
  
  sapply(x, function(x){	
        s <- strsplit(x, " ")[[1]]
        if(onlyFirst)	paste0c(c(simpleCap1(s[1]), s[-1]))	else	simpleCap1(s)
      })
  
}