# Uses the system level find commands
# TODO: 
#	all.files
#	ignore.case
#	no..

find.files <- function(path=".",pattern=NULL,all.files = FALSE,
		full.names = FALSE, recursive = FALSE,
		ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

{
	if(!file.exists(path)) stop("path not found.")
	
	
	if (.Platform$OS=="unix")
	{
		cmd <- paste("find",path,"-name")
		if(!recursive) cmd <- paste(cmd,"-maxdepth 1")
		cmd <- paste(cmd,pattern)
		
	}
	
	if (.Platform$OS=="windows")
	{
		
		if(recursive)
		{
			cd <- paste("cd /d",normalizePath(path),"&")
			cmd <- paste(cd,"dir")
			cmd <- paste(cmd,"/s")
			# Will auto-detect full names, and strip them off later...
			cmd <- paste(cmd,"/b")		
			# We swap in a path separator to allow for subdirectories to be searched.
			pattern_windows  <- paste('"',sub("\\^","\\^.\\*\\\\",pattern),'"',sep="")
			cmd <- paste(cmd,"| findstr /i", pattern_windows)
			suppressWarnings(found_files <- shell(cmd,intern=TRUE))
			if(length(found_files)==0) return("")
			if(!full.names) found_files <- basename(found_files)
		} else
		{
			cd <- paste("cd /d",normalizePath(path),"&")
			cmd <- paste(cd,"dir")
			cmd <- paste(cmd,"/b")
			pattern_windows  <- paste('"',pattern,'"',sep="")
			cmd <- paste(cmd,"| findstr /i", pattern_windows)
			suppressWarnings(found_files <- shell(cmd,intern=TRUE,wait=TRUE,mustWork=NA))
			if(length(found_files)==0) return("")
			if(full.names) found_files <- normalizePath(file.path(path,found_files))
		}
	}
	
	
	
	
}

