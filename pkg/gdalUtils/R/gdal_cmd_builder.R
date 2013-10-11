#' @export

gdal_cmd_builder <- function(executable,parameter_variables,parameter_values,parameter_order,parameter_noflags)
{
	# path to executable check in here?
	
	executable <- gdal_get_executable(executable)
	
	parameter_variables_types <- names(parameter_variables)
	defined_variables <- names(parameter_values)[sapply(parameter_values,function(X) class(X) != "name")]
	
	if(any("logical" %in% parameter_variables_types))
	{
		parameter_variables_logical <- parameter_variables$logical[[1]]
		parameter_variables_logical_defined <- defined_variables[defined_variables %in% parameter_variables_logical]
		# Only set the flag if TRUE
		if(length(parameter_variables_logical_defined)>0)
		{
			parameter_variables_logical_defined_true <- sapply(parameter_variables_logical_defined,
					function(X,parameter_values)
					{
						return(parameter_values[[which(names(parameter_values)==X)]])
					},parameter_values=parameter_values)
			parameter_variables_logical_strings <- paste("-",
					parameter_variables_logical[parameter_variables_logical_defined_true],sep="")
		} else
		{
			parameter_variables_logical_strings <- NULL
		}
		
	}
	
	if(any("vector" %in% parameter_variables_types))
	{
		parameter_variables_vector <- parameter_variables$vector[[1]]
		parameter_variables_vector_defined <- defined_variables[defined_variables %in% parameter_variables_vector]
		if(length(parameter_variables_vector_defined)>0)
		{
			parameter_variables_vector_strings <- sapply(parameter_variables_vector_defined,
					function(X,parameter_values)
					{
						if(X %in% parameter_noflags)
						{
							flag=NULL
						} else
						{
							flag=paste("-",X," ",sep="")
						}
						parameter_variables_vector_string <- paste(flag,
								qm(paste(parameter_values[[which(names(parameter_values)==X)]],collapse=" ")),
								sep="")
						return(parameter_variables_vector_string)
					},parameter_values=parameter_values)			
		} else
		{
			parameter_variables_vector_strings <- NULL
		}
	}
	
	if(any("scalar" %in% parameter_variables_types))
	{
		parameter_variables_scalar <- parameter_variables$scalar[[1]]
		parameter_variables_scalar_defined <- defined_variables[defined_variables %in% parameter_variables_scalar]
		if(length(parameter_variables_scalar_defined)>0)
		{
			parameter_variables_scalar_strings <- sapply(parameter_variables_scalar_defined,
					function(X,parameter_values)
					{
						if(X %in% parameter_noflags)
						{
							flag=NULL
						} else
						{
							flag=paste("-",X," ",sep="")
						}
						parameter_variables_scalar_string <- paste(flag,
								qm(parameter_values[[which(names(parameter_values)==X)]]),
								sep="")
						return(parameter_variables_scalar_string)
					},parameter_values=parameter_values)			
		} else
		{
			parameter_variables_scalar_strings <- NULL
		}
	}
	
	if(any("character" %in% parameter_variables_types))
	{
		# Do we need to embed quotes in the command?
		parameter_variables_character <- parameter_variables$character[[1]]
		parameter_variables_character_defined <- defined_variables[defined_variables %in% parameter_variables_character]
		if(length(parameter_variables_character_defined)>0)
		{
			parameter_variables_character_strings <- sapply(parameter_variables_character_defined,
					function(X,parameter_values,parameter_noflags)
					{
						if(X %in% parameter_noflags)
						{
							flag=NULL
						} else
						{
							flag=paste("-",X," ",sep="")
						}
						parameter_variables_character_string <- paste(flag,
								qm(parameter_values[[which(names(parameter_values)==X)]]),
								sep="")
						return(parameter_variables_character_string)
					},parameter_values=parameter_values,parameter_noflags=parameter_noflags)			
		} else
		{
			parameter_variables_character_strings <- NULL
		}
	}
	
	if(any("repeatable" %in% parameter_variables_types))
	{
		parameter_variables_repeatable <- parameter_variables$repeatable[[1]]
		parameter_variables_repeatable_defined <- defined_variables[defined_variables %in% parameter_variables_repeatable]
		if(length(parameter_variables_repeatable_defined)>0)
		{
			parameter_variables_repeatable_strings <- sapply(parameter_variables_repeatable_defined,
					function(X,parameter_values)
					{
						if(X %in% parameter_noflags)
						{
							flag=NULL
						} else
						{
							flag=paste("-",X," ",sep="")
						}
						parameter_variables_repeatable_string <- paste(
								paste(flag,
										qm(parameter_values[[which(names(parameter_values)==X)]]),
										sep=""),
								collapse=" ")
						return(parameter_variables_repeatable_string)
					},parameter_values=parameter_values)			
		} else
		{
			parameter_variables_repeatable_strings <- NULL
		}
	}
	
	if(!is.null(parameter_noflags))
	{
#		parameter_variables_noflag <- parameter_variables$noflag[[1]]
#		parameter_variables_noflag_defined <- defined_variables[defined_variables %in% parameter_variables_noflag]
#		if(length(parameter_variables_noflag_defined)>0)
#		{
			parameter_variables_noflag_strings <- sapply(parameter_noflags,
					function(X,parameter_values)
					{
						parameter_variables_noflag_string <- paste(
								parameter_values[[which(names(parameter_values)==X)]],
								sep="")
						return(parameter_variables_noflag_string)
					},parameter_values=parameter_values)			
#		} else
#		{
#			parameter_variables_noflag_strings <- NULL
#		}
	}

	parameter_vector <- c(
			parameter_variables_logical_strings,
			parameter_variables_vector_strings,
			parameter_variables_scalar_strings,
			parameter_variables_character_strings,
			parameter_variables_repeatable_strings,
			parameter_variables_noflag_strings
	)
	
	# Reorder the parameters if neccessary
	if(!missing(parameter_order))
	{
		parameter_order_defined <- parameter_order[which(parameter_order %in% names(parameter_vector))]
		parameter_vector <- parameter_vector[parameter_order_defined]
	}
	
	cmd <- paste(c(qm(executable),parameter_vector),collapse=" ")
	
	return(cmd)
	
}