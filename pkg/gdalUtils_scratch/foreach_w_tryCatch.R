

e <- simpleError("test error")

iterations <- 1:5

foreach(i=iterations,.errorhandling=c("remove")) %dopar%
		{
			stop(e)
		}