# operation: save data retrieved from API into db
save_tbl_action <- function(tbl, df, overwrite = FALSE){

	if(overwrite){
	    DBI::dbWriteTable(
	    	con, 
	    	DBI::SQL(tbl),
	    	df, 
	    	overwrite = TRUE
	    	)
	} else {
	    DBI::dbWriteTable(
	    	con, 
	    	DBI::SQL(tbl),
	    	df,
	    	append = TRUE)
	}

}