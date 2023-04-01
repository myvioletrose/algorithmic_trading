# operation: delete symbol(s) from YYYY-MM-DD
delete_action <- function(tbl, symbol = NA, delete_time_from = NA, delete_time_to = NA, confirm_to_delete = FALSE){

	if(confirm_to_delete){
		t = tbl		
		if(is.na(delete_time_to)){delete_time_to = Sys.Date()}
		if(symbol %>% paste0(., collapse = ", ") == "NA" && is.na(delete_time_from)){
			d(glue::glue("delete from {t}"))
		} else {
			if(symbol %>% paste0(., collapse = ", ") == "NA"){symbol = d(glue("select distinct symbol from {tbl}")) %>% .$symbol}
			s = paste0(sapply(symbol, function(x){paste0("'", x, "'")}), collapse = ", ")
			if(is.na(delete_time_from)){
				d(glue::glue("delete from {t} where symbol in ({s})"))			
			} else {
				d(glue::glue("delete from {t} where symbol in ({s}) and date >= '{delete_time_from}' and date <= '{delete_time_to}'"))
			}
		}
	} else {
		print("please set confirm_to_delete to TRUE should you confirm and go ahead to delete the data from the underlying table")
	}
	
}