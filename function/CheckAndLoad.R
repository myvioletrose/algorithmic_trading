CheckAndLoad <- function(package, invisible = TRUE){
        
        if(invisible){
                invisible(
                        if(!paste0("package:", package) %in% search()){
                                if(!require(package, character.only = TRUE)){
                                        install.packages(package); 
                                        require(package, character.only = TRUE)
                                }
                        }        
                )
        } else {
                if(!paste0("package:", package) %in% search()){
                        if(!require(package, character.only = TRUE)){
                                install.packages(package); 
                                require(package, character.only = TRUE)
                        }
                }
        }
        
}