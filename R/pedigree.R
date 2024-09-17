### Pedigree ###################################################################

#' Get Direct Parents
#'
#' @description
#' Utility function to split a given pedigree string and retrieve the pedigrees of the direct parents (female and male).
#' The function handles different formats of cross representations, such as single slashes (/), double slashes (//), 
#' or numbered crosses (e.g., /3/). It extracts the highest cross order when available and returns the sub-pedigree 
#' for the immediate parents.
#'
#' @param pedigree A string providing the parentage through which a cultivar was obtained.
#' 
#' @return 
#' A vector of two items representing the direct female and male parents. If parent information is unavailable or unknown, 
#' `NA` is returned for the respective parent.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

get_parents <- function(pedigree) {
  # make sure it is a string
  pedigree <- as.character(pedigree)
  
  # 1. we did not expect cross depth to be more than two digits (up to 99)
  cross <- regmatches(pedigree, gregexpr("/[0-9]{1,2}/", pedigree))
  
  if (length(cross[[1]]) != 0) {
    # find the highest cross order to cut at it and get parents sub-pedigree
    last_cross <- max(as.numeric(gsub("/", "", cross[[1]])))
    parents    <- regmatches(pedigree, regexpr(paste0("/", last_cross, "/"), pedigree), invert = TRUE)[[1]]
  } else {
    # 2. if it is not of type /#/, then try double backslash //
    cross <- regmatches(pedigree, gregexpr("//", pedigree))
    
    if (length(cross[[1]]) != 0) {
      # get parents sub-pedigree if it is crossed using //
      parents <- regmatches(pedigree, regexpr("//", pedigree), invert = TRUE)[[1]]
    } else {
      # 3. if it is not // then try with single backslash /
      cross <- regmatches(pedigree, gregexpr("/", pedigree))
      
      if (length(cross[[1]]) != 0) {
        # get parents names
        parents <- regmatches(pedigree, regexpr("/", pedigree), invert = TRUE)[[1]]
      } else {
        # 4. else, there is no more cross info in this pedigree, so parents are unknown
        parents <- c(NA, NA)
      }
    }
  }
  
  # remove leading/trailing white-space
  parents <- trimws(parents)
  
  # replace unknown parents by NA
  parents <- gsub("unknown", NA, parents)
  
  # send back a vector of two items, the direct female and male parents
  return(parents)
}


#' Building Pedigree Table Recursively
#'
#' @description
#' Recursively builds a pedigree table by extracting and tracking parents for each genotype/germplasm in the provided list. 
#' The function handles backcross cases and updates the pedigree data frame with parent information for multiple generations.
#'
#' @param geno_list A character vector of genotype/germplasm names.
#' @param pedigree_list A character vector of associated pedigree strings, corresponding to the genotypes in \code{geno_list}.
#' @param pedigree_df A data frame of pedigrees from a previous iteration, used to accumulate pedigree data. If NULL, a new data frame is created.
#' 
#' @return 
#' A data frame with three columns: 
#'   - `Variety`: The identifier for the individual genotype.
#'   - `Female`: The identifier for the female parent.
#'   - `Male`: The identifier for the male parent.
#' The pedigree is built recursively, with individuals listed before any appearance as a parent.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}

build_pedigree_table <- function(geno_list = NULL, pedigree_list = NULL, pedigree_df = NULL) {
  # check if geno list is not empty
  if (length(geno_list) == 0) warning("Empty genotype/germplasm list!")
  
  # check if the length of pedigree list is the same length of geno list
  if (length(pedigree_list) != length(geno_list)) warning("Pedigree list does not match the length of genotype/germplasm list!")
  
  # if no previous pedigree data.frame passed by current call
  if (is.null(pedigree_df)) {
    # create an empty pedigree data.frame
    pedigree_df   <- data.frame(Variety = factor(), Female = factor(), Male = factor())
    
    # make sure that all strings of genotype/germplasm and pedigree lists are in small letters (needs only first time)
    geno_list     <- tolower(iconv(geno_list, 'WINDOWS-1252', 'UTF-8'))
    pedigree_list <- tolower(iconv(pedigree_list, 'WINDOWS-1252', 'UTF-8'))
  }
  
  # create an empty dummy list of previous generation parents
  prev_generation <- c()
  
  # extract the parents of each genotype/germplasm in the given list
  for (i in 1:length(geno_list)) {
    geno    <- as.character(geno_list[i])
    cross   <- as.character(pedigree_list[i])
    parents <- get_parents(cross)
    
    # check for backcross cases and handle them properly
    female_bc <- regmatches(parents[1], regexec("(.+)\\*(\\d+)$", parents[1]))
    
    if (length(female_bc[[1]]) != 0) {
      n <- as.numeric(female_bc[[1]][3])
      if (n > 2) {
        parents <- c(female_bc[[1]][2], sub(parents[1], paste0(female_bc[[1]][2], "*", n - 1), cross, fixed = TRUE))
      } else {
        parents <- c(female_bc[[1]][2], sub(parents[1], female_bc[[1]][2], cross, fixed = TRUE))
      }
    } else {
      male_bc <- regmatches(parents[2], regexec("^(\\d+)\\*(.+)", parents[2]))
      
      if (length(male_bc[[1]]) != 0) {
        n <- as.numeric(male_bc[[1]][2])
        if (n > 2) {
          parents <- c(sub(parents[2], paste0(n - 1, "*", male_bc[[1]][3]), cross, fixed = TRUE), male_bc[[1]][3])
        } else {
          parents <- c(sub(parents[2], male_bc[[1]][3], cross, fixed = TRUE), male_bc[[1]][3])
        }
      }
    }
    
    # update the pedigree data.frame and dummy list of previous generation parents
    pedigree_df     <- rbind(c(geno, parents), pedigree_df)
    prev_generation <- c(prev_generation, parents)
  }
  
  # clean the previous generation parents list by remove NA and duplicates
  prev_generation <- prev_generation[which(!is.na(prev_generation))]
  prev_generation <- unique(prev_generation)
  
  # check if we still have any previous generation parents need to extract
  if (length(prev_generation) > 0) {
    # recall this function recursively to process the previous generation parents passing current pedigree data.frame
    build_pedigree_table(prev_generation, prev_generation, pedigree_df)
  } else {
    # rename the pedigree data.frame columns properly
    names(pedigree_df) <- c("Variety", "Female", "Male")
    
    # remove duplicated entries in the pedigree data.frame
    pedigree_df <- pedigree_df[!duplicated(pedigree_df$Variety), ]
    
    # return the pedigree data.frame
    return(pedigree_df)
  }
}


#' Get the Pedigree Table
#'
#' @description
#' Retrieves a comprehensive pedigree table for the given dataset, which contains genotype names and pedigree strings. 
#' The function recursively traces parentage across generations and builds a pedigree table where each row corresponds 
#' to an individual, with columns for the female and male parents. It also handles cases of similar genotype names 
#' by standardizing them.
#'
#' @param data A data frame containing genotype/germplasm data, including names and pedigree strings.
#' @param geno_column The name of the column that identifies the genotype/germplasm names.
#' @param pedigree_column The name of the column that contains the pedigree strings.
#' 
#' @return 
#' A data frame with three columns:
#'   - `Variety`: The identifier for the individual genotype.
#'   - `Female`: The identifier for the female parent.
#'   - `Male`: The identifier for the male parent.
#' The pedigree table is sorted such that individuals appear before any row where they are listed as a parent.
#' For founders (i.e., individuals with no parent information), `NA` is used for the parental columns.
#' 
#' @author 
#' Khaled Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
#' if (interactive()) {
#'   set_qbms_config("https://bms.icarda.org/ibpworkbench")
#'   login_bms()
#'   set_crop("wheat")
#'   set_program("Wheat International Nurseries")
#'   set_trial("IDYT39")
#'   set_study("IDYT39 Environment Number 9")
#'   germplasm <- get_germplasm_list()
#'   pedigree_table <- get_pedigree_table(germplasm, "germplasmName", "pedigree")
#'
#'   #############################
#'   # nadiv package way
#'   # library(nadiv)
#'
#'   # Get additive relationship matrix in sparse matrix format
#'   # A <- nadiv::makeA(pedigree_table)
#'
#'   # Get A inverse matrix using base R function
#'   # AINV <- solve(as.matrix(A))
#'
#'   #############################
#'   # ASReml-R package way
#'   # library(asreml)
#'
#'   # Represent A inverse matrix in an efficient way using i, j index and Ainverse value
#'   # Actual genotype names of any given index are in the attr(ainv, "rowNames")
#'   # ainv <- asreml::ainverse(pedigree_table)
#'
#'   #############################
#'   # Dummy data set for testing
#'   test <- data.frame(genotype = c("X", "Y"),
#'                      pedigree = c("A//B/D/2/C", "B/C/3/A//B/C/2/D"))
#'
#'   pedigree_table <- get_pedigree_table(test, "genotype", "pedigree")
#' }
#' 
#' @export

get_pedigree_table <- function(data, geno_column = "germplasmName", pedigree_column = "pedigree") {
  # extract the list of genotypes/germplasms and associated pedigrees
  geno_list     <- data[, geno_column]
  pedigree_list <- data[, pedigree_column]
  
  # extract the first round of pedigree data.frame to check/audit it before the final call
  pedigree_df <- build_pedigree_table(geno_list, pedigree_list)
  
  # get only root genotypes (i.e., have no parents info)
  roots <- pedigree_df[is.na(pedigree_df$Female) & is.na(pedigree_df$Male), "Variety"]
  
  # compute the string edit distance
  diff <- utils::adist(roots)
  
  # keep the lower triangular part of the matrix
  diff[!lower.tri(diff)] <- NA
  
  # get the index of pairs with distance = 1 (i.e., one char difference)
  check <- which(diff == 1, arr.ind = TRUE)
  
  # replace index by the genotype name
  check <- cbind(roots[check[, 1]], roots[check[, 2]])
  
  # if there are cases of similar genotype names
  if (nrow(check) > 0) {
    # for each pair of similar genotype names
    for (i in 1:nrow(check)) {
      # go through all letters of the given pair
      for (j in 1:max(nchar(check[i, ]))) {
        # if the given letters in the j offset are same, then move to the next letter
        if (substr(check[i, 1], j, j) == substr(check[i, 2], j, j)) next
        
        # if they are not the same, then check
        # if the different letter is one of this group: <space>, -, _, .
        # then update the geno_list and pedigree_list to be the same
        if (substr(check[i, 1], j, j) %in% c(" ", "-", "_", ".")) {
          geno_list     <- gsub(check[i, 2], check[i, 1], geno_list,     ignore.case = TRUE)
          pedigree_list <- gsub(check[i, 2], check[i, 1], pedigree_list, ignore.case = TRUE)
        } else if (substr(check[i, 2], j, j) %in% c(" ", "-", "_", ".")) {
          geno_list     <- gsub(check[i, 1], check[i, 2], geno_list,     ignore.case = TRUE)
          pedigree_list <- gsub(check[i, 1], check[i, 2], pedigree_list, ignore.case = TRUE)
        }
      }
    }
  }
  
  # get the final pedigree data.frame using updated/audited lists of geno_list and pedigree_list
  pedigree_df <- build_pedigree_table(geno_list, pedigree_list)
  
  return(pedigree_df)
}
