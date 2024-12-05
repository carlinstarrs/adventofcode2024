library("tidyverse")

input <- readLines("inputs/day05_input.txt")

updates <- input[grepl(",", input)] %>% map(\(x) str_split(x, ",") %>% unlist() %>% as.numeric())
rules <- input[grepl("\\|", input)] %>% map_chr(\(x) paste0("(?=", str_extract(x, "^\\d+"), str_replace(x, "^\\d+\\|", ").+")))

check_updates <- function(update){
  pass_tracker <- c()
  i <- 1
  while(i <= length(update) & all(pass_tracker == TRUE)){
    relevant_rules <- rules[grepl(glue::glue("\\(\\?\\={update[i]}\\)"), rules) & 
                              grepl(paste0(update[-i], collapse = '|'), rules)]
    if(length(relevant_rules) > 0){
      update_string <- paste0(update, collapse = ",")
      does_it_pass <- map_lgl(relevant_rules, \(x) grepl(x, update_string, perl = TRUE))
      if(all(does_it_pass)){
        pass_tracker <- c(pass_tracker, TRUE)
      } else {
        pass_tracker <- c(pass_tracker, FALSE)
      }
    }
    i <- i + 1
  }
  
  return(all(pass_tracker))
}

test_result <- map_lgl(updates, check_updates)

#part 1
updates[test_result] %>% 
  map_dbl(\(x) x[ceiling(length(x)/2)]) %>% 
  sum()

#part 2
failures <- updates[!test_result]

rule_pass <- function(update, relevant_rules){
  update_string <- paste0(update, collapse = ",")
  map_lgl(relevant_rules, \(x) grepl(x, update_string, perl = TRUE))
}

reorder_update <- function(update, failed_rule){
  nums <- str_extract_all(failed_rule, "\\d+") %>% unlist() %>% as.numeric()
  new_update <- update[-which(update == nums[1])]
  new_update <- append(new_update, nums[1], after = which(update == nums[2]) - 1)
  return(new_update)
}

fix_updates <- function(update){ 
  pass_tracker <- c()
  i <- 1
  while(i <= length(update) & all(pass_tracker == TRUE)){
    relevant_rules <- rules[grepl(glue::glue("\\(\\?\\={update[i]}\\)"), rules) & 
                              grepl(paste0(update[-i], collapse = '|'), rules)]
    if(length(relevant_rules) > 0){
      does_it_pass <- rule_pass(update, relevant_rules)
      if(all(does_it_pass)){
        pass_tracker <- c(pass_tracker, TRUE)
      } else {
        failed_rules <- relevant_rules[!does_it_pass]
        new_update <- update
        
        new_update <- reorder_update(new_update, failed_rules[1])
        does_it_pass <- rule_pass(new_update, relevant_rules[1])
        
        pass_tracker <- c(pass_tracker, all(does_it_pass))
        update <- new_update
        i <- 0
      }
    }
    i <- i + 1
  }
  
  return(update)
}

imap(failures, \(x, i) {
  print(i)
  fix_updates(x)
}) %>% 
  map_dbl(\(x) x[ceiling(length(x)/2)]) %>% 
  sum()
