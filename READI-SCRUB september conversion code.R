latest_open_science_data_september<-readRDS("latest_open_science_data_september.RDS")

library(data.table)
library(dplyr)
source_data <- data.table(latest_open_science_data_september)


#add canonical state_aus marker
source_data[,state_aus:=ausonly_state]

source_data <- data.frame(source_data)

latest_open_science_data_old<-readRDS("latest_open_science_data.RDS")

labels(latest_open_science_data_old)


col_label_list <- NULL
data_colnames <- colnames(latest_open_science_data_old)
for (col_n in 1:ncol(latest_open_science_data_old)){
  
  name_label_pair <- attr(latest_open_science_data_old[,col_n],"label")
  if (is.null(name_label_pair)){
    next
  }
  name_label_df <- data.frame("name"=data_colnames[col_n],"label"=unlist(name_label_pair[1]))
  
  if(is.null(col_label_list)){
    col_label_list=name_label_df
  }else{
    #col_label_list <-c(col_label_list,name_label_pair)# rbind(col_label_list,name_label_df)
    col_label_list <-rbind(col_label_list,name_label_df)
  }
}

#now go through each of the columns in the dataset we want to change.
#and if they don't have a label attribute, and we have one for them, then add it.
for (col_n in 1:ncol(source_data )){
  name_label_pair <- attr(source_data[,col_n],"label")
  col_name <- colnames(source_data)[col_n]
  #label is missing in target dataset
  if(is.null(name_label_pair)){
    print(paste0("label is null for ",col_name))
    
    old_source_lookup <- col_label_list %>% filter(name==col_name)
    #if we have a label to add
    if(nrow(old_source_lookup>0)){
      #add it
      setattr(source_data[,col_n],"label", old_source_lookup$label)
      print(paste0("set label for ",old_source_lookup$name))
    }
    
  }
}
## TO DO NEXT: TRY TO GET THESE COL LABELS. THEN SEE IF ANYTHING ELSE NEEDS FIXING.
saveRDS(source_data,"latest_open_science_data_sep_transform.RDS")