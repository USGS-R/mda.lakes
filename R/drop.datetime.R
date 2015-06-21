datetime.pattern = "(datetime|timestamp|time|date)"

drop.datetime = function(data, error=FALSE){
	
	header = names(data)
	dt_indx = grep(datetime.pattern, header, ignore.case=TRUE)
	
	if(length(dt_indx) < 1){
		if(error){
			stop('Unable to find a datetime column. Datetime column was supplied.')
		}else{
			warning('Unable to find a datetime column. Assuming no datetime column was supplied.')
			return(data)
		}
		
	}else if(length(dt_indx) > 1){
		stop('datetime column ambiguity. You can only have one column of datetime.')
	}
	
	return(data[,-dt_indx])
}