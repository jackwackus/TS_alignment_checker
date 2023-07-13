import('ggplot2')
import('plotly')
import('scales')
import('rlang')
import('splus2R')
import('utils')

find.peaks <- function(data, parameter, search_peak_n, return_peak_n)
{
	"
	Find a specified number of peaks in a data series.
	This method searches for a greater number of peaks than it returns  
	Args:
		data (dataframe): dataset
		parameter (string): column name to look for peaks in
		search_peak_n (int): total number of peaks to search for
		return_peak_n (int) number of peaks to include in the final matrix
	"
	message("Finding ", parameter, " peaks.")

	# Transfer all data that is not NA into a list. Also list the indices of the non-NA data values.
	data_list = list()
	index_list = list()
	for (i in 1:nrow(data))
	{
		if(!is.na(data[i, parameter]))
		{
			data_list = append(data_list, list(data[i, parameter]))
			index_list = append(index_list, list(i))
		}
	}
	# Force the index and data lists into vectors.
	index_vector = as.vector(index_list, mode = 'numeric')
	data_vector = as.vector(data_list, mode = 'numeric')

	"
	The peaks() function searches for local maxima over window lengths defined by the span argument.
	For a value to be selected as a local maximum, it must be greater than all other values within a
	window of width span centered at the value.	

	The number of peaks yielded by the peaks() function depends on the span value passed.
	The larger the span value, the fewer peaks will be found, as only peak is detected per width span. 

	The routines below use the peaks() function to identify a user specified number of peaks (search_peak_n)
	by starting with a large span value (data_length/2) and decreasing the span value until the peaks() function
	yields the specified number of peaks.

	The span is set equal to data_length/span_ratio.The span_ratio begins at 2 and is increased by 1 at each iteration. 
	"	
	data_length = length(data_vector)
	span_ratio = 2
	span = as.integer(data_length/span_ratio)
	# Make span odd. peaks() throws a warning when span is even.
	if(span%%2 == 0){span = span + 1}

	# Run peaks function.
		# strict = TRUE (default), an element must be "strictly greater than
		# all other values in its window to be considered a peak."
		# strict = FALSE allows repeated values to be counted as maxima.
		# endbehavior = 0 (default) ignores maximima "within a halfwidth of the ends of the sequence."
		# endbehavior = 1 counts maxima "within a halfwidth of the start or end" of the sequence.	

	# peaks() returns a vector of logical values, with TRUE where peaks occur.
	peaks = peaks(data_vector, span = span, strict = FALSE, endbehavior = 1) 

	# Use the peaks output to generate a vector of the indices where the peaks occur, and a vector of the associated peak maxima.
	peak_indices = c()
	peak_vals = c()
	if (length(peaks) == 0)
	{
		message(paste(parameter, "contains no data, causing Error below."))
	}
	for (i in 1:length(peaks))
	{
		if(peaks[i] == TRUE)
		{
			peak_indices = append(peak_indices, c(index_vector[i]))
			peak_vals = append(peak_vals, c(data_vector[i]))
		}
	}
	peak_matrix = cbind(peak_indices, peak_vals)

	"
	Sometimes the peak_matrix generated at this stage does not contain enough rows required for subsequent routines - 
	peak_matrix[,x] throws an error if peak_matrix only contains one row.
	Since dim(peak_matrix) is null if there is only one row, the aforementioned error will be prevented by 
	checking if dim(peak_matrix) is null before running any peak_matrix[,x] operations.	
	"
	
	# Remove duplicate rows in peak_matrix if peak_matrix contains enough rows for that operation.
	if(!is.null(dim(peak_matrix))){peak_matrix = peak_matrix[!duplicated(peak_matrix[,2]),]}

	# While dim(peak_matrix) is null, increase span ratio until enough peaks are detected such that dim(peak_matrix) is not null. 
	while(is.null(dim(peak_matrix)))
	{
		span_ratio = span_ratio + 1
		span = as.integer(data_length/span_ratio)
		if(span%%2 == 0){span = span + 1}
		peaks = peaks(data_vector, span = span, strict = FALSE, endbehavior = 1)
		peak_indices = c()
		peak_vals = c()
		for (i in 1:length(peaks))
		{
			if(peaks[i] == TRUE)
			{
				peak_indices = append(peak_indices, c(index_vector[i]))
				peak_vals = append(peak_vals, c(data_vector[i]))
			}
		}
		peak_matrix = cbind(peak_indices, peak_vals)
		if(!is.null(dim(peak_matrix))){peak_matrix = peak_matrix[!duplicated(peak_matrix[,2]),]}
	}

	# Increase the span ratio until the specified number of peaks is found.
	while(length(peak_matrix[,1]) < search_peak_n)
	{
		span_ratio = span_ratio + 1
		span = as.integer(data_length/span_ratio)
		if(span%%2 == 0){span = span + 1}
		peaks = peaks(data_vector, span = span, strict = FALSE, endbehavior = 1)
		peak_indices = c()
		peak_vals = c()
		for (i in 1:length(peaks))
		{
			if(peaks[i] == TRUE)
			{
				peak_indices = append(peak_indices, c(index_vector[i]))
				peak_vals = append(peak_vals, c(data_vector[i]))
			}
		}
		peak_matrix = cbind(peak_indices, peak_vals)
		peak_matrix = peak_matrix[!duplicated(peak_matrix[,2]),]
	}

	# Order the peaks by peak height and only retain the top return_peak_n.
	# Order the resulting matrix by peak index.
	ordered_matrix = peak_matrix[order(peak_matrix[,2]),]
	filtered_matrix = tail(ordered_matrix, n = return_peak_n)
	sorted_matrix = filtered_matrix[order(filtered_matrix[,1]),]
	return(sorted_matrix)
}

proximity.checker <- function(value, check_array, threshold)
{
	check_bool = FALSE
	for (check_value in check_array)
	{
		if (abs(value - check_value) <= threshold)
		{
			check_bool = TRUE
			break
		}
	}
	return(check_bool)
}

find.common.peaks <- function(data, parameters, search_peak_n, return_peak_n)
{
	matrix_list = list()
	for (i in 1:length(parameters))
	{
		peak_matrix = find.peaks(data, parameters[i], search_peak_n, return_peak_n) 
		matrix_list = append(matrix_list, list(peak_matrix))
	}
	message('Finding common peaks.')
	shared_peaks = c()
	peak_occurrence_table = list()
	for (element in parameters)
	{
		peak_occurrence_table = append(peak_occurrence_table, list(c()))
	}
	for (i in 1:length(matrix_list))
	{
		peak_indices = matrix_list[[i]][,1]
		for (peak_index in peak_indices)
		{
			peak_occurrence_vector = c()
			if (!proximity.checker(peak_index, shared_peaks, 10))
			{
				for (element in matrix_list)
				{
					peak_compare_array = element[,1]
					if (proximity.checker(peak_index, peak_compare_array, 10))
					{
						peak_occurrence_vector = append(peak_occurrence_vector, c(1))
					}
					else
					{
						peak_occurrence_vector = append(peak_occurrence_vector, c(0))
					}
				}	
				#print(peak_occurrence_vector)
				if (sum(peak_occurrence_vector) >= 3)
				{
					shared_peaks = append(shared_peaks, c(peak_index))
					for (k in 1:length(peak_occurrence_vector))
					{
						peak_occurrence_table[[k]] = append(peak_occurrence_table[[k]], c(peak_occurrence_vector[k]))
					}

				}
			}


		}
	}
	peak_occurrence_df = as.data.frame(peak_occurrence_table, col.names = parameters)
	shared_peaks_df = as.data.frame(list(shared_peaks), col.names = c('Peak Index'))
	shared_peak_times_df = as.data.frame(list(data$date[shared_peaks]), col.names= c('DateTime'))
	results = cbind(shared_peaks_df, shared_peak_times_df, peak_occurrence_df)
	return(results[order(results[,1]),])
}

plot <- function(data, parameter)
{
	p = ggplot(data) + ggtitle("Test") +
		labs(y=parameter) + labs(x="Time PST") +
		geom_line(aes(date, !! sym(parameter))) +
		scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))
	p <- ggplotly(p)
	return(p)
}

plot.time.bounded <- function(data, parameter, start_time, end_time)
{
	date_string = strftime(data$date[1], "%Y-%m-%d")
	start_ts = strptime(paste(date_string, start_time), "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
	end_ts = strptime(paste(date_string, end_time), "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")
	data = data[data$date > start_ts,]
	data = data[data$date < end_ts,]
	p = ggplot(data) + ggtitle("Test") +
		labs(y=parameter) + labs(x="Time PST") +
		geom_line(aes(date, !! sym(parameter))) +
		scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))
	p <- ggplotly(p)
	return(p)
}

plot.stacked.subplots <- function(data, parameters)
{
	plot_list = list()
	for (i in 1:length(parameters))
	{
		plot_list[[i]] = plot(data, parameters[i])
	}
	fig = subplot(plot_list, nrows = length(plot_list), titleY = TRUE)
	return(fig)
}

plot.time.bounded.stacked.subplots <- function(data, parameters, start_time, end_time)
{
	plot_list = list()
	for (i in 1:length(parameters))
	{
		plot_list[[i]] = plot.time.bounded(data, parameters[i], start_time, end_time)
	}
	fig = subplot(plot_list, nrows = length(plot_list), titleY = TRUE)
	return(fig)
}
