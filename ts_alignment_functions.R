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

	This function uses the splus2R peaks() function to identify a user specified number (return_peak_n) of large peaks in a dataset. 
	This function runs routines that overcome some of the shortcomings of the standalone peaks() function.

	The peaks() function searches for local maxima over window lengths defined by the span argument.
	For a value to be selected as a local maximum, it must be greater than all other values within a
	window of width span centered at the value.	

	The number of peaks yielded by the peaks() function depends on the span value passed.
	The larger the span value, the fewer peaks will be found, as only one peak is detected per width span. 

	The peaks() function often misses large peaks at high span values that it catches at lower span values.
	Similarly, the peaks() function often catches smaller peaks while missing larger peaks...
	until the span value is sufficiently decreased that the larger peaks are caught. 

	Because of the the tendency of the peaks() function to miss important peaks until after finding many peaks of lesser importance,
	the find.peaks() function employs the search_peak_n argument to obtain search_peak_n peaks from the peaks() function,
	then find.peaks() sorts the results by peak height and select the return_peak_n heighest peaks.	

	Args:
		data (dataframe): dataset
		parameter (string): column name to look for peaks in
		search_peak_n (integer): total number of peaks to search for
		return_peak_n (integer) number of peaks to include in the final matrix
	Returns:
		sorted_matrix (matrix): matrix containing the return_peak_n largest peaks found
			first column is peak index, second column is peak height
	"
	# Send status message to user.
	message("Finding ", parameter, " peaks.")

	# Transfer all data that is not NA into a list. Also list the indices of these values.
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

	# Check if any data remains after removing NA values. Return NULL if not.
	if (length(data_vector) == 0)
	{
		message(paste(parameter, "contains no data.", parameter, "peak finding aborted."))
		# Returning NULL will allow other functions to proceed normally.
		return()
	}


	"
	The routines below use the peaks() function to identify a user specified number of peaks (search_peak_n)...
	by starting with a large span value (data_length/2) and decreasing the span value until the peaks() function...
	yields the specified number of peaks.

	The span is set equal to data_length/span_ratio. The span_ratio begins at 2 and is increased by 1 at each iteration. 
	"	
	data_length = length(data_vector)
	span_ratio = 2
	span = as.integer(data_length/span_ratio)
	# Make span odd. peaks() throws a warning when span is even.
	if(span%%2 == 0){span = span + 1}

	"
	Run peaks() function.
		When strict = TRUE (default), an element must be 'strictly greater than...
		all other values in its window to be considered a peak.'
		We run strict = FALSE, which allows repeated values to be counted as maxima.
		When endbehavior = 0 (default), maximima 'within a halfwidth of the ends of the sequence' are ignored.
		We run endbehavior = 1, which counts maxima 'within a halfwidth of the start or end' of the sequence.	
	"

	# peaks() returns a vector of logical values, with TRUE where peaks occur.
	peaks = peaks(data_vector, span = span, strict = FALSE, endbehavior = 1) 

	# Use the peaks() output to generate 2 vectors:
	# A vector of the indices where the peaks occur,
	# and a vector of the associated peak maxima.
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

	"
	Sometimes the peak_matrix generated at this stage does not contain enough rows required for subsequent routines - 
	peak_matrix[,x] throws an error if peak_matrix only contains one row.
	Since dim(peak_matrix) is NULL if there is only one row, the aforementioned error will be prevented by 
	checking if dim(peak_matrix) is NULL before running any peak_matrix[,x] operations.	
	"
	
	# Remove duplicate rows in peak_matrix if peak_matrix contains enough rows for that operation.
	if(!is.null(dim(peak_matrix))){peak_matrix = peak_matrix[!duplicated(peak_matrix[,2]),]}

	# While dim(peak_matrix) is NULL, increase span ratio until enough peaks are detected such that dim(peak_matrix) is not NULL.
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

	# Increase the span ratio until the specified number of peaks (search_peak_n) is found.
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

	# Order the peaks by peak height and only retain the top return_peak_n peaks.
	# Order the resulting matrix by peak index.
	ordered_matrix = peak_matrix[order(peak_matrix[,2]),]
	filtered_matrix = tail(ordered_matrix, n = return_peak_n)
	sorted_matrix = filtered_matrix[order(filtered_matrix[,1]),]
	return(sorted_matrix)
}

proximity.checker <- function(value, check_vector, threshold)
{
	"
	Checks if a numerical value is close to (within a threshold of) any numerical value within another vector.
	Args:
		value (double): numerical value to check proximity of
		check_vector (numeric vector): vector containing numbers that value may be close to
		threshold (double): distance used to determine closeness	
	Returns:
		check_bool (logical): boolean indicating if value is close to any number in check_vector
			TRUE if proximity to any check_vector element is detected
			FALSE if no proximity is detected
	"
	check_bool = FALSE
	for (check_value in check_vector)
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
	"
	Runs find.peaks() on all parameters in the parameters vector.
	Reads through peak output and identifies all peaks that are shared by at least three parameters.
	Args:
		data (dataframe): dataset to look for common peaks in
		parameters (character vector): parameters to look for common peaks across
		search_peak_n (integer): for find.peaks() -> total number of peaks to search for
		return_peak_n (integer): for find.peaks() -> number of peaks to include in the final matrix
	Returns:
		results (dataframe): table containing all the peak indices where common peaks (shared by at least 3 parameters) are found
			indicates which parameters the peak was found in
			ordered by peak index	
	"
	# Run find.peaks() on all parameters and store peak matrices in a list.
	matrix_list = list()
	for (i in 1:length(parameters))
	{
		peak_matrix = find.peaks(data, parameters[i], search_peak_n, return_peak_n) 
		matrix_list = append(matrix_list, list(peak_matrix))
	}

	# Inform the user that common peak search is commencing.
	message('Finding common peaks.')

	# Prepare data structures for common peak search.
	
	# Create a vector to store shared peak indices.
	shared_peaks = c()
	# Create a list containing vectors for each parameter. These vectors start as empty but when filled will...
	# indicate which peaks in the shared_peaks vector are exhibited by each parameter.	
	peak_occurrence_table = list()
	for (element in parameters)
	{
		# Begin by creating empty vectors for each parameter.
		peak_occurrence_table = append(peak_occurrence_table, list(c()))
	}


	# Begin the common peak search. Iterate through the peak matrices for all parameters in the matrix list.
	for (i in 1:length(matrix_list))
	{
		# Pull the vector containing the found peak indices for the parameter.
		peak_indices = matrix_list[[i]][,1]
		# Iterate through the parameter's found peak indices.
		for (peak_index in peak_indices)
		{
			# If the peak index is close to any of the shared peaks that have already been handled,
			# skip the following routines.
			if (!proximity.checker(peak_index, shared_peaks, 10))
			{
				# Create a vector used for logging if the peak is shared by other parameters.
				peak_occurrence_vector = c()
				# Iterate through the peak matrices for all parameters.
				for (element in matrix_list)
				{
					# Check if the peak is close to any of the peaks found for the other parameter.
					peak_compare_array = element[,1]
					if (proximity.checker(peak_index, peak_compare_array, 10))
					{
						# If a match is found, add a 1 to the peak_occurence_vector.
						peak_occurrence_vector = append(peak_occurrence_vector, c(1))
					}
					else
					{
						# If no match is found, add a 0 to the peak_occurence_vector.
						peak_occurrence_vector = append(peak_occurrence_vector, c(0))
					}
				}	
				# If the peak is shared by at least three parameters:
				if (sum(peak_occurrence_vector) >= 3)
				{
					# Add the peak index to the shared_peaks vector,
					shared_peaks = append(shared_peaks, c(peak_index))
					# Add the peak occurence information to the peak_occurence_table.
					for (k in 1:length(peak_occurrence_vector))
					{
						peak_occurrence_table[[k]] = append(peak_occurrence_table[[k]], c(peak_occurrence_vector[k]))
					}

				}
			}


		}
	}
	
	"
	At this stage we have a peak occurence table containing rows for each shared peak.
	Each row expresses with a 1 or a 0 whether or not the peak is present for each compound.
	We also have a shared peaks vector that contains the indices of each shared peak. 
	Each peak in the shared peaks vector corresponds to a row in the peak occurence table. 
	The shared peaks vector and the peak occurence table have the same peak ordering.
	"
	# Convert peak_occurrence_table and shared_peaks into dataframes.
	peak_occurrence_df = as.data.frame(peak_occurrence_table, col.names = parameters)
	shared_peaks_df = as.data.frame(list(shared_peaks), col.names = c('Peak Index'))
	# Create a dataframe containing peak times.
	shared_peak_times_df = as.data.frame(list(data$date[shared_peaks]), col.names= c('DateTime'))
	# Combine the three dataframes into a single dataframe and order the rows by peak index.
	results = cbind(shared_peaks_df, shared_peak_times_df, peak_occurrence_df)
	return(results[order(results[,1]),])
}

plot <- function(data, parameter)
{
	"
	Plots a time series of a user specified parameter.
	Args:
		data (dataframe): dataframe containing data to plot
			must have datetimes in POSIXct contained in a column labeled 'date'
		parameter (character): column name of parameter to plot on y-axis
	Returns:
		p (plotly): plotly time series plot
	"
	p = ggplot(data) + ggtitle("Test") +
		labs(y=parameter) + labs(x="Time PST") +
		geom_line(aes(date, !! sym(parameter))) +
		scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))
	p <- ggplotly(p)
	return(p)
}

plot.time.bounded <- function(data, parameter, start_time, end_time)
{
	"
	Plots a time series of a user specified parameter bounded by user specified start and end times.
	Args:
		data (dataframe): dataframe containing data to plot
			must have datetimes in POSIXct contained in a column labeled 'date'
		parameter (character): column name of parameter to plot on y-axis
		start_time (character): start time in format '%H:%M:%S'
		end_time (character): end time in format '%H:%M:%S'
	Returns:
		p (plotly): plotly time series plot
	"
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
	"
	Plots a stacked set of time series of user specified parameters, with a subplot for each parameter.
	Args:
		data (dataframe): dataframe containing data to plot
			must have datetimes in POSIXct contained in a column labeled 'date'
		parameters (character vector): vector of column names of parameters to plot on y-axes of subplots
	Returns:
		fig (plotly): figure containing stacked time series subplots for each parameter
	"
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
	"
	Plots a stacked set of time series of user specified parameters bounded by user specified start and end times,
	with a subplot for each parameter.
	Args:
		data (dataframe): dataframe containing data to plot
			must have datetimes in POSIXct contained in a column labeled 'date'
		parameters (character vector): vector of column names of parameters to plot on y-axes of subplots
		start_time (character): start time in format '%H:%M:%S'
		end_time (character): end time in format '%H:%M:%S'
	Returns:
		fig (plotly): figure containing stacked time series subplots for each parameter
	"
	plot_list = list()
	for (i in 1:length(parameters))
	{
		plot_list[[i]] = plot.time.bounded(data, parameters[i], start_time, end_time)
	}
	fig = subplot(plot_list, nrows = length(plot_list), titleY = TRUE)
	return(fig)
}
