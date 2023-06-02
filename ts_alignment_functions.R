import('pracma')
import('ggplot2')
import('plotly')
import('scales')
import('rlang')
import('splus2R')
import('utils')

find.peaks <- function(data, parameter, search_peak_n, return_peak_n)
{
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
	index_vector = as.vector(index_list, mode = 'numeric')
	data_vector = as.vector(data_list, mode = 'numeric')
	data_length = length(data_vector)
	span_ratio = 2
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
	return(cbind(shared_peaks_df, peak_occurrence_df))
}

plot <- function(data, parameter)
{
	p = ggplot(data) + ggtitle("Test") +
		labs(y="ylabel") + labs(x="Time PST") +
		geom_line(aes(date, !! sym(parameter), colour = parameter)) +
		scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))
	p <- ggplotly(p)
	return(p)
}
