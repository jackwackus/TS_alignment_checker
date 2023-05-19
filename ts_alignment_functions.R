import('pracma')
import('ggplot2')
import('plotly')
import('scales')
import('rlang')
import('splus2R')
import('rotations')

generate.data.vector.df <- function(data, parameter)
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
  date = data$date[index_vector]
  return(cbind(date, data_vector))
}

find.peaks <- function(data, parameter, minimum_peaks, min_peak_sampling_res)
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
  data_max = max(data_vector)
  peaks = findpeaks(data_vector, minpeakheight = data_max/min_peak_sampling_res)
  for (minheight in seq(from = data_max, to =  data_max/min_peak_sampling_res, by = -data_max/min_peak_sampling_res))
  {
    test_peaks = findpeaks(data_vector, minpeakheight = minheight)
    if(length(test_peaks[,1]) >= minimum_peaks)
    {
      peaks = test_peaks
      break
    }
  }
  peak_heights = as.vector(peaks[,1], mode = 'numeric')
  peak_indices = as.vector(index_list[peaks[,2]], mode = 'numeric')
  return(cbind(peak_heights, peak_indices))
}

find.peaks.2 <- function(data, parameter, minimum_peaks)
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
  while(length(peaks[peaks == TRUE]) < minimum_peaks)
    {
      span_ratio = span_ratio + 1
      span = as.integer(data_length/span_ratio)
      if(span%%2 == 0){span = span + 1}
      peaks = peaks(data_vector, span = span, strict = FALSE, endbehavior = 1)
    }
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
  return(cbind(peak_indices, peak_vals))
}

find.peaks.3 <- function(data, parameter, search_peak_n, return_peak_n)
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
  #sorted_matrix = filtered_matrix[order(filtered_matrix[,1]),]
  return(filtered_matrix)
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
