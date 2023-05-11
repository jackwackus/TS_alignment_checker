import('pracma')
import('ggplot2')
import('plotly')
import('scales')
import('rlang')

find.peaks <- function(data, parameter, minimum_peaks)
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
  peaks = findpeaks(data_vector, minpeakheight = data_max/10)
  for (minheight in seq(from = data_max, to =  data_max/10, by = -data_max/10))
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

plot <- function(data, parameter)
{
  p = ggplot(data) + ggtitle("Test") +
    labs(y="ylabel") + labs(x="Time PST") +
    geom_line(aes(date, !! sym(parameter), colour = parameter)) +
    scale_x_datetime(date_breaks = "30 mins", labels = date_format("%H:%M", tz = "Etc/GMT+8"))
  p <- ggplotly(p)
  return(p)
}
