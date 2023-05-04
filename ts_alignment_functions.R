import('pracma')

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
  return(data_vector)
}
