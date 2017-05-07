movie_data = read.csv('movie_metadata.csv',header=TRUE)
movie_data = movie_data[, !colnames(movie_data) %in% c('num_voted_users','movie_imdb_link','num_user_for_reviews','imdb_score','movie_facebook_likes')]
movie_data = movie_data[complete.cases(movie_data),]

sapply(movie_data$movie_title,class)

#fix movie titles
movie_data$movie_title = as.character(movie_data$movie_title)
movie_data$movie_title = substr(movie_data$movie_title,1,nchar(movie_data$movie_title)-2)

#get a list of unique genres
movie_data$genres = as.character(movie_data$genres)
genres = c()
for (i in 1:length(movie_data$genres)) {
  genres = union(genres,unlist(strsplit(movie_data$genres[i],"[|]")))
}
for (i in 1:length(genres)) {
  genres[i] = gsub('-','_',tolower(genres[i]))
}

#populate a matrix with genre values
movie_genres = matrix(0,length(movie_data$genres),length(genres))
for (i in 1:length(movie_data$genres)) {
  temp = unlist(strsplit(movie_data$genres[i],"[|]"))
  for (j in 1:length(temp)) {
    movie_genres[i,match(gsub('-','_',tolower(temp[j])),genres)] = 1
  }
}

#create genre columns in movie_data based on movie_genres
for (i in 1:length(genres)) {
  movie_data[genres[i]] = movie_genres[,i]
}

#delete genre row
movie_data = movie_data[,!colnames(movie_data) %in% c('genres')]

#get and process ratings
movie_data$content_rating = as.character(movie_data$content_rating)
ratings = c()
for (i in 1:length(movie_data$content_rating)) {
  if (movie_data$content_rating[i] == '' | movie_data$content_rating[i] == 'Not Rated') {
    movie_data$content_rating[i] = 'not_rated'
  } 
  ratings = union(ratings,movie_data$content_rating[i])
}
for (i in 1:length(ratings)) {
  ratings[i] = gsub('-','_',tolower(ratings[i]))
}

#populate a matrix with rating values
movie_ratings = matrix(0,length(movie_data$content_rating),length(ratings))
for (i in 1:length(movie_data$content_rating)) {
  rate = gsub('-','_',tolower(movie_data$content_rating[i]))
  movie_ratings[i,match(rate,ratings)] = 1
}

#create rating columns in movie_data based on movie_ratings
for (i in 1:length(ratings)) {
  movie_data[ratings[i]] = movie_ratings[,i]
}

#delete rating row
movie_data = movie_data[,!colnames(movie_data) %in% c('content_rating')]

write.csv(movie_data,'movie_data_cleaned_v2.csv')