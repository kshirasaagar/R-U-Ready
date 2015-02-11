
#Find India's position 
url = 'http://maps.google.com/maps/api/staticmap?center=22.5,82.5&zoom=5&size=720x720&maptype=satelleite&format=png32&sensor=true';

download.file('http://maps.google.com/maps/api/staticmap?center=22.5,82.5&zoom=5&size=720x720&maptype=terrain&format=png32&sensor=true', destfile = "IndiaMap.png")

#Also use qmap() and google map trials using getmap and ggmap
