# CNN-stories
A bit of web-scrapping for CNN articles, some leaflet-shiny-dashboard stuff and a bit of NLP if I'm lucky.


### The database
The executable file "scraper" will store scrapped articles into a remote mysql server I've set up with Amazon RDS.
For obvious reasons, I don't want anyone getting access to it and the executable file will look for the proper system variables for the credentials. 
I've attached a sample of what the data would look like here: ```data/articles.csv``` .
Feel free to set up your own database (it must be mysql). If you do so you'll have to set system variables. The easiest way is in a .Renviron file

Create a .Renviron file in the home directory with the following lines:
```
MYSQL_CNN_USER     = [Put your database username here]
MYSQL_CNN_PASSWORD = [Put your database password here]
MYSQL_CNN_HOST     = [Put your database hostname here]
MYSQL_CNN_PORT     = [Put your database port number here]
CNN_STORAGE        = REMOTE
```
