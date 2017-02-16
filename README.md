# Notes

## Amazon Web Services
1. [Create EC2 instance](https://us-west-2.console.aws.amazon.com/ec2/v2/home?region=us-west-2#Instances:sort=monitoring) and launch it (I used a Linux AMI t2.medium). You now have a remote server that will host the app
2. [Create VPC](https://us-west-2.console.aws.amazon.com/vpc/home?region=us-west-2) for infrastructure to control access to the server
3. [Create a Security Group](https://us-west-2.console.aws.amazon.com/ec2/v2/home?region=us-west-2#SecurityGroups:sort=groupId) for the created VPC. This is where inbound/outbound traffic to the server will be defined and controlled
4. Create Inbound Rules in the Security Group using the home IP as the source
	* SSH connection&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(port 80)
	* MongoDB connection&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(port 27107)
	* RStudio server connection&nbsp;&nbsp;&nbsp;(port 8787)
5. Download the file-key, here referred to as *file-key.pem* for SSH authentication
6. Hide the file-key with: `chmod  400 file-key.pem`
7. You can now access the remove server from your PC now that you have (1) the *file-key.pem* file and (2) the Public DNS of your instance, here referred to as *Pub_DNS*. Tunnel into the server with: 

	`ssh -i file-key.pem ec2-user@PUB_DNS`  

## MongoDB general setup
While we are tunnelled into the remote server, we need to setup the database infrastructure. 

1. [Install MongoDB](https://docs.mongodb.com/ecosystem/platforms/amazon-ec2/) with `sudo yum install -y mongodb-org`
2. Open the mongo configuration file with the VIM text editor with: `sudo vim /etc/mongod.conf`. If there is nothing in the file, copy and paste this [default template](https://github.com/mongodb/mongo/blob/master/rpm/mongod.conf).
3. Press `i` which will start the **insert mode** in the VIM editor. Replace
	```
	net:
	  port: 27017
	  bindIp: 127.0.0.1
	  
	security:
      authorization: enabled
	```
	with 

	```
	net:
	  port: 27017
	  bindIp: 0.0.0.0
	  
	security:
      authorization: disabled
	```
	
	The IP `127.0.0.1` only allows local connections to the Mongo database. This is the most secure setup and can be used once the app is all finished and on the server. But while the app is still being made (on your personal work machine) we want to connect to the database remotely. IP `0.0.0.0` will configure Mongo to allow all incoming IP connections.
4. Press `Esq` to exit **insert** mode, then press `:wq` which will save the changes and quit the editor
5. Start the Mongo server in the background with: `mongod -f /etc/mongod.conf`

## MongoDB database setup
The app does not call Mongo directly, but uses an API from the rich but outdated R package: [rmongodb](https://github.com/dselivanov/rmongodb). This package was built to work with Mongo's **MONGODB-CR** authentication protocol. However as of **version 3.0**, Mongo now uses **SCRAM-SHA-1**. Since this package is no longer maintained, we need to [revert our Mongo database to the old authentication](http://stackoverflow.com/questions/31065196/rmongodb-support-for-mongodb-3) to make this work.

1. Start the mongo shell with: `mongo`. We are now in the mongo program.
2. Write some mongo code to revert to old authentication
	```mongo
	use admin
	var schema = db.system.version.findOne({"_id" : "authSchema"})
	schema.currentVersion = 3
	db.system.version.save(schema)
	```
	
3. Create an admin account with a username and password, here referred to as *admin_username* and *admin_password*
	```mongo
	db.addUser( { user: "admin_username",
              pwd: "admin_password",
              roles: [ "userAdminAnyDatabase" ] } )
	```
4. Create a single database in mongo for the app to use, here referred to as *app_db*, and a "collection" (SQL equivalent of a table), here referred to as *app_collection* to house the data
	```mongo
  use app_db
  db.createCollection("app_collection")
	```
5. Create an account the the app will use, here referred to as *app_username* and *app_password* inside *app_db*
	```mongo
	db.addUser( { user: "app_username",
              pwd: "app_password",
              roles: [ "readWrite" ] } )
	```
6. Exit mongo with: `exit`
7. Open up the mongo config file again: `sudo vim /etc/mongod.conf`
8. In **insert mode** (pressing `i`), replace
	```
	security:
      authorization: disabled
	```
  with
  	```
	security:
      authorization: enabled
	```
9. Exit **insert mode** with `Esq` and write and save `:wq`. Now that we have setup users and passwords, we re-enable authentication so **all** access to the mongo server will require both.
10. Restart mongo to use this new configuration with: `sudo service mongod restart`.

## R setup
### Installation and permissions

1. Install R with: `sudo yum install R`
2. `R -e ".libPaths()[1]" -q` to see where R is saving packages(most likely /usr/lib64/R/library)
3. Give yourself (non-admin user) write permissions for that folder: `sudo chmod o+w /usr/local/lib64/R/library` 
4. `R -e "R.home('doc')"` to see where R is storing package docs (most likely /usr/share/doc/R-3.2.2)
5. Give yourself (non-admin user) write permissions for the html/packages.html file within the path given by step 4 `sudo chmod o+w /usr/share/doc/R-3.2.2/html/packages.html`

### Configuration file

1. Create a R config file, where we will store sensitive info like *app_username*, *app_password*, *app_db* and *PUB_DNS* so that we don't need to actually hard-code this info in our code (which could be viewed by others). `vim .Renviron`
2. In **insert mode** (with `i`), the file should look something like the following
	```
APP_USER       = app_username
APP_PASSWORD   = app_password
APP_HOST       = PUB_DNS
APP_DB         = app_db
APP_COLLECTION = app_collection
	```
    where *app username*, *app password*, *PUB_DNS*, *app_db* and *app_collection* are to be replaced with what you had used earlier
3. Exit **insert mode** `Esq` and save and quit `:wq`

This is a verbatim example of how the R code would look using the config file to access Mongo.
```r
library(rmongodb)
app_collection <- Sys.getenv("APP_COLLECTION")
mongo <- mongo.create(
  host     = Sys.getenv("APP_HOST"), 
  username = Sys.getenv("APP_USER"), 
  password = Sys.getenv("APP_PASSWORD"), 
  db       = Sys.getenv("APP_DB")
)
```
Where all of this is run as-is, verbatim, without replacing anything. This code will interact with the secure Mongo database without revealing username, password and database names in the code.

### Rscript executable
We need to write R code (such as scraping) into a script that can be run outside of R, here referred to as *app_script*. 

1. Create text file `vim app_script` and enter **insert mode** (`i`). The file should like this:
    ```
    #!/usr/lib64/R/bin/Rscript
    
    [R code goes here]
    ```
    Now `Esq` then `:wq` to exit **insert mode** and saving and quitting. 
2. Make the file an executable: `chmod u+x app_script`
3. This file can be executed with: `./app_script`
4. Now, we want the server to automatically execute this file at regular intervals instead of having to manual execute the file using chron scheduling: `chrontab -e` to open the vim editor. Your file should look something like this
    ```
    00 12,18 * * * /home/ec2-user/app_script
    ```
    This will run *app_script* at the top of the hour, at 12PM and 6PM, every day, every month, every day of week. More info on crontab formatting [here](https://crontab.guru/)
5. `Esq` and `:wq`. The program *app_script* is now scheduled according to crontab. 