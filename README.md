# NorIns_shiny

Shiny app for displaying info on the Norwegian Insect monitoring program (NorIns), in a shinydashboard layout.

The app containerized through docker using an .env file for credentials with this expected content:


```
DB_HOST=""
DB_NAME=""
DB_USER=""
DB_PASSWORD=""

#To limit the pulls to the production branch
IMAGE_TAG=latest
```

To run:
```
docker compose pull
docker-compose up -d

#Alternatively, to run the test branch
IMAGE_TAG=test docker compose up -d
```

NB! To pull this container, you first need to login to ghcr.io, usually using a github token PAT for your github account.
