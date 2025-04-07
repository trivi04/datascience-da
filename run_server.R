library(plumber)

# Load the main API file
pr <- plumb("D:/programming of datascience/labda6apicode.R")

# Enable CORS so your JS can connect
pr$registerHooks(list(
  preroute = function(req, res) {
    res$setHeader("Access-Control-Allow-Origin", "*")
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept")
  }
))

# Run the server
pr$run(port=8000)
