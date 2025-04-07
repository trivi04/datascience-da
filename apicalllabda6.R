# library(plumber)
# pr <- plumb("D:/programming of datascience/labda6apicode.R")
# pr$registerHooks(list(
#   preroute = function(req, res) {
#     res$setHeader("Access-Control-Allow-Origin", "*")
#   }
# ))
# pr$run(port=8000)

library(plumber)

#* @plumber
function(pr) {
  pr$registerHooks(
    list(
      preroute = function(req, res) {
        res$setHeader("Access-Control-Allow-Origin", "*")
        res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept")
      }
    )
  )
  pr
}

# Load your existing endpoints
pr <- plumb("labda6apicode.R")

# Run the server
pr$run(port=8000)
