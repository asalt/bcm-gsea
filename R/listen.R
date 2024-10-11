# listen.R
# Functions for interacting with a language model using WebSocket for persistence
# code written with help from ChatGPT 4o

library(reticulate)
library(websocket)

# Function to establish a WebSocket connection to a Python server
initialize_websocket <- function(port=8765) {
  ws <- WebSocket$new(paste0("ws://localhost:", port)
  return(ws)
}

# WebSocket client function to send messages to Python server
# the "side effects" of this message send all occur on the Python side
# we do not yet have a way to recieve messages back.
# that is a planned feature
send_to_websocket <- function(message, port=8765) {
  ws <- initialize_websocket(port=port)
  ws$onOpen(function(event) {
    ws$send(message)
  })
  ws$onClose(function(event) {
    message("WebSocket connection closed")
  })
}



# Unified function to generate response using WebSocket connection
# this would require running in a separate thread                      
generate_response_websocket <- function(ws, prompt) {
  ws$send(prompt)
  response <- NULL

  ws$onMessage(function(event) {
    response <<- event$data
    cat("Generated response: ", response, "\n")
    # TODO call voice speak 
  })

  # Wait for response to be assigned (simplified example, consider timeout handling)
  while (is.null(response)) {
    Sys.sleep(0.1)
  }

  return(response)
}

# Example function to handle log streaming with WebSocket
# this would require running in a separate thread
generate_persistent_response <- function(log_file_path) {
  ws <- initialize_websocket()
  last_line_read <- 0
  while(TRUE) {
    # Read new lines from the log
    log_lines <- readLines(log_file_path)
    new_lines <- log_lines[(last_line_read + 1):length(log_lines)]
    
    if(length(new_lines) > 0) {
      # Generate response for new log entries
      prompt <- paste(new_lines, collapse = " ")
      response <- generate_response_websocket(ws, prompt)
      
      # Output response (to be integrated with a separate voice module)
      speak_response
      cat("Generated response: ", response, "\n")
      
      # Update last line read
      last_line_read <- length(log_lines)
    }
    
    Sys.sleep(5) # Adjust this interval as needed
  }
}
