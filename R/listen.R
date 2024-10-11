# listen.R
# Functions for interacting with a language model using WebSocket for persistence
# code written with help from ChatGPT 4o

library(reticulate)
library(websocket)

# WebSocket integration for persistent LLM conversation

# Function to establish a WebSocket connection to a Python server
initialize_websocket <- function() {
  ws <- WebSocket$new("ws://localhost:8765")
  return(ws)
}

# Unified function to generate response using WebSocket connection
generate_response_websocket <- function(ws, prompt) {
  ws$send(prompt)
  response <- NULL

  ws$onMessage(function(event) {
    response <<- event$data
    cat("Generated response: ", response, "\n")
  })

  # Wait for response to be assigned (simplified example, consider timeout handling)
  while (is.null(response)) {
    Sys.sleep(0.1)
  }

  return(response)
}

# Example function to handle log streaming with WebSocket
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
      cat("Generated response: ", response, "\n")
      
      # Update last line read
      last_line_read <- length(log_lines)
    }
    
    Sys.sleep(5) # Adjust this interval as needed
  }
}

speak_response <- function(prompt) {
  ws <- initialize_websocket()
  response <- generate_response_websocket(ws, prompt)
  cat("Generated response: ", response, "\n")
}

# Example usage: Feeding a custom prompt to the language model and outputting the response
speak_response("Tell me about the progress of my calculations.")
