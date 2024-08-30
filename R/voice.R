# voice.R
# utility functions for text-to-speech

speak_text_unix <- function(text, ...) {
  # Using spd-say for Unix
  tryCatch({
    system2("spd-say", args = shQuote(text), wait=FALSE)
  }, error = function(e) {
    system2("say", args = shQuote(text), wait=FALSE)
  })
}


speak_text_windows <- function(text, ...) {
  # PowerShell script as a string
  ps_script <- '
param([string]$text = "Default text to speak")
Add-Type -AssemblyName System.Speech
$speak = New-Object System.Speech.Synthesis.SpeechSynthesizer
$speak.Speak($text)
'

  # Create a temporary file for the PowerShell script
  temp_file <- tempfile(fileext = ".ps1")
  writeLines(ps_script, temp_file)

  # Call the PowerShell script with the provided text
  system2("Powershell.exe", args = c("-File", shQuote(temp_file), "-text", shQuote(text)), wait = FALSE)
  unlink(temp_file)
}

.speak_text <- function(text, ...) {
  if (.Platform$OS.type == "windows") {
    speak_text_windows(text, ...)
  } else {
    speak_text_unix(text, ...)
  }
}

speak_text <- function(text, ...){
  tryCatch({
    .speak_text(text, ...)
  }, error = function(e) {
    message("Text-to-speech failed: ", e$message)
  })
}
