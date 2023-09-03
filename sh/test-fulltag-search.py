#!/usr/bin/env python

import os
import glob
import sys
import http.client
import json
import time

def read_http_port_file(directory):
  # Build the search pattern
  search_pattern = os.path.join(directory, "*.http.port")

  # Find matching files
  matching_files = glob.glob(search_pattern)

  # Check the number of matching files
  if len(matching_files) == 0:
    print("No matching files found.")
    return None
  elif len(matching_files) > 1:
    print("Multiple matching files found.")
    return None

  # Read the file
  filename = matching_files[0]
  with open(filename, 'r') as f:
    content = f.read()

  return content

def run_load_against_port(port):
  # Create an HTTP connection with port number
  conn = http.client.HTTPConnection("localhost:" + port)

  headers = {
    'Content-Type': 'application/json'
  }


  for i in range(0, 21126, 25):
    print("offset", i, "...", end="");
    start_time = time.time()

    payload = json.dumps({
      "tag": "Search",
      "contents": {
        "offset": i,
        "tags": [
          "twilight sparkle",
          "rarity",
          "fluttershy",
          "rainbow dash",
          "applejack",
          "pinkie pie"
        ]
      }
    })
    conn.request("POST", "/search", body=payload, headers=headers)

    # Get the response
    response = conn.getresponse()

    # Check if the request was successful
    if response.status == 200:
      # Read and decode response
      data = response.read().decode('utf-8')

      end_time = time.time();
      elapsed_time = end_time - start_time
      elapsed_time_ms = round(elapsed_time * 1000, 3)
      print(elapsed_time_ms, " ms elapsed");
      
      # # Parse JSON response
      # parsed_response = json.loads(data)
      # print(parsed_response)
    else:
      print(f"Failed to send POST request, status code: {response.status}")


if __name__ == "__main__":
  if len(sys.argv) != 2:
    print("Usage: python script.py <directory>")
    sys.exit(1)

  directory = sys.argv[1]
  port = read_http_port_file(directory)

  if port is not None:
    run_load_against_port(port);
