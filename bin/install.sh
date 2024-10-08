#!/bin/bash

# Install dependencies
sudo apt-get update && sudo apt-get install -y \
    libgmp10 \
    liblmdb0 \
    zlib1g

# Download the pallas binary
curl -L https://pallas-binaries.nyc3.cdn.digitaloceanspaces.com/test/pallas -o pallas

# Make it executable
chmod +x pallas

# Move it to a directory in the PATH
sudo mv pallas /usr/local/bin/

echo "Pallas has been installed successfully!"
