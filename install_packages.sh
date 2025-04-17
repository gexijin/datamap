#!/bin/bash

# Check if uv is installed
if ! command -v uv &> /dev/null; then
    echo "uv is not installed. Installing uv..."
    pip install uv
fi

# Create a virtual environment if it doesn't exist
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    uv venv
fi

# Activate the virtual environment
source venv/bin/activate

# Install packages from requirements.txt
echo "Installing packages from requirements.txt..."
uv pip install -r requirements.txt

echo "Installation complete!" 