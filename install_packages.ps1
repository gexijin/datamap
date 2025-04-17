# Check if uv is installed
if (-not (Get-Command uv -ErrorAction SilentlyContinue)) {
    Write-Host "uv is not installed. Installing uv..."
    pip install uv
}

# Create a virtual environment if it doesn't exist
if (-not (Test-Path "venv")) {
    Write-Host "Creating virtual environment..."
    uv venv
}

# Activate the virtual environment
.\venv\Scripts\Activate.ps1

# Install packages from requirements.txt
Write-Host "Installing packages from requirements.txt..."
uv pip install -r requirements.txt

Write-Host "Installation complete!" 