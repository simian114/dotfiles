#!/bin/bash

# runemacs.sh - Script to launch Emacs with the sanam configuration

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if Emacs is installed
if ! command -v emacs &> /dev/null; then
    echo "Error: Emacs is not installed or not in your PATH"
    echo "Please install Emacs and try again"
    exit 1
fi

# Check for required fonts
if [ "$(uname)" == "Darwin" ]; then
    # macOS font check
    fc-list | grep -q "JetBrains Mono"
    if [ $? -ne 0 ]; then
        echo "Warning: JetBrains Mono font not found. Some UI elements may not display correctly."
    fi
    
    fc-list | grep -q "Fira Code"
    if [ $? -ne 0 ]; then
        echo "Warning: Fira Code font not found. Some UI elements may not display correctly."
    fi
elif [ "$(uname)" == "Linux" ]; then
    # Linux font check
    fc-list | grep -q "JetBrains Mono"
    if [ $? -ne 0 ]; then
        echo "Warning: JetBrains Mono font not found. Some UI elements may not display correctly."
    fi
    
    fc-list | grep -q "Fira Code"
    if [ $? -ne 0 ]; then
        echo "Warning: Fira Code font not found. Some UI elements may not display correctly."
    fi
fi

# Launch Emacs with custom init file
echo "Starting Emacs with sanam configuration..."
emacs -Q -l "$SCRIPT_DIR/sanam-init.el" "$@"