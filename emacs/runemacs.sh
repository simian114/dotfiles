#!/bin/bash

# runemacs.sh - Script to launch Emacs with the sanam configuration

# Get the directory where this script is located, following symlinks
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPT_DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

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

# Double-check that the init file exists
INIT_FILE="$SCRIPT_DIR/sanam-init.el"
if [ ! -f "$INIT_FILE" ]; then
    echo "Error: Configuration file not found at $INIT_FILE"
    echo "Script directory: $SCRIPT_DIR"
    echo "You might need to re-run the setup script or create a symlink to the correct location"
    exit 1
fi

# Launch Emacs with custom init file
echo "Starting Emacs with sanam configuration..."
echo "Loading configuration from: $INIT_FILE"
emacs -Q -l "$INIT_FILE" "$@"