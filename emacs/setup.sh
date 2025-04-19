#!/bin/bash

# setup.sh - Makes runemacs.sh available system-wide

# Get the absolute path to the dotfiles/emacs directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUNEMACS_PATH="$SCRIPT_DIR/runemacs.sh"

# Ensure runemacs.sh exists and is executable
if [ ! -f "$RUNEMACS_PATH" ]; then
    echo "Error: runemacs.sh not found at $RUNEMACS_PATH"
    exit 1
fi

chmod +x "$RUNEMACS_PATH"
echo "✓ Made runemacs.sh executable"

# Determine the bin directory to use (prefer ~/.local/bin if it exists or is in PATH)
if [ -d "$HOME/.local/bin" ] || [[ "$PATH" == *"$HOME/.local/bin"* ]]; then
    BIN_DIR="$HOME/.local/bin"
    # Create the directory if it doesn't exist
    mkdir -p "$BIN_DIR"
elif [ -d "/usr/local/bin" ] && [ -w "/usr/local/bin" ]; then
    BIN_DIR="/usr/local/bin"
else
    # Fallback to creating ~/.local/bin and adding it to PATH
    BIN_DIR="$HOME/.local/bin"
    mkdir -p "$BIN_DIR"
    
    # Detect shell and update appropriate profile file
    SHELL_NAME="$(basename "$SHELL")"
    case "$SHELL_NAME" in
        bash)
            PROFILE_FILE="$HOME/.bashrc"
            ;;
        zsh)
            PROFILE_FILE="$HOME/.zshrc"
            ;;
        *)
            # Default to .profile for other shells
            PROFILE_FILE="$HOME/.profile"
            ;;
    esac
    
    # Add to PATH in profile if not already there
    if ! grep -q "export PATH=\"\$HOME/.local/bin:\$PATH\"" "$PROFILE_FILE"; then
        echo 'export PATH="$HOME/.local/bin:$PATH"' >> "$PROFILE_FILE"
        echo "✓ Added $BIN_DIR to PATH in $PROFILE_FILE"
        echo "⚠️  Please run 'source $PROFILE_FILE' or restart your terminal to update PATH"
    fi
fi

# Create a symlink or copy the script
DEST_PATH="$BIN_DIR/runemacs"

# Remove existing file/symlink if it exists
if [ -f "$DEST_PATH" ] || [ -L "$DEST_PATH" ]; then
    rm "$DEST_PATH"
    echo "✓ Removed existing runemacs command"
fi

# Create a wrapper script instead of a symlink for better reliability
cat > "$DEST_PATH" << EOF
#!/bin/bash
# This is a wrapper script for runemacs
exec "$RUNEMACS_PATH" "\$@"
EOF
chmod +x "$DEST_PATH"
echo "✓ Created wrapper script at $DEST_PATH"

echo ""
echo "✅ Installation complete! You can now run 'runemacs' from anywhere in your terminal."
echo "   Your Emacs configuration will be loaded from: $SCRIPT_DIR/sanam-init.el"