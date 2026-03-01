#!/bin/bash

# Screenshot script for AV1 Remote Browser Client
# Captures only the AV1 Remote Browser window using window ID

set -e

# Check if colors are supported
if [ -t 1 ]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    NC='\033[0m' # No Color
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    NC=''
fi

# Configuration
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_DIR="screenshots"
WINDOW_NAME="AV1 Remote Browser"

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

echo "📸 AV1 Remote Browser Screenshot Tool"
echo "===================================="

# Function to detect the distribution
detect_distro() {
    if [ -f /etc/gentoo-release ]; then
        echo "gentoo"
    elif [ -f /etc/debian_version ]; then
        echo "debian"
    elif [ -f /etc/fedora-release ]; then
        echo "fedora"
    elif [ -f /etc/arch-release ]; then
        echo "arch"
    else
        echo "unknown"
    fi
}

# Function to show installation instructions
show_install_instructions() {
    local missing_tools=("$@")
    local distro=$(detect_distro)
    
    printf "\n${YELLOW}Installation instructions for missing tools:${NC}\n"
    
    case $distro in
        "gentoo")
            printf "${BLUE}Gentoo Linux:${NC}\n"
            for tool in "${missing_tools[@]}"; do
                case $tool in
                    "scrot")
                        printf "  sudo emerge --ask media-gfx/scrot\n"
                        ;;
                    "wmctrl")
                        printf "  sudo emerge --ask x11-wm/wmctrl\n"
                        ;;
                    "xdotool")
                        printf "  sudo emerge --ask x11-misc/xdotool\n"
                        ;;
                esac
            done
            ;;
        "debian")
            printf "${BLUE}Debian/Ubuntu:${NC}\n"
            printf "  sudo apt-get install "
            for tool in "${missing_tools[@]}"; do
                printf "$tool "
            done
            printf "\n"
            ;;
        "fedora")
            printf "${BLUE}Fedora:${NC}\n"
            printf "  sudo dnf install "
            for tool in "${missing_tools[@]}"; do
                printf "$tool "
            done
            printf "\n"
            ;;
        "arch")
            printf "${BLUE}Arch Linux:${NC}\n"
            printf "  sudo pacman -S "
            for tool in "${missing_tools[@]}"; do
                printf "$tool "
            done
            printf "\n"
            ;;
        *)
            printf "${YELLOW}Unknown distribution. Please install the following tools:${NC}\n"
            for tool in "${missing_tools[@]}"; do
                printf "  - $tool\n"
            done
            printf "${YELLOW}Check your distribution's package manager for the correct package names.${NC}\n"
            ;;
    esac
}

# Function to check if required tools are available
check_dependencies() {
    printf "${BLUE}Checking dependencies...${NC}\n"
    
    local missing_tools=()
    local all_found=true
    
    # Check scrot
    if command -v scrot &> /dev/null; then
        printf "  ${GREEN}✓ scrot${NC}\n"
    else
        printf "  ${RED}✗ scrot${NC} - Screenshot tool\n"
        missing_tools+=("scrot")
        all_found=false
    fi
    
    # Check wmctrl
    if command -v wmctrl &> /dev/null; then
        printf "  ${GREEN}✓ wmctrl${NC}\n"
    else
        printf "  ${RED}✗ wmctrl${NC} - Window control utility\n"
        missing_tools+=("wmctrl")
        all_found=false
    fi
    
    # Check xdotool
    if command -v xdotool &> /dev/null; then
        printf "  ${GREEN}✓ xdotool${NC}\n"
    else
        printf "  ${RED}✗ xdotool${NC} - X11 automation tool\n"
        missing_tools+=("xdotool")
        all_found=false
    fi
    
    if [ "$all_found" = true ]; then
        printf "${GREEN}✓ All dependencies found${NC}\n"
    else
        printf "\n${RED}✗ Missing dependencies: ${missing_tools[*]}${NC}\n"
        show_install_instructions "${missing_tools[@]}"
        exit 1
    fi
}

# Function to check if the client is running
check_client_running() {
    printf "${BLUE}Checking if AV1 Remote Browser is running...${NC} "
    
    if pgrep -f "macroquad-client" > /dev/null; then
        printf "${GREEN}✓ Client is running${NC}\n"
        return 0
    else
        printf "${RED}✗ Client is not running${NC}\n"
        printf "${YELLOW}Start the client with: ./start_client.sh${NC}\n"
        exit 1
    fi
}

# Function to find the window ID
find_window_id() {
    local window_id=$(wmctrl -l | grep "$WINDOW_NAME" | awk '{print $1}' | head -1)
    echo "$window_id"
}

# Function to take window screenshot using window ID
take_window_screenshot() {
    printf "${BLUE}Taking AV1 Remote Browser window screenshot...${NC} "
    
    local window_id=$(find_window_id)
    
    if [ -n "$window_id" ]; then
        printf "${YELLOW}Window ID: $window_id${NC}\n"
        
        # Use xdotool for more reliable window activation
        printf "${BLUE}Bringing window to front using xdotool...${NC} "
        
        # Convert hex window ID to decimal for xdotool
        local decimal_id=$((window_id))
        
        # Try xdotool activation (more reliable)
        if xdotool windowactivate "$decimal_id" 2>/dev/null; then
            printf "${GREEN}✓ Window activated with xdotool${NC}\n"
            
            # Also raise the window to ensure it's on top
            xdotool windowraise "$decimal_id" 2>/dev/null || true
            
            # Focus the window
            xdotool windowfocus "$decimal_id" 2>/dev/null || true
            
        else
            printf "${YELLOW}⚠ xdotool failed, trying wmctrl fallback${NC}\n"
            # Fallback to wmctrl
            if wmctrl -a "$WINDOW_NAME" 2>/dev/null; then
                printf "${GREEN}✓ Window activated with wmctrl fallback${NC}\n"
            else
                printf "${YELLOW}⚠ All activation methods failed, but screenshot will still work${NC}\n"
            fi
        fi
        
        # Delay to ensure window is fully visible
        printf "${BLUE}Waiting for window to settle...${NC} "
        sleep 1
        printf "${GREEN}✓ Ready${NC}\n"
        
        local filename="$OUTPUT_DIR/av1_client_${TIMESTAMP}.png"
        # Use scrot with window ID to capture only the client window
        printf "${BLUE}Capturing screenshot...${NC} "
        if scrot -w "$window_id" "$filename"; then
            printf "${GREEN}✓ Saved: $filename${NC}\n"
            printf "${BLUE}✓ Screenshot captured successfully${NC}\n"
            return 0
        else
            printf "${RED}✗ Failed to capture window screenshot${NC}\n"
            return 1
        fi
    else
        printf "${RED}✗ Window not found${NC}\n"
        return 1
    fi
}

# Function to show screenshot info
show_screenshot_info() {
    local filename="$1"
    
    if [ -f "$filename" ]; then
        local filesize=$(ls -lh "$filename" | awk '{print $5}')
        local dimensions=$(file "$filename" | grep -o '[0-9]* x [0-9]*' || echo "Unknown")
        
        printf "${BLUE}Screenshot info:${NC}\n"
        printf "  File: $filename\n"
        printf "  Size: $filesize\n"
        printf "  Dimensions: $dimensions\n"
    fi
}

# Function to display help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -h, --help        Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                # Take AV1 Remote Browser window screenshot"
}

# Main execution
main() {
    # Parse command line arguments
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            *)
                printf "${RED}Unknown option: $1${NC}\n"
                show_help
                exit 1
                ;;
        esac
    done
    
    # Run checks
    check_dependencies
    check_client_running
    
    printf "\n${BLUE}=== Capturing Screenshot ===${NC}\n\n"
    
    # Take window screenshot
    if take_window_screenshot; then
        show_screenshot_info "$OUTPUT_DIR/av1_client_${TIMESTAMP}.png"
    else
        printf "${RED}Failed to capture screenshot${NC}\n"
        exit 1
    fi
    
    printf "\n${GREEN}🎉 Screenshot process completed!${NC}\n"
}

# Run main function with all arguments
main "$@"
