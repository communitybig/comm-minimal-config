# ~/.bashrc: executed by bash(1) for non-login shells.
# See /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Add user's bin directory to PATH
PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/bin:/usr/games:/sbin:$HOME/bin"

# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

# ===== COLORS CONFIGURATION =====
# BigCommunity custom color palette (based on logo)
# These are the ANSI color codes for direct echo/printf use
BLUE_DARK="\e[38;5;33m"      # Brighter dark blue for TTY visibility
MEDIUM_BLUE="\e[38;5;32m"    # Medium blue (#0077B6)
LIGHT_BLUE="\e[38;5;39m"     # Light blue (#00A8E8)
CYAN="\e[38;5;45m"           # Cyan (#00D4FF)
WHITE="\e[97m"               # White
WHITE_BOLD="\e[1;97m"        # White Bold
RESET="\e[0m"                # Reset color

# ===== HISTORY CONFIGURATION =====
HISTCONTROL=ignoreboth    # Don't put duplicate lines or lines starting with space in the history
shopt -s histappend       # Append to the history file, don't overwrite it
HISTSIZE=1000             # History length in memory
HISTFILESIZE=2000         # History length in file

# ===== SHELL OPTIONS =====
# Check the window size after each command and update LINES and COLUMNS if necessary
shopt -s checkwinsize

# ===== LESSPIPE =====
# Make less more friendly for non-text input files
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# ===== CHROOT DETECTION =====
# Set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# ===== TERMINAL COLOR SUPPORT =====
# Set color prompt for compatible terminals
case "$TERM" in
xterm-color | *-256color) color_prompt=yes ;;
esac

# ===== COLOR ALIASES =====
# Enable color support for various commands
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# ===== ADDITIONAL CONFIGURATION FILES =====
# Load optional bash aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

# Load additional configuration files if they exist
[ -f /usr/share/doc/pkgfile/command-not-found.bash ] && source /usr/share/doc/pkgfile/command-not-found.bash
[ -f ~/.fzf.bash ] && . ~/.fzf.bash
[ -f ~/.bashrcfull ] && . ~/.bashrcfull
[ -f ~/.bashrckali ] && . ~/.bashrckali
[ -f /etc/bashrc ] && . /etc/bashrc

# ===== PYENV CONFIGURATION =====
# Load pyenv automatically if available
if command -v pyenv >/dev/null; then
    export PYENV_ROOT="$HOME/.pyenv"
    [[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# ===== GRC-RS CONFIGURATION =====
# Enable colorized output for various commands using grc-rs
GRC="/usr/bin/grc-rs"
if tty -s && [ -n "$TERM" ] && [ "$TERM" != "dumb" ] && command -v "$GRC" >/dev/null; then
    # Define an alias for easier application of grc-rs to commands
    alias colourify="$GRC"

    # List of commands to configure for colored output
    commands=(
        ant blkid configure df diff dig dnf docker-machinels dockerimages dockerinfo
        dockernetwork dockerps dockerpull dockersearch dockerversion du fdisk
        findmnt go-test ifconfig iostat_sar ip ipaddr ipneighbor iproute iptables
        irclog iwconfig kubectl last ldap lolcat lsattr lsblk lsmod lsof lspci
        lsusb mount mtr mvn netstat nmap ntpdate ping ping2 proftpd pv
        semanageboolean semanagefcontext semanageuser sensors showmount sockstat
        ss stat sysctl tcpdump traceroute tune2fs ulimit uptime vmstat wdiff yaml
    )

    # Iterate through the list of commands and create an alias only if the command exists
    for cmd in "${commands[@]}"; do
        if command -v "$cmd" >/dev/null; then
            alias "$cmd"="colourify $cmd"
        fi
    done

    # Remove temporary variables to avoid polluting the environment
    unset commands cmd
fi

# ===== GCC COLOR SETTINGS =====
# Set GCC color settings for error and warning messages
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# ===== BAT INTEGRATION =====
# Use 'bat' as a replacement for 'cat' with improved output formatting if available
if [ -f /usr/bin/bat ]; then
    cat() {
        local use_cat=false
        # Check if any argument contains -v, -e, -t or their combinations
        for arg in "$@"; do
            if [[ "$arg" =~ ^-[vet]+$ ]]; then
                use_cat=true
                break
            fi
        done

        # If special options are used, use regular cat, otherwise use bat
        if [ "$use_cat" == true ]; then
            command cat "$@"
        else
            bat --paging=never --style=plain "$@"
        fi
    }

    # Customize the 'help' command to display colorized output
    help() {
        if [ $# -eq 0 ]; then
            command help
        else
            "$@" --help 2>&1 | bat --paging=never --style=plain --language=help
        fi
    }
fi

# ===== TTY CONFIGURATION =====
# Apply special configurations when in pure TTY mode (no X server)
if [ "$TERM" = "linux" ]; then
    # Define colors for the TTY with blue theme
    echo -en "\e]P0000000" # black
    echo -en "\e]P8555555" # bright black
    echo -en "\e]P10000AA" # red (using blue instead)
    echo -en "\e]P90055FF" # bright red (using blue instead)
    echo -en "\e]P20088AA" # green (using blue-green instead)
    echo -en "\e]PA00AAFF" # bright green (using blue-green instead)
    echo -en "\e]P300AAEE" # yellow (using light blue instead)
    echo -en "\e]PB00DDFF" # bright yellow (using cyan instead)
    echo -en "\e]P4003399" # blue (brighter blue for visibility)
    echo -en "\e]PC0077B6" # bright blue (medium blue from logo)
    echo -en "\e]P500A8E8" # magenta (light blue from logo)
    echo -en "\e]PD00D4FF" # bright magenta (cyan from logo)
    echo -en "\e]P600AAAA" # cyan
    echo -en "\e]PE00FFFF" # bright cyan
    echo -en "\e]P7AAAAAA" # white
    echo -en "\e]PFFFFFFF" # bright white
    
    # Clear the screen to apply the colors
    clear
fi

# ===== WELCOME FUNCTION =====
# Display system information at login with BigCommunity theme
welcome() {
    # Get system information
    local os_info=$(grep PRETTY_NAME /etc/os-release 2>/dev/null | cut -d'"' -f2 || echo "BigCommunity Linux")
    local kernel=$(uname -r)
    local uptime=$(uptime -p | sed 's/up //')
    local load=$(awk '{print $1 ", " $2 ", " $3}' /proc/loadavg)
    # Fixed memory display - explicitly using the free command
    local memory=$(free -h | awk 'NR==2 {print $3 " / " $2}')
    local disk=$(df -h --output=used,size / | awk 'NR==2 {print $1 " / " $2}')
    
    # Create a clean, visually appealing header with BigCommunity colors
    echo -e "${BLUE_DARK}┌─────────────────────────────────────────────────┐${RESET}"
    echo -e "${BLUE_DARK}│${CYAN}           BigCommunity Linux Terminal           ${BLUE_DARK}│${RESET}"
    echo -e "${BLUE_DARK}└─────────────────────────────────────────────────┘${RESET}\n"
    
    # Display system information in an aligned, clean format with BigCommunity colors
    echo -e "${CYAN}System Info:${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}OS:${RESET}        ${WHITE_BOLD}$os_info${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Kernel:${RESET}    ${WHITE_BOLD}$kernel${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Time:${RESET}      ${WHITE_BOLD}$(date '+%A, %B %d, %Y - %H:%M:%S')${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Uptime:${RESET}    ${WHITE_BOLD}$uptime${RESET}"
    
    echo -e "\n${CYAN}Resources:${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Load:${RESET}      ${WHITE_BOLD}$load${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Memory:${RESET}    ${WHITE_BOLD}$memory${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Disk:${RESET}      ${WHITE_BOLD}$disk${RESET}"
    
    echo -e "\n${CYAN}User Info:${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}User:${RESET}      ${WHITE_BOLD}$(whoami)${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Shell:${RESET}     ${WHITE_BOLD}$SHELL${RESET}"
    echo -e "  ${BLUE_DARK}•${RESET} ${BLUE_DARK}Terminal:${RESET}  ${WHITE_BOLD}$TERM${RESET}"
    
    echo -e "\n${CYAN}Website:${RESET}    ${WHITE_BOLD}https://communitybig.org/${RESET}\n"
}

# Run welcome message when shell starts
welcome
