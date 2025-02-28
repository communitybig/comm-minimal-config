# Setup fzf
# ---------
if [[ ! "$PATH" == *~/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}~/.fzf/bin"
fi

# Auto-completion
# ---------------
if [[ $- == *i* ]]; then
  [[ -e "/usr/share/fzf/completion.bash" ]] && source "/usr/share/fzf/completion.bash" 2> /dev/null
fi

# Key bindings
# ------------
[[ -e "/usr/share/fzf/key-bindings.bash" ]] && source "/usr/share/fzf/key-bindings.bash" 2> /dev/null
