##filename            	: .zshrc
##description    	: https://github.com/Rsifff/dotfiles/
##author             	: Roman Le Page (Rsifff)
##date                	: 2020/07/31
##version             	: 1.0
##=============================================================

# --------------------------------- EXPORT --------------------------------

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'
export LESSHISTFILE=-
export ZSH="$HOME/.oh-my-zsh"
export TERM=xterm-256color
export EDITOR='vim' 
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#800080,bold,underline"

# --------------------------------- ZSH -----------------------------------

ZSH_THEME="powerlevel10k/powerlevel10k"
plugins=(git zsh-syntax-highlighting zsh-completions zsh-autosuggestions tmux autojump)
source $HOME/.oh-my-zsh/oh-my-zsh.sh
source /usr/share/autojump/autojump.zsh

# ------------------------------- ALIASES ---------------------------------
# color
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias diff='diff --color=auto'
alias ip='ip --color=auto'
alias pacman='pacman --color=auto'
# other
#alias ..='cd ..'
alias scss='scss --no-cache --quiet --sourcemap=none'
alias xclip='xclip -selection c'
# replace commands
command -v vim > /dev/null && alias vi='vim'
alias exegol='python3 ~/.local/bin/Exegol/exegol.py'
# ls & tree
alias ls='lsd'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -F'
command -v lsd > /dev/null && alias ls='lsd --group-dirs first' && \
alias tree='lsd --tree'
command -v colorls > /dev/null && alias ls='colorls --sd --gs' && \
alias tree='colorls --tree'
# top
command -v htop > /dev/null && alias top='htop'
command -v gotop > /dev/null && alias top='gotop -p $([ "$COLOR_SCHEME" = "light" ] && echo "-c default-dark")'
command -v ytop > /dev/null && alias top='ytop -p $([ "$COLOR_SCHEME" = "light" ] && echo "-c default-dark")'
command -v bashtop > /dev/null && alias top='bashtop' # themes for light/dark color-schemes inside ~/.config/bashtop; Press ESC to open the menu and change the the

# ----------------------------------- MISC -----------------------------------
# enable terminal linewrap
setterm -linewrap on
if [ -x /usr/bin/dircolors ]
then
        # colorize ls files
        eval "$(dircolors -b)"
        # Take advantage of $LS_COLORS for completion as well
        zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
fi
                #
# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
precmd () { print -Pn "\e]0;%n@%m: %~\a" }
        ;;
esac

# TMUX
# if no session is started, start a new session
if test -z ${TMUX}; then
    tmux
fi
# when quitting tmux, try to attach
while test -z ${TMUX}; do
    tmux attach || break
done
# ------------------------------ ZSH SETTINGS --------------------------------

# ----- options -----
setopt AUTO_CD
unsetopt beep
unsetopt hist_beep
unsetopt list_beep

#setopt CORRECT
setopt HIST_BEEP
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY
setopt INTERACTIVE_COMMENTS
setopt MAGIC_EQUAL_SUBST
setopt NO_NO_MATCH
setopt NOTIFY
setopt NUMERIC_GLOB_SORT
setopt PROMPT_SUBST
setopt SHARE_HISTORY

HISTFILE="$HOME/.cache/zsh_history"
HIST_STAMPS=mm/dd/yyyy
HISTSIZE=5000
SAVEHIST=5000
ZLE_RPROMPT_INDENT=0
WORDCHARS=${WORDCHARS//\/}

# ----- promt -----
PS1="%F{cyan} %~ >%F{blue}> %F{reset}"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

s=' ' # fix too wide icons
POWERLEVEL9K_MODE=nerdfont-complete
POWERLEVEL9K_SHORTEN_STRATEGY=truncate_beginning
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_PROMPT_ON_NEWLINE=true # <- Set this to false for simgle line prompt
POWERLEVEL9K_RPROMPT_ON_NEWLINE=true
POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
POWERLEVEL9K_OS_ICON_CONTENT_EXPANSION='${P9K_CONTENT} $(whoami | grep -v "^root\$")'
POWERLEVEL9K_OS_ICON_BACKGROUND=red
POWERLEVEL9K_OS_ICON_FOREGROUND=white
POWERLEVEL9K_CONTEXT_BACKGROUND=black
POWERLEVEL9K_CONTEXT_FOREGROUND=red
POWERLEVEL9K_FOLDER_ICON=
POWERLEVEL9K_DIR_BACKGROUND=blue
POWERLEVEL9K_DIR_FOREGROUND=black
POWERLEVEL9K_DIR_WRITABLE_BACKGROUND=black
POWERLEVEL9K_DIR_WRITABLE_FOREGROUND=red
POWERLEVEL9K_VCS_CLEAN_FOREGROUND=black
POWERLEVEL9K_VCS_CLEAN_BACKGROUND=green
POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND=black
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND=yellow
POWERLEVEL9K_VCS_MODIFIED_FOREGROUND=white
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND=black
POWERLEVEL9K_VCS_UNTRACKED_ICON=●
POWERLEVEL9K_VCS_UNSTAGED_ICON=±
POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON=↓
POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON=↑
POWERLEVEL9K_VCS_COMMIT_ICON=$s
POWERLEVEL9K_COMMAND_BACKGROUND_JOBS_BACKGROUND=black
POWERLEVEL9K_COMMAND_BACKGROUND_JOBS_FOREGROUND=cyan
POWERLEVEL9K_IP_INTERFACE=wlp2s0
POWERLEVEL9K_IP_BACKGROUND=red
POWERLEVEL9K_IP_FOREGROUND=white
POWERLEVEL9K_LEFT_PROMPT_FIRST_SEGMENT_START_SYMBOL='\uE0B2'
POWERLEVEL9K_RIGHT_PROMPT_LAST_SEGMENT_END_SYMBOL='\uE0B0'
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX='%F{blue}╭─'
POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX='%F{blue}╰%f '
POWERLEVEL9K_VPN_IP_INTERFACE=tun0
POWERLEVEL9K_VPN_IP_FOREGROUND=black
POWERLEVEL9K_VPN_IP_BACKGROUND=blue
POWERLEVEL9K_EXECUTION_TIME_ICON=$s
POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=0
POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND=black
POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND=red
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(os_icon host dir dir_writable vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=( vpn_ip command_execution_time ip )
ZLE_RPROMPT_INDENT=0