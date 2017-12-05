#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Set some aliases
alias ls='ls --color=auto'
alias la='ls -la'
alias ll='la -l'

# Custom PS1
PS1="\[\$([[ \$? != 0 ]] && echo '\033[48;5;196m\033[1m \342\234\227 \033[38;5;196m\033[48;5;19m\356\202\260\033[0m')\]\[\033[48;5;19m\033[1m\] \u \[\033[38;5;19m\033[48;5;67m\356\202\260\033[0;1m\033[48;5;67m\] \h \[\033[38;5;67m\033[48;5;237m\356\202\260\033[0m\033[48;5;237m\] \W \[\033[0m\033[38;5;237m\356\202\260\033[0m \]"

# Always VIM ;P
export EDITOR=vim
export TERM=xterm-256color

# Java path
export PATH=$PATH:/opt/dmenu:/opt/jdk8/bin
# Ruby gems path
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

# Start X if we are on tty1
[[ $(tty) == "/dev/tty1" ]] && exec startx

# Source vte.sh
source /etc/profile.d/vte.sh

# Set $? = 0
true
