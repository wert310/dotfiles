#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Set some aliases
alias ls='ls --color=auto'
alias la='ls -la'

# Custom PS1
# PS1='[\u@\h \W]\$ '
PS1="\[\033[0;37m\]\342\224\214\342\224\200\$([[ \$? != 0 ]] && echo \"[\[\033[0;31m\]\342\234\227\[\033[0;37m\]]\342\224\200\")[\[\033[0;33m\]\u\[\033[0;37m\]@\[\033[0;96m\]\h\[\033[0;37m\]]\342\224\200[\[\033[0;32m\]\w\[\033[0;37m\]]\n\[\033[0;37m\]\342\224\224> \[\033[0m\]"

# Always VIM ;P
export EDITOR=vim

# Java path
export PATH=$PATH:/opt/dmenu:/opt/jdk8/bin
# Ruby gems path
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

# Start X if we are on tty1
[[ $(tty) == "/dev/tty1" ]] && exec startx

# Set $? = 0
true
