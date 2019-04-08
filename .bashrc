# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

LOCAL=$HOME/local
#
# User specific aliases and functions
#
export PATH=$LOCAL/bin:/opt/bin:$PATH

alias l='ls -F'
alias lt='ls -ltr'
alias ll='ls -ll'

git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

export PS1="\[\033[33m\]\$(git_branch)\[\033[00m\]\$ " 
