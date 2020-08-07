# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

#
# User specific aliases and functions
#

alias l='ls -F'
alias lt='ls -ltr'
alias ll='ls -al'

git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# For Centos
#export PS1="\[\033[33m\]\$(git_branch)\[\033[00m\]\$ "
# For Ubuntu 18.04 (gnome 3)
export PS1="\[\033]0;\w\007\]\[\033[33m\]\$(git_branch) \[\033[00m\]\\$ "

source $HOME/.git-completion.bash

# https://stackoverflow.com/questions/9457233/unlimited-bash-history
# Eternal bash history.
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
#PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
shopt -s histappend

export INPUTRC=$HOME/.inputrc

# Change this from time-to-time for the RISC-V cross toolchain.
export RISCV_TOOLCHAIN_HOME=$HOME/work/riscv/toolchains/2020.07.02-10.1.0
export RISCV_SYSROOT=${RISCV_TOOLCHAIN_HOME}/riscv64-unknown-linux-gnu/sysroot

export PATH=${RISCV_TOOLCHAIN_HOME}/bin:$HOME/local/bin:$HOME/.local/bin:$PATH

export PYTHONPATH=$HOME/work/swtools/build/py/swtools/sim:$HOME/work/swtools/py

source $HOME/work/venv/bin/activate
