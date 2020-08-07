# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

# User specific environment and startup programs

# Change this from time-to-time for the RISC-V cross toolchain.
export RISCV_TOOLCHAIN_HOME=$HOME/work/riscv/toolchains/2020.07.02-10.1.0
export RISCV_SYSROOT=${RISCV_TOOLCHAIN_HOME}/riscv64-unknown-linux-gnu/sysroot

export PATH=${RISCV_TOOLCHAIN_HOME}/bin:$HOME/local/bin:$HOME/.local/bin:$PATH
