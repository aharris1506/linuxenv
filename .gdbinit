#display /i $pc
#set disassembly-flavor intel

set output-radix 16
set print pretty
set print elements 0
set print demangle

#handle SIGTRAP print stop nopass

# For handling raise_if_attach()
define rdy
  set __spin=0
end
