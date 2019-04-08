display /i $pc
set output-radix 16
set disassembly-flavor intel
set print pretty
set print elements 0
#handle SIGTRAP print stop nopass
define rdy
  set __spin=0
end
