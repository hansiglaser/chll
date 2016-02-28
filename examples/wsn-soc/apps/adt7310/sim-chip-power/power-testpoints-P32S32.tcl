# Define testpoints
#
# The array must start with 0 and increase one by one
#
#                        ClkDiv ClkDivRTC Threshold CycleTime BusDivider
set i 0
set Testpoints($i) [list   3051      3051       100         5          1] ; incr i
set Testpoints($i) [list   3051      3051       100        10          1] ; incr i
set Testpoints($i) [list   3051      3051       100        20          1] ; incr i
set Testpoints($i) [list   3051      3051       100        33          1] ; incr i
set Testpoints($i) [list   3051      3051       100        40          1] ; incr i
set Testpoints($i) [list   3051      3051       100        50          1] ; incr i
set Testpoints($i) [list   3051      3051       100       100          1] ; incr i
set Testpoints($i) [list   3051      3051       100       200          1] ; incr i
set Testpoints($i) [list    999      3051       100         5          1] ; incr i
set Testpoints($i) [list    999      3051       100        10          1] ; incr i
set Testpoints($i) [list    999      3051       100        20          1] ; incr i
set Testpoints($i) [list    999      3051       100        33          1] ; incr i
set Testpoints($i) [list    999      3051       100        40          1] ; incr i
set Testpoints($i) [list    999      3051       100        50          1] ; incr i
set Testpoints($i) [list    999      3051       100       100          1] ; incr i
set Testpoints($i) [list    999      3051       100       200          1] ; incr i
set Testpoints($i) [list    199      3051       100         5          1] ; incr i
set Testpoints($i) [list    199      3051       100        10          1] ; incr i
set Testpoints($i) [list    199      3051       100        20          1] ; incr i
set Testpoints($i) [list    199      3051       100        33          1] ; incr i
set Testpoints($i) [list    199      3051       100        40          1] ; incr i
set Testpoints($i) [list    199      3051       100        50          1] ; incr i
set Testpoints($i) [list    199      3051       100       100          1] ; incr i
set Testpoints($i) [list    199      3051       100       200          1] ; incr i
set Testpoints($i) [list     99      3051       100         5          1] ; incr i
set Testpoints($i) [list     99      3051       100        10          1] ; incr i
set Testpoints($i) [list     99      3051       100        20          1] ; incr i
set Testpoints($i) [list     99      3051       100        33          1] ; incr i
set Testpoints($i) [list     99      3051       100        40          1] ; incr i
set Testpoints($i) [list     99      3051       100        50          1] ; incr i
set Testpoints($i) [list     99      3051       100       100          1] ; incr i
set Testpoints($i) [list     99      3051       100       200          1] ; incr i
set Testpoints($i) [list     24      3051       100         5          1] ; incr i
set Testpoints($i) [list     24      3051       100        10          1] ; incr i
set Testpoints($i) [list     24      3051       100        20          1] ; incr i
set Testpoints($i) [list     24      3051       100        33          1] ; incr i
set Testpoints($i) [list     24      3051       100        40          1] ; incr i
set Testpoints($i) [list     24      3051       100        50          1] ; incr i
set Testpoints($i) [list     24      3051       100       100          1] ; incr i
set Testpoints($i) [list     24      3051       100       200          1] ; incr i
set Testpoints($i) [list      9      3051       100         5          1] ; incr i
set Testpoints($i) [list      9      3051       100        10          1] ; incr i
set Testpoints($i) [list      9      3051       100        20          1] ; incr i
set Testpoints($i) [list      9      3051       100        33          1] ; incr i
set Testpoints($i) [list      9      3051       100        40          1] ; incr i
set Testpoints($i) [list      9      3051       100        50          1] ; incr i
set Testpoints($i) [list      9      3051       100       100          1] ; incr i
set Testpoints($i) [list      9      3051       100       200          1] ; incr i
# use with Testchip2CHLL-ADT7310P32LS16L to enable -I0 for the the analysis
#set Testpoints($i) [list      9         0       100      2000          1] ; incr i
#set Testpoints($i) [list     24         0       100      2000          1] ; incr i
#set Testpoints($i) [list     99         0       100      2000          1] ; incr i
#set Testpoints($i) [list    199         0       100      2000          1] ; incr i
#set Testpoints($i) [list    999         0       100      2000          1] ; incr i
#set Testpoints($i) [list   3051         0       100      2000          1] ; incr i
# no multi-dimensional arrays supported by TCL, therefore use a list
echo "Setup $i testpoints."
