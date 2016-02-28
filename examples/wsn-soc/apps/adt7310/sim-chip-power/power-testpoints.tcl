# Define testpoints
#
# The array must start with 0 and increase one by one
#
#                        ClkDiv ClkDivRTC Threshold CycleTime BusDivider
set i 0
# play around
#set Testpoints($i) [list      9      3051       100         5          1] ; incr i
#set Testpoints($i) [list     24      3051       100         5          1] ; incr i
#set Testpoints($i) [list     99      3051       100         5          1] ; incr i
#set Testpoints($i) [list    999      3051       100         5          1] ; incr i

# use for Testchip2CPU-LPM3 analogous to normal measurements
#set Testpoints($i) [list     99      3051       100         5          1] ; incr i
#set Testpoints($i) [list     99      3051       100        10          1] ; incr i
#set Testpoints($i) [list     99      3051       100        20          1] ; incr i
#set Testpoints($i) [list     99      3051       100        33          1] ; incr i
#set Testpoints($i) [list     99      3051       100        40          1] ; incr i
#set Testpoints($i) [list     99      3051       100        50          1] ; incr i
#set Testpoints($i) [list     99      3051       100       100          1] ; incr i
#set Testpoints($i) [list     99      3051       100       200          1] ; incr i
#set Testpoints($i) [list     24      3051       100         5          1] ; incr i
#set Testpoints($i) [list     24      3051       100        10          1] ; incr i
#set Testpoints($i) [list     24      3051       100        20          1] ; incr i
#set Testpoints($i) [list     24      3051       100        33          1] ; incr i
#set Testpoints($i) [list     24      3051       100        40          1] ; incr i
#set Testpoints($i) [list     24      3051       100        50          1] ; incr i
#set Testpoints($i) [list     24      3051       100       100          1] ; incr i
#set Testpoints($i) [list     24      3051       100       200          1] ; incr i
#set Testpoints($i) [list      9      3051       100         5          1] ; incr i
#set Testpoints($i) [list      9      3051       100        10          1] ; incr i
#set Testpoints($i) [list      9      3051       100        20          1] ; incr i
#set Testpoints($i) [list      9      3051       100        33          1] ; incr i
#set Testpoints($i) [list      9      3051       100        40          1] ; incr i
#set Testpoints($i) [list      9      3051       100        50          1] ; incr i
#set Testpoints($i) [list      9      3051       100       100          1] ; incr i
#set Testpoints($i) [list      9      3051       100       200          1] ; incr i

# Characterization of SoC
# use with either Testchip2CPU-LPM1 or Testchip2CPU-LPM3 to determine infinite loop power
#set Testpoints($i) [list      9      3051       100         0          1] ; incr i
#set Testpoints($i) [list     24      3051       100         0          1] ; incr i
#set Testpoints($i) [list     99      3051       100         0          1] ; incr i
# use with Testchip2CPU-LPM1 and Testchip2CPU-LPM3 each to determine LPM1 and LPM3 sleep power
#set Testpoints($i) [list      9      3051       100      2000          1] ; incr i
#set Testpoints($i) [list     24      3051       100      2000          1] ; incr i
#set Testpoints($i) [list     99      3051       100      2000          1] ; incr i
# use with Testchip2CPU-LPM3 to enable -I0 for the analysis
#set Testpoints($i) [list      9         0       100      2000          1] ; incr i
#set Testpoints($i) [list     24         0       100      2000          1] ; incr i
#set Testpoints($i) [list     99         0       100      2000          1] ; incr i

# no multi-dimensional arrays supported by TCL, therefore use a list
echo "Setup $i testpoints."
