
# cmplhs

TUI utillity that runs any process and shows you the output split into stderr and stdout.
The output is viewed in FIFO manner and you can re-execute the process with keybind.

## Usage

Used to work with compilers, as they simply tell you all errors at once, and scroll your terminal at the and of the output.
That is not optimal as you have to scroll to the top again and again...

This utility simply allows you to have that same output shown from the first error and scroll the output with your arrows.
It also splits the standart input/output into separate views, as errors are on stderr.

## How it works

Execute the binary, it parses the makefile in same directory and gives you abillity to interactively execute any target in the makefile.

Press '4' to get to overview panel which stores information about keybindings.
