#!/bin/sh

# Wrapper script that runs a command and, if the command exits nonzero, waits
# for user input before exiting.  This script also emits the control sequences
# to notify VSCode's terminal integration API when the wrapped command exits.
#
# When creating a VSCode terminal to run a specific process, the terminal
# closes automatically as soon as the process exits.  So if the process prints
# an error message and exits nonzero (as happens when a Python script throws an
# uncaught exception), the user has no opportunity to see the error.  Wrapping
# the process with this script means the terminal will stay open, giving the
# user an opportunity to read the error message.
#
# The terminal integration end-of-execution marker lets the VSCode plugin get
# the exit code of the wrapped process and proceed, even though the main
# process in the terminal hasn't yet exited.

# To produce an `onDidStartTerminalShellExecution` event, VSCode wants to see
# "prompt end" (`B`) and "pre-execution" (`C`) markers.  We include "prompt
# start" (`A`) for completeness.
printf '\033]633;A\007'
printf '\033]633;B\007'
printf '\033]633;C\007'

"$@"
x="$?"

# End-of-execution marker with exit code
printf '\033]633;D;%d\007' "$x"

if [ "$x" -ne 0 ]; then
    echo "Process exited with code $x"
    read dummy
fi
exit "$x"
