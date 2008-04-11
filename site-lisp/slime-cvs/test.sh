#!/bin/sh

# Run the SLIME test suite inside screen, saving the results to a file.

# This script's exit status is the number of tests failed. If no tests
# fail then no output is printed. If at least one test fails then a
# one-line summary is printed.

# If something unexpected fails, you might get an exit code like 127
# or 255 instead. Sorry.

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

function usage () {
    echo "Usage: $name [-v] [-r] <emacs> <lisp>"
    exit 1
}

name=$0

while getopts vr opt; do
    case $opt in
	v) verbose=true;;
	r) dump_results=true;;
	*) usage;;
    esac
done

shift $((OPTIND - 1))
[ $# = 2 ] || usage

emacs=$1; lisp=$2;

# Move the code into a directory in /tmp, so that we can compile it
# for the current lisp.

slimedir=$(dirname $name)
testdir=/tmp/slime-test.$$
results=$testdir/results
dribble=$testdir/dribble
statusfile=$testdir/status

test -d $testdir && rm -r $testdir

trap "rm -r $testdir" EXIT	# remove temporary directory on exit

mkdir $testdir
cp -r $slimedir/*.{el,lisp} ChangeLog $slimedir/contrib  $testdir
mkfifo $dribble

cmd=($emacs -nw -q -no-site-file --no-site-file
       --eval "(setq debug-on-quit t)"
       --eval "(add-to-list 'load-path \"$testdir\")"
       --eval "(require 'slime)"
       --eval "(setq inferior-lisp-program \"$lisp\")"
       --eval "(slime-batch-test \"$results\")")

if [ "$verbose" = true ]; then
    "${cmd[@]}"
    echo $? > $statusfile
else 
    session=slime-screen.$$
    screen -S $session -m -D \
	bash -c "\"\$@\"; echo \$? > $statusfile" "" "${cmd[@]}" &
    screenpid=$!
    trap "screen -S $session -X quit" SIGINT
    wait $screenpid
fi

if [ -f "$statusfile" ]; then
    [ "$dump_results" = true ] && cat $results;
    echo $(cat $statusfile) "test(s) failed."
else
    # Tests crashed
    echo crashed
fi

exit $status
