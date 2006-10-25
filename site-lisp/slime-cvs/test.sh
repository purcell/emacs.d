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
cp $slimedir/*.el $slimedir/*.lisp ChangeLog $testdir
mkfifo $dribble

session=slime-screen.$$

screen -S $session -m -D bash -c "$emacs -nw -q -no-site-file --no-site-file \
       --eval '(setq debug-on-quit t)' \
       --eval '(setq max-lisp-eval-depth 1000)' \
       --eval '(setq load-path (cons \"$testdir\" load-path))' \
       --eval '(require (quote slime))' \
       --eval '(setq inferior-lisp-program \"$lisp\")' \
       --eval '(slime-batch-test \"$results\")' > $dribble;\
       echo \$? > $statusfile" &

screenpid=$!

if [ "$verbose" = true ]; then
    cat $dribble &
else
    cat $dribble > /dev/null &
fi;

trap "screen -S $session -X quit" SIGINT
wait $screenpid

if [ -f "$statusfile" ]; then
    [ "$dump_results" = true ] && cat $results;
    echo $(cat $statusfile) "test(s) failed."
else
    # Tests crashed
    echo crashed
fi

exit $status
