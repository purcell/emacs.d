#!/bin/bash

# Run the SLIME test suite inside screen, saving the results to a file.


# This script's exit status is the number of tests failed. If no tests
# fail then no output is printed. If at least one test fails then a
# one-line summary is printed.

# If something unexpected fails, you might get an exit code like 127
# or 255 instead. Sorry.

# This code has been placed in the Public Domain.  All warranties
# are disclaimed.

function usage () {
    cat <<EOF
Usage: $name [-bsRTS] [-n <name>] <emacs> <lisp>"
  -b  use batch mode
  -s  use screen to hide emacs
  -R  don't show results file
  -T  no temp directory (use slime in current directory)
  -S  don't execute tests in random order (use default ordering)
  -n <name>  run only the test with name <name>
EOF
    exit 1
}

name=$0
batch_mode="" # command line arg for emacs
dump_results=true
use_temp_dir=true
test_name=nil
randomize=t

while getopts bsRTSn: opt; do
    case $opt in
	b) batch_mode="-batch";;
	s) use_screen=true;;
	n) test_name="'$OPTARG";;
	S) randomize=nil;;
	R) dump_results=false;;
	T) use_temp_dir=false;;
	*) usage;;
    esac
done

shift $((OPTIND - 1))
[ $# = 2 ] || usage

emacs=$1; lisp=$2;

# Move the code into a directory in /tmp, so that we can compile it
# for the current lisp.

slimedir=$(dirname $name)
tmpdir=/tmp/slime-test.$$
if [ $use_temp_dir == true ] ; then
    testdir=$tmpdir
else
    testdir=$(pwd)
fi
results=$tmpdir/results
statusfile=$tmpdir/status

test -d $tmpdir && rm -r $tmpdir

trap "rm -r $tmpdir" EXIT	# remove temporary directory on exit

mkdir $tmpdir
if [ $use_temp_dir == true ] ; then 
    cp -r $slimedir/*.{el,lisp} ChangeLog $tmpdir 
    # cp -r $slimedir/contrib $tmpdir 
fi

cmd=($emacs -nw -q -no-site-file $batch_mode --no-site-file
       --eval "(setq debug-on-quit t)"
       --eval "(add-to-list 'load-path \"$testdir\")"
       --eval "(require 'slime)"
       --eval "(setq inferior-lisp-program \"$lisp\")"
       --eval "(slime-batch-test \"$results\" $test_name $randomize)")

if [ "$use_screen" = "" ]; then
    "${cmd[@]}"
    echo $? > $statusfile
else 
    session=slime-screen.$$
    screen -S $session -m -D \
	bash -c "\"\$@\"; echo \$? > $statusfile" "" "${cmd[@]}" &
    screenpid=$!
    trap "screen -S $session -X quit" SIGINT SIGQUIT
    wait $screenpid
fi

if [ -f "$statusfile" ]; then
    [ "$dump_results" = true ] && cat $results
    status=$(cat $statusfile)
    echo $status "test(s) failed."
else
    # Tests crashed
    echo crashed
    status=255
fi

exit $status
