#!/usr/bin/env zsh

# desc:
#
# Output an org compatible structure representing the filesystem from
# the point passed on the command line (or . by default).
#
# options:
#     none
#
# usage:
#     dir2org.zsh [DIR]...
#
# author:
#     Phil Jackson (phil@shellarchive.co.uk)

set -e

function headline {
    local depth="${1}"
    local text="${2}"

    printf "%${depth}s %s" "" | tr ' ' '*'
    echo " ${text}"
}

function scan_and_populate {
    local depth="${1}"
    local dir="${2}"

    headline ${depth} "${dir}"

    # if there is no files in dir then just move on
    [[ $(ls "${dir}" | wc -l) -eq 0 ]] && return

    (( depth += 1 ))

    for f in $(ls -d "${dir}"/*); do
        if [ -d "${f}" ]; then
            scan_and_populate ${depth} "${f}"
        else
            headline ${depth} "[[file://${f}][${${f##*/}%.*}]]"
        fi
    done

    (( depth -= 1 ))
}

function main {
    local scan_dir="${1:-$(pwd)}"
    local depth=0

    scan_and_populate ${depth} "${scan_dir}"
}

main "${@}"
