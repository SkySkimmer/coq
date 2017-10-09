#!/usr/bin/env bash

# A script to check prettyness for a range of commits

set -x

CALLNAME="$0"

function usage
{
    >&2 echo "usage: $CALLNAME <commit> <commit>"
    >&2 echo "The order of commits is as given to 'git diff'"
}

if [ "$#" != 2 ];
then
    usage
    exit 1
fi

set -o pipefail

BASE_COMMIT="$1"
HEAD_COMMIT="$2"

tab="	"

globs='*.ml *.mli *.ml *.mlp *.mll'

if git diff -U0 "$BASE_COMMIT" "$HEAD_COMMIT" -- $globs | grep -q "^+${tab}";
then
    set +x
    >&2 echo "Some touched lines still have tabs!"
    >&2 echo "Running 'git diff \"\$(echo -e '-G^\t')\" $BASE_COMMIT $HEAD_COMMIT' may help you find them."
    >&2 echo "Currently only the following file extensions are checked: $globs"
    >&2 echo "If you use emacs, you can prevent this error from reocurring by installing ws-butler and enabling ws-butler-convert-leading-tabs-or-spaces."
    exit 1
fi
