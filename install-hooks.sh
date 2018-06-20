#!/usr/bin/env bash

set -e

if [[ $# != 2 ]]; then
    echo "Usage: $0 [path to auto-push] [.git directory]"
    exit 1
fi

bin=$1
d=$2

if [ ! -e $bin ]; then
    echo "$bin is not executable; is it really the auto-push binary?"
    exit 1
fi

if [ ! -d $d/hooks ]; then
    echo "$d/hooks directory doesn't exist; are you certain $d is a .git directory?"
    exit 1
fi

cat >$d/hooks/pre-receive <<EOF
#!/bin/sh -e

$bin pre-receive
EOF

cat >$d/hooks/post-receive <<EOF
#!/bin/sh -e

$bin post-receive
EOF

chmod ugo+rx $d/hooks/{pre-receive,post-receive}
