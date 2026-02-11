#! /usr/bin/env nix-shell
#! nix-shell -i bash -p inotify-tools

set -x

# python -m http.server &
# pid=$!
# trap "kill $pid" EXIT

clear; make o.rex

while
        inotifywait -e modify -e close_write -e move -e create . ex
        true
do
        clear; make o.rex
done
