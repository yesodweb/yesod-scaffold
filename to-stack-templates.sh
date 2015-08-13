#!/usr/bin/env bash

set -eux

for f in hsfiles/*.hsfiles
do
    cat $f | \
        # This next one is a hack...
        sed s@PROJECTNAME_LOWER@{{name}}@g | \
        sed s@PROJECTNAME@{{name}}@g > \
        ../stack-templates/yesod-$(basename $f)
done
