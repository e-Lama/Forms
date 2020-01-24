#!/bin/bash

echo 'Usage: $ . envset.sh'
if [ -z "$LD_LIBRARY_PATH" ]
then
    echo 'The path is empty'
    LD_LIBRARY_PATH='../libgui/'
    export LD_LIBRARY_PATH
    echo "Done! LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
else
    echo 'The path is not empty!'
fi
