#!/bin/sh
set -e

if [ "$1" == "python3" ]; then
    USE_PYTHON=3
elif [ "$1" == "python2" ]; then
    USE_PYTHON=2
elif [ "$(python --version 2>&1 | grep ' 3.')" != "" ]; then
    USE_PYTHON=3
else
    USE_PYTHON=2
fi

if [ "$USE_PYTHON" == "3" ]; then
    echo "Installing for python 3.x"
    PIP=pip3
    ROPE=rope_py3k
else
    echo "Installing for python 2.x"
    PIP=pip2
    ROPE=rope
fi

function pip_install() {
    $PIP install $1
}

pip_install jedi
pip_install importmagic
pip_install flake8
pip_install ipython
pip_install $ROPE


