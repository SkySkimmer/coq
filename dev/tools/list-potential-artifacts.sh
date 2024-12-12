#!/bin/sh

if [ -d _install_ci ]; then
  find _install_ci -type f | sort
fi
