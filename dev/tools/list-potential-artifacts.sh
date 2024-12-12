#!/bin/sh

for d in _build stdlib/_build; do
  if [ -d $d ]; then
    find $d -type f | sort
  fi
done
