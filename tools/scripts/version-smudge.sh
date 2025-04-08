#!/bin/bash

git_version="$(git describe --tags --always --match 'v*')"
date="$(date -Iseconds)"
sed "s,DEVELOPMENT_VERSION,${git_version} built on ${date},g"
