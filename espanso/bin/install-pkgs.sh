#!/usr/bin/env bash

for pkg in basic-emojis gitmojis
do
  espanso install "$pkg" || true
done
