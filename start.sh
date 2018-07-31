#! /usr/bin/env bash
LEAN_PATH="$LEAN_PATH:`pwd`/ircbot" lean --run sample-bot.lean `cat login_data`