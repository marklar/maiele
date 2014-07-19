#!/bin/sh
cd `dirname $0`
monit -g lsd unmonitor all
./restart.rb > /dev/null
sleep 1
monit -g lsd monitor all
