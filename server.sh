#!/bin/bash

# Copyright (C) 2025 Kishor Datar
# Author: Kishor Datar <kishordatar at gmail>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


stty -icanon
session="session-$BASHPID-$$"
scriptdir=$(realpath $(dirname "${BASH_SOURCE[0]}"))
mcpserver=$1
logdir="$scriptdir/mcp-server.el-logs"
mkdir -p "$logdir"
logfile="$logdir/$mcpserver-$session-log.log"
infile="$logdir/$mcpserver-$session-last-request.json"
outfile="$logdir/$mcpserver-$session-last-response.json"
timeout=${2:-0}
pollinginterval=1

echo "$(date): Starting $mcpserver" >> "$logfile"

while read -r line; do
    echo "$(date): Sending request: ${line:0:100} ..." >> "$logfile"
    echo $line > $infile
    echo "$(date): Request wc stats: $(wc "$infile")" >> "$logfile"

    if ! emacsclient -e "(mcp-server-file-transport-dispatch-request \"$session\" "$mcpserver" \"$infile\" \"$outfile\" $timeout)" >> "$logfile"; then
	echo "$(date): Failed to dispatch request" >> "$logfile"
	break
    fi

    while : ; do
	ready_output=$(emacsclient -e "(mcp-server-file-transport-pop-response-if-ready \"$session\")")
        if [ $? -ne 0 ]; then
            echo "$(date): Failed to poll for response" >> "$logfile"
            break
        elif [ "$ready_output" != "0" ]; then
	    echo "$(date): popped the last response. wc stats: $(wc "$outfile")" >> "$logfile"
	    cat $outfile >> "$logfile"
	    cat $outfile
            break
	else
	    echo "$(date): Response not ready, sleeping for $pollinginterval sec." >> "$logfile"
	    sleep $pollinginterval
	fi
    done
done
