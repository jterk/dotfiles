#!/usr/bin/env bash
#
# Check every ten seconds if the process identified as $1 is still running. After 29
# checks (~5 minutes), kill it. Return non-zero to indicate something was killed.
#
# From http://pbrisbin.com/posts/mutt_gmail_offlineimap/
OFFLINEIMAP="/usr/local/bin/offlineimap"

if [ ! -e "$OFFLINEIMAP" ]
then
    OFFLINEIMAP="/opt/boxen/homebrew/bin/offlineimap"
fi

FULL_SYNC_COMMAND="$OFFLINEIMAP -o -u quiet"
FULL_SYNC_FILE=~/tmp/offlineimap.lastfullsync
FULL_SYNC_INTERVAL=1800 # 30 minutes, in seconds
INBOX_SYNC_COMMAND="$OFFLINEIMAP -o -f INBOX -u quiet"
PIDFILE=~/.offlineimap/pid

monitor() {
  local pid=$1 i=0

  while ps $pid &>/dev/null; do
    if (( i++ > 29 )); then
      echo "`date` - Max checks reached. Sending SIGKILL to ${pid}..." >&2
      kill -9 $pid; return 1
    fi

    sleep 10
  done

  return 0
}

if [[ -e $PIDFILE ]]
then
      read -r pid < $PIDFILE

      if ps $pid &>/dev/null
      then
          echo "`date` - Process $pid already running. Exiting..." >&2
          exit 1
      fi
fi

curtime=$(date +%s)
if [ -e $FULL_SYNC_FILE ] ; then
    last_full_sync=`stat -f "%m" $FULL_SYNC_FILE`
    time_diff=$(( curtime - last_full_sync ))

    # echo $time_diff
    # echo $FULL_SYNC_INTERVAL
    if [ ${time_diff} -gt ${FULL_SYNC_INTERVAL} ] ; then
        echo "`date` - Last full sync was long ago, performing full sync."
        touch $FULL_SYNC_FILE
        $FULL_SYNC_COMMAND & monitor $!
    else
        echo "`date` - Performing partial sync"
        $INBOX_SYNC_COMMAND & monitor $!
    fi
else
    echo "`date` - No last-full-sync file, performing full sync."
    touch $FULL_SYNC_FILE
    $FULL_SYNC_COMMAND & monitor $!
fi

