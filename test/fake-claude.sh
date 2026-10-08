#!/bin/sh
# A stand-in for the claude program, for test/smoke-tty.sh: it says where it
# runs and what Heml gave it, and then waits.
echo "CONFIG=$CLAUDE_CONFIG_DIR"
echo "PWD=$(pwd)"
echo "PORT=$CLAUDE_CODE_SSE_PORT IDE=$ENABLE_IDE_INTEGRATION"
exec cat > /dev/null
