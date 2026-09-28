#!/bin/sh -e
set -x

# use this script to set up a binary distribution of heml.
#
# Since the binary is not relocatable at the moment, you need to
# build it in the place where it would be extracted on the user's system. 
#
# 1. Make a directory /opt/heml
#
# 2. Check out clbuild to /opt/heml/clbuild
#
# 3. Use clbuild to download heml
#
# 4. Use clbuild to download SBCL, patch it using sbcl.diff and build it
#
# 5. Run this script
#
# 6. Find tarballs in /opt/heml
#
# - Only the -base- tarballs is required for users.
# - The optional -src- tarball enabled use of M-.
#

base=/opt/heml
ver=$(date '+%Y-%m-%d')-$(cd $base/clbuild/source/heml && git show-ref --hash=8 HEAD)
export PATH=$base/clbuild:$PATH

cd $base/clbuild/source/heml
./build.sh tty
cp heml $base/

cd $base

tar cjf heml-bin-base-$ver.tar.bz2 \
	--absolute-names \
	--exclude '*/sbcl.core' \
	$base/heml \
	$base/clbuild/source/iolib/src/syscalls/libiolib-syscalls.so \
	$base/clbuild/source/osicat/posix/libosicat.so \
	$base/clbuild/target/lib/sbcl

tar cjf heml-src-$ver.tar.bz2 \
	--absolute-names \
	--exclude '*/sbcl.core' \
	--exclude '*/source/heml/heml' \
	--exclude '*/source/sbcl/obj/*' \
	--exclude '*/source/sbcl/output/*' \
	--exclude '*.fasl' \
	--exclude '*/clbuild/target/*' \
	$base/clbuild
