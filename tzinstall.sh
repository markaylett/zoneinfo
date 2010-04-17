#!/bin/sh

set -e

prefix=${1:-'/usr'}

pkgs_url='http://riksun.riken.go.jp/pub/pub/Linux/slackware/slackware-current/source/l/glibc'
code_tgz=tzcode2009d.tar.gz
data_tgz=tzdata2009d.tar.gz

code_md5=bcd22fb108f302bdcc2128e3de915550
data_md5=95a7af89e6ec9a08096dda3c659d1bf3

rm -fR tz/*
mkdir -p tz
pushd tz

for tgz in $code_tgz $data_tgz; do
    wget "$pkgs_url/$tgz"
done

cat <<EOF | md5sum -c
$code_md5 *$code_tgz
$data_md5 *$data_tgz
EOF

for tgz in $code_tgz $data_tgz; do
    gunzip -c $tgz | tar -xvf -
done

make TOPDIR=$prefix install
$prefix/etc/zdump America/New_York

popd
