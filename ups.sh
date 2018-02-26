#! /usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log

mkdir -p $(dirname $LOG_FILE)
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGUMENTS="f2fs-tools 1.9.0 1.10.0
faba-icon-theme 2016-09-13 2017-07-12
faba-mono-icons 2016-04-30 2016-04-30
facter 3.9.3 3.10.0
fakeroot 1.20.2 1.22
fastlane 2.80.0 2.81.0
fatresize 1.0.2 1.0.4
fatsort 1.3.365 1.4.2.439
fbida 2.13 2.14
fbpanel 6.1 7.0
fbreader 0.99.4 2.8.7
fbterm 1.7.0 1.8
fceux 2.2.2 2.2.3
fcitx 4.2.9.5 4.2.9.6
fcitx-anthy 0.2.2 0.2.3
fcitx-chewing 0.2.2 0.2.3
fcitx-cloudpinyin 0.3.4 0.3.6
fcitx-configtool 0.4.9 0.4.10
fcitx-hangul 0.3.0 0.3.1
fcitx-m17n 0.2.3 0.2.4
fcitx-mozc 2.20.2673.102 2.20.2677.102.02
fcitx-qt5 1.2.1 1.2.2
fcitx-table-other 0.2.3 0.2.4
fcitx-unikey 0.2.5 0.2.7
fcron 3.2.1 3.3.0
feh 2.23 2.23.2
ffado 2.3.0 2.4.0
fflas-ffpack 1.6.0 2.3.2
filebeat 6.1.2 6.2.2
filegive 0.7.4 0.7.5
filezilla 3.30.0 3.31.0
findbugs 3.0.1 3.1.0
fio 3.4 3.5
fira 4.106 4.203
firebird 2.5.7 3.0.3
flacon 2.1.1 4.0.0
FlameGraph 2017-07-01 2017-02-08
flannel 0.6.2 0.10.0
flashbench 2012-06-06 2012-06-06
flatbuffers 1.4.0 1.8.0
fldigi 3.23.15 4.0.16
fleet 0.11.8 1.0.0
flexc++ 2.05.00 2.06.02
flite 2.0.0 2.1
flow 0.65.0 0.66.0
fltk 1.3.4 1.3.4.2
fluidsynth 1.1.8 1.1.9
flyway 4.2.0 5.0.7
focuswriter 1.6.8 1.6.10
folly 2017.11.06.00 2018.02.19.00
font-awesome 4.7.0 5.0.6
fontconfig 2.10.2 2.12.6
fontforge 20170730 20170731
foo2zjs 20110210 20171202
fop 2.1 2.2
fortune-mod 1.99.1 2.2.1
fossil 2.3 2.5
fox 1.6.49 1.6.56
fox 1.7.9 1.7.64
fpart 0.9.3 1.0.0
fpc 3.0.0 3.0.4
fping 3.16 4.0
fpp 0.7.2 1.1
fprint_demo 2008-03-03 2008-03-03
fprintd 0.7.0 0.8.0
freecell-solver 4.8.0 4.16.0
freefall 4.9.82 4.15.4
freeimage 3.17.0 3.154
freepats 20060219 20170822
freeswitch 1.6.15 1.6.19
freetds 1.00.70 1.00.80
freetype 2.7.1 2.9
frescobaldi 2.0.16 2.20.0
fribidi 0.19.7 1.0.1
fricas 1.3.1 1.3.2
frostwire 6.4.5 6.6.3
fs-uae 2.8.3 2.8.4
fsharp 4.1.7 4.1.29
fswebcam 20140113 20170115
fwup 0.18.1 1.0.0
fwupd 1.0.4 1.0.5
fwupdate 8 10"


IFS=$'\n'
for a in $ARGUMENTS
do
    unset IFS
    if eval "$SCRIPT_DIR/up.sh $a"
    then
        echo "$a SUCCESS" >> $LOG_FILE
    else
        echo "$a FAIL" >> $LOG_FILE
    fi
done

