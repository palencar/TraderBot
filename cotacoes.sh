#!/bin/bash

wget -q -O$1 "http://streamerapi.finance.yahoo.com/streamer/1.0?s=$2&k=l10,a00,b00,g00,h00&j=l10,a00,b00,g00,h00&r=0&marketid=us_market&callback=parent.yfs_u1f&mktmcb=parent.yfs_mktmcb&gencallback=parent.yfs_gencb"
