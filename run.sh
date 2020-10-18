#! /bin/sh

if [[ "$1" != "" ]] then
        mkdir crawl_result
        logfile=${1#http*://}
        cabal v2-run krawler "$1" | tee -a crawl_result/${logfile%%/*}.log
fi
