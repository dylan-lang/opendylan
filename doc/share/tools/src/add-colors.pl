#!/usr/local/bin/perl

# add-colors  Sep 98

# Finishes processing HTML files processed by doc_html 
# Usage: add-colors html-file [ html-files ]



$^I="~";

while (<>) {

# s/\<BODY\ BGCOLOR\=\"\#FEFEF2\"\>/\<BODY\ BGCOLOR\=\"\#FEFEF2\"\ TEXT\=\"\#000000\"\>/g;

s/\<BODY\ BGCOLOR\=\"\#FEFEF2\"\>/\<BODY\ BGCOLOR\=\"\#FEFEF2\"\ TEXT\=\"\#000000\"\ LINK\=\"\#0000FF\"\ VLINK\=\"\#800080\"\ ALINK\=\"\#FF0000\"\>/g;

 print;

}
