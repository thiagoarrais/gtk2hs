#!/bin/bash

# This script sticks a bunch of DocBook fragments together into one xml file.
# The result is *not* valid DocBook xml! But it is the right input format for
# the format-docs.xsl program which munges the DocBook into the format
# accepted by the ApiGen program.

echo '<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.1.2//EN"
     "http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd" [
     <!ENTITY gdk-pixbuf "<application>gdk-pixbuf</application>">
     <!ENTITY nbsp        "&#x00A0;">
     <!ENTITY mdash       "&#x2014;">
     <!ENTITY times       "&#x00D7;">
     <!ENTITY percnt      "&#x0025;">
     <!ENTITY num "&#x0023;">
     <!ENTITY lt  "&#38;#60;">
     <!ENTITY gt  "&#x003E;">
     ]>'
echo "<apidoc>"

case $1 in
  --standalone)
  	shift 1
	xsltproc extract-docs.xsl $(find $@ -name '*.xml');;

  *)	for DOC in $(find $@ -name '*.xml')
	do
		echo "<book filename=\"$(basename $DOC)\">"
		cat $DOC
		echo "</book>"
	done;;
esac

echo "</apidoc>"
