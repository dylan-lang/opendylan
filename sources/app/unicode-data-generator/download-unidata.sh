#!/bin/sh

# This will download the currently-required files into $CWD

wget http://www.unicode.org/Public/UNIDATA/Blocks.txt
wget http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
wget http://www.unicode.org/Public/UNIDATA/PropList.txt
wget http://www.unicode.org/Public/UNIDATA/DerivedCoreProperties.txt
wget http://www.unicode.org/Public/UNIDATA/DerivedNormalizationProps.txt
