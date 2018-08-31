#!/usr/bin/env python
### tnmfetch.py
##
## Copyright (c) 2018 Matthew Love <matthew.love@colorado.edu>
##
## Permission is hereby granted, free of charge, to any person obtaining a copy 
## of this software and associated documentation files (the "Software"), to deal 
## in the Software without restriction, including without limitation the rights 
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
## of the Software, and to permit persons to whom the Software is furnished to do so, 
## subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
## INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
## PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
## FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
## ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
##
### Code:

import os
import sys
import tnmlib

_version = '0.1'

def Usage():
    print('')
    print('tnmfetch.py [-region xmin xmax ymin ymax]')
    print('')
    print('Options:')
    print('  -region\tSpecifies the desired input region; xmin xmax ymin ymax')
    print('  -dataset\tSpecify the dataset to fetch from; see -dataset-index for index; specify sub-datasets with a colon.')
    print('          \te.g. to fetch from NED 1/9: -dataset 1:3')
    print('')
    print('  -index\tReturn the index of available datasets and formats.')
    print('')
    print('  -help\t\tPrint the usage text')
    print('  -version\tPrint the version information')
    print('')
    print('Example:')
    print('tnmfetch.py -region -90.75 -88.1 28.7 31.25')
    print('')
    print('tnmfetch.py v.%s | tnmlib v.%s' %(_version, tnmlib._version))
    sys.exit(0)

## Mainline
if __name__ == '__main__':
    
    extent = None
    want_index = False
    dtypes = [1]
    dformats = None

    # Process the command line
    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                      float(sys.argv[i+3]),float(sys.argv[i+4]))
            i = i + 4

        elif arg == '-dataset':
            dtypes = map(int, sys.argv[i+1].split(":"))
            i = i + 1

        elif arg == '-format':
            dformats = sys.argv[i+1].split(",")
            i = i + 1

        elif arg == '-index':
            want_index = True

        elif arg == '-help' or arg == '--help' or arg == '-h':
            Usage()

        elif arg == '-version' or arg == '--version':
            print('tnmfetch.py v.%s | tnmlib v.%s' %(_version, tnmlib._version))
            print(tnmlib._license)
            sys.exit(1)

        elif arg[0] == '-':
            Usage()

        else:
            Usage()

        i = i + 1

    if extent is None and want_index is False:
        Usage()

    # --

    tnm = tnmlib.tnm()    

    if want_index:
        tnm.print_datasets()
    else:
        tnm._query_dataset(dtypes, extent, dformats)
        tnm._fetch_results()
    #--


