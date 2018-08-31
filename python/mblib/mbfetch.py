#!/usr/bin/env python
### mblib.py
##
## Copyright (c) 2011, 2017, 2018 Matthew Love <matthew.love@colorado.edu>
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
import mblib

_version = '0.2'

def Usage():
    print('')
    print('mbfetch.py [-region xmin xmax ymin ymax] [-list-only]')
    print('')
    print('Options:')
    print('  -region\tSpecifies the desired input region; xmin xmax ymin ymax')
    print('  -list_only\tStop processing once the .lst files are generated')
    print('  -process\tGenerate a shell script to convert the downloaded data to standard xyz.')
    print('')
    print('  -help\t\tPrint the usage text')
    print('  -version\tPrint the version information')
    print('')
    print('Example:')
    print('mbfetch.py -region -90.75 -88.1 28.7 31.25')
    print('')
    print('mbfetch.py v.%s | mblib v.%s' %(_version, mblib._version))
    sys.exit(0)

def gen_proc(region):
    proc_n = 'mb_lst2xyz.sh'
    proc_file = open(proc_n, 'w')
    proc_file.write('#!/bin/sh\n\n')
    proc_file.write('### Code: \n\n')
    proc_file.write('')
    proc_file.write('mkdir xyz\n')
    proc_file.write('for i in *.lst; do\n')
    proc_file.write('\tmblist -F-1 -D3 -I$i | gmt gmtselect -R %s/%s/%s/%s | awk \'{print $1,$2,$3}\' > xyz/$(basename $i .lst).xyz;\n' %(region[0],region[1],region[2],region[3]))
    proc_file.write('done\n')
    proc_file.write('')
    proc_file.write('### End\n')
    proc_file.close()
    os.chmod(proc_n, 0o775)

## Mainline
if __name__ == '__main__':
    
    extent = None
    lst_only = False
    want_process = False

    # Process the command line
    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                      float(sys.argv[i+3]),float(sys.argv[i+4]))
            i = i + 4

        elif arg == '-list_only':
            lst_only = True

        elif arg == '-process':
            want_process = True

        elif arg == '-help' or arg == '--help' or arg == '-h':
            Usage()

        elif arg == '-version' or arg == '--version':
            print('mbfetch.py v.%s | mblib v.%s' %(_version, mblib._version))
            print(mblib._license)
            sys.exit(1)

        elif arg[0] == '-':
            Usage()

        else:
            Usage()

        i = i + 1

    if extent is None:
        Usage()

    #--
    mbr = mblib.mb_results(extent)
    mbr.parse_results(True)

    if lst_only:
        pass
    else:
        mbr.fetch()
        if want_process:
            gen_proc(extent)
    #--

### End
