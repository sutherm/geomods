#!/usr/bin/env python
### nosfetch.py
##
## Copyright (c) 2012, 2013, 2014, 2016, 2017, 2018 Matthew Love <matthew.love@colorado.edu>
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
import urllib2
from xml.dom import minidom
import noslib

fnos_version='0.2.2'

nos_file = 'fetch_nos_surveys'

nl = noslib.nosLib()

def Usage(use_error=None):
    print('')
    print('usage: nosfetch.py [-region xmin xmax ymin ymax] [-survey survyeID] [-data datatype] [-list-only] [-process] [-update] [-verbose]')
    print('')
    print('Options:')
    print('  -region\tSpecifies the desired input region; xmin xmax ymin ymax')
    print('  -survey\tFetch a specific survey, enter the surveyID here; this will also accept a file with a list of surveyIDs')
    print('  -data\t\tSpecify the data type to download; separate datatypes with a `,`')
    print('       \t\tPossible types include: %s' %(nl.dic_key_list(noslib._nos_extentions)))
    print('  -list_only\tOnly fetch a list of surveys in the given region.')
    print('  -process\tGenerate a shell script to convert the downloaded BAG or GEODAS/XYZ data to standard xyz.')
    print('  -update\tUpdate the stored list of surveys.')
    print('  -verbose\tIncrease verbosity')
    print('')
    print('  -help\t\tPrint the usage text')
    print('  -version\tPrint the version information')
    print('')
    print('Example:')
    print('nosfetch.py -region -90.75 -88.1 28.7 31.25 -data XYZ,BAG')
    print('')
    print('nos_fetch.py v.%s | noslib v.%s' %(fnos_version, noslib._version))
    sys.exit(0)

def gen_proc(data_type='xyz'):
    proc_n = 'nos_%s2xyz.sh' %(data_type)
    proc_file = open(proc_n, 'w')
    proc_file.write('#!/bin/sh\n\n')
    proc_file.write('### Code: \n\n')
    proc_file.write('mkdir %s\n' %(data_type))
    proc_file.write('for i in *.%s.gz; do\n' %(data_type))
    proc_file.write('\tgunzip $i;\n')
    if data_type == 'xyz':
        proc_file.write('\tawk -F, \'{if (NR!=1) {print $3,$2,$4*-1}}\' $(basename $i .gz) > %s/$(basename $i .gz);\n' %(data_type))
    elif data_type == 'bag':
        proc_file.write('\tgdalwarp $(basename $i .gz) $(basename $i .bag.gz).tif -t_srs \'EPSG:4326\';\n')
        proc_file.write('\tndata=$(gdalinfo $(basename $i .bag.gz).tif | grep NoData | awk -F= \'{print $2}\')\n')
        proc_file.write('\tNoData=$(echo $ndata | awk \'{print $1}\')\n')
        proc_file.write('\tgdal_translate $(basename $i .bag.gz).tif $(basename $i .bag.gz).xyz -of XYZ\n')
        proc_file.write('\tcat $(basename $i .bag.gz).xyz | grep -v $NoData > %s/$(basename $i .bag.gz).xyz;\n' %(data_type))
        proc_file.write('\trm $(basename $i .bag.gz).tif $(basename $i .bag.gz).xyz;\n')
    proc_file.write('\tgzip $(basename $i .gz);\n')
    proc_file.write('done\n\n')
    proc_file.write('### End\n')
    proc_file.close()
    os.chmod(proc_n, 0o775)

#--
#
# Mainline
#
#--
if __name__ == '__main__':

    extent = None
    fetch_list = None
    lst_only = False
    proc = False
    want_update = False
    dtype="ALL"
    verbose=False

    # Process the command line
    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            try:
                extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                          float(sys.argv[i+3]),float(sys.argv[i+4]))
                i = i + 4
            except: Usage("you must enter a value with the -region switch")

        elif arg == '-list_only':
            lst_only = True

        elif arg == '-data':
            try:
                dtype = sys.argv[i+1]
                i = i + 1
            except: Usage("you must enter a value with the -data switch")

        elif arg == '-survey':
            try:
                fetch_list = sys.argv[i+1]
                i = i + 1
            except: Usage("you must enter a value with the -survey switch")

        elif arg == '-process':
            proc = True

        elif arg == '-update':
            want_update = True

        elif arg == '-verbose':
            verbose = True

        elif arg == '-help' or arg == '--help' or arg == '-h':
            Usage()
            sys.exit(0)

        elif arg == '-version' or arg == '--version':
            print('nos_fetch.py v.%s | noslib v.%s' %(fnos_version, noslib._version))
            print(noslib._license)
            sys.exit(1)

        elif arg[0] == '-':
            Usage()

        else:
            Usage()

        i = i + 1

    bounds = extent

    if extent is None and fetch_list is None and want_update is False:
        Usage("you must either enter a region or a surveyID")
    
    dtypes = dtype.split(",")
    nl._verbose = verbose
    dts = []
    if dtypes != ['ALL']:
        for dt in dtypes:
            if proc:
                if dt == 'XYZ': gen_proc('xyz')
                if dt == 'BAG': gen_proc('bag')
            if dt == 'XYZ': dt = 'GEODAS'
            if dt in nl._dtypes: dts.append(dt)
        nl._set_dtypes(dts)
    else:
        if proc:
            if dt == 'XYZ': gen_proc('xyz')
            if dt == 'BAG': gen_proc('bag')

    if want_update:
        nbOb = noslib.nosBounds("nos_bounds.py.update")
        for i in noslib._nos_directories:
            sl = nbOb._readDir(i)
            for j in sl:
                nbOb._updateLines(j)
        nbOb._write()

    elif fetch_list:
        s = noslib.nosSurvey(fetch_list)
        for dt in nl._dtypes:
            s.fetch(dt)
    else:
        nl.bfilter(extent)
        if lst_only:
            for i in nl.surveys:
                print i
        else:
            nl.fetch()

### End
