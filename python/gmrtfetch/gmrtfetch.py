#!/usr/bin/env python
### gmrtfetch.py
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
### Commentary
##
## Fetch Global Multiresolution Topography (GMRT)
## https://www.gmrt.org/about/index.php
##
### Code:

import os
import sys
import requests
import urllib
import urllib2

_version = '0.0.1'

_license = """
version %s
Copyright (c) 2018 Matthew Love <matthew.love@colorado.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy 
of this software and associated documentation files (the "Software"), to deal 
in the Software without restriction, including without limitation the rights 
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
of the Software, and to permit persons to whom the Software is furnished to do so, 
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR 
PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE 
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
    """ %(_version)

_usage = """

gmrtfetch.py [ -hlpRv [ args ] ]

Options:
  -R, --region\t\tSpecifies the desired input region; xmin/xmax/ymin/ymax

  -help\t\t\tPrint the usage text
  -version\t\tPrint the version information

Example:
gmrtfetch.py -R -90.75/-88.1/28.7/31.25

gmrtfetch.py v.%s 
""" %(_version)

_gmrt_grid_url = "https://www.gmrt.org/services/GridServer?"

class gmrt_results:
    def __init__(self, bounds):
        self._verbose = False
        self._cancel = False
        self._surveys = []
        self._survey_list = []
        self.bounds = bounds

        self.fetch_results()
        
    def fetch_results(self):

        data = { 'north':self.bounds[3],
                 'west':self.bounds[0],
                 'south':self.bounds[2],
                 'east':self.bounds[1],
                 'layer':'topo',
                 'format':'geotiff' }
        
        # URL Encode the data so it can be posted to the web service
        req_data = urllib.urlencode(data)
        req = urllib2.Request(_gmrt_grid_url+req_data)
        response = urllib2.urlopen(req)
        #response = requests.post(_gmrt_grid_url, params=data)

        for i in response.info().headers:
            if 'filename' in i: outf = i.split("=")[1].strip()

        outf = open(outf, 'wb')
        outf.write(response.read())

        outf.close()
        
        #results = response.read()
        #response.close()

if __name__ == '__main__':
    
    extent = None

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-R' or arg == '--region':
            extent = map(float, sys.argv[i+1].split("/"))
            i = i + 1

        elif arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '--version' or arg == '-v':
            print('gmrtfetch.py v.%s' %(_version))
            print(_license)
            sys.exit(1)

        else:
            print(_usage)
            sys.exit(0)

        i = i + 1

    if extent is None:
        print(_usage)
        sys.exit(1)

    #--
    mgrt = gmrt_results(extent)
    #--

### End
