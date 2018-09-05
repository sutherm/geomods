#!/usr/bin/env python
### srtmfetch.py
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
import urllib2
import requests
import lxml.html as lh

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

srtmfetch.py [-region xmin xmax ymin ymax] [-list-only]

Options:
  -region\tSpecifies the desired input region; xmin xmax ymin ymax
  -list-only\tReturn a URL list of the discovered tiles.

  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
srtmfetch.py -region -90.75 -88.1 28.7 31.25

srtmfetch.py v.%s 
""" %(_version)

_srtm_url = "http://srtm.csi.cgiar.org"
_srtm_search_url = "http://srtm.csi.cgiar.org/SELECTION/listImages.asp"

class srtm_cgiar:
    def __init__(self, bounds):
        self.bounds = bounds
        self._results = []
        self.fetch_results()
        
    def fetch_results(self):
        data = { 'DownloadMirror':'USA',
                 'InputType':'Decimal',
                 'txtMaxX':str(self.bounds[1]),
                 'txtMinX':str(self.bounds[0]),
                 'txtMaxY':str(self.bounds[3]),
                 'txtMinY':str(self.bounds[2]),
                 'DownloadType':'GeoTiff',
                 'Submit': 'Submit1' }

        response = requests.post(_srtm_search_url, data=data)
        page = lh.document_fromstring(response.content)

        rows = page.xpath("//a[contains(@href, 'SRT-ZIP')]/@href")
        data = list()
        for row in rows:
            self._results.append(row.strip())

    def fetch_file(self, filename):
        print "downloading " + _srtm_url + "/" + filename

        self._dirname = "./" + os.path.dirname(filename)
        
        if not os.path.exists(self._dirname):
            os.makedirs(self._dirname)
 
        with open(self._dirname + "/" + os.path.basename(filename), "wb") as local_file:
            f = urllib2.urlopen(_srtm_url + "/" + filename)
            local_file.write(f.read())
        f.close()

    def fetch_data(self):
        for row in self._results:
            self.fetch_file(row)

    def print_data(self):
        for row in self._results:
            print _srtm_url + row
        
if __name__ == '__main__':
    
    extent = None
    list_only = False

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                      float(sys.argv[i+3]),float(sys.argv[i+4]))
            i = i + 4

        elif arg == '-list-only':
            list_only = True

        elif arg == '-help' or arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '-version' or arg == '--version':
            print('srtmfetch.py v.%s' %(_version))
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
    srtm = srtm_cgiar(extent)
    if list_only: srtm.print_data()
    else: srtm.fetch_data()
    #--

### End
