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
import urllib
import urllib2
import json

_version = '0.0.2'

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
usage: tnmfetch.py [ -fhlRvx [ args ] ]

Options:
  -R, --region\t\tSpecifies the desired input region; xmin/xmax/ymin/ymax
  -d, --dataset\t\tSpecify the dataset to fetch from; specify sub-datasets with a colon.
  -f, --format\t\tSpecify the data format to download as a string; default downloads all data formats.
  -l, --list-only\tOnly fetch a list of surveys in the given region.

  -x, --index\t\tReturn the index of available datasets and formats.

  --help\t\tPrint the usage text
  --version\t\tPrint the version information

Example:
To fetch only IMG format from NED 1/9:
tnmfetch.py -R -90.75/-88.1/28.7/31.25 --dataset 1:3 --format "IMG"

tnmfetch.py v.%s
""" %(_version)


_tnm_api_url = "http://viewer.nationalmap.gov/tnmaccess/"
_tnm_dataset_url = "http://viewer.nationalmap.gov/tnmaccess/api/datasets?"
_tnm_product_url = "http://viewer.nationalmap.gov/tnmaccess/api/products?"

class tnm:
    def __init__(self):
        self._datasets = self._fetch_datasets()
        self._dataset_results = None

    def _fetch_json(self, tnm_url):
        req = urllib2.Request(tnm_url)
        response = urllib2.urlopen(req)
        results = response.read()
        response.close()

        return json.loads(results)        

    def _fetch_datasets(self):
        return self._fetch_json(_tnm_dataset_url)

    def print_datasets(self):
        for i,j in enumerate(self._datasets):
            print('%s: %s [ %s ]' %(i, j['title'], ", ".join(j['formats'])))
            for m,n in enumerate(j['tags']):
                print('\t%s: %s [ %s ]' %(m, n, ", ".join(j['tags'][n]['formats'])))

    def fetch_file(self, url):
        try:
            f = urllib2.urlopen(url)
            print "downloading " + url

            dirname = "./" + os.path.dirname(url)[7:] + "/"
            if not os.path.exists(dirname):
                os.makedirs(dirname)

            with open(dirname + os.path.basename(url), "wb") as local_file:
                local_file.write(f.read())
        except: pass

    def _query_dataset(self, dtype, bbox, formats):

        try: 
            if len(dtype) > 1:
                dtags = self._datasets[dtype[0]]['tags']
                dtag = dtags.keys()[dtype[1]]
                sbDTag = self._datasets[dtype[0]]['tags'][dtag]['sbDatasetTag']
            else:
                sbDTag = self._datasets[dtype[0]]['sbDatasetTag']
        except: 
            sbDTag = self._datasets[1]['sbDatasetTag']

        if formats is not None:
            tnm_qurl = urllib.urlencode( {'datasets':sbDTag, 'bbox':'%s,%s,%s,%s' %(bbox[0], bbox[2], bbox[1], bbox[3]), 'prodFormats':",".join(formats)} )
        else:
            tnm_qurl = urllib.urlencode( {'datasets':sbDTag, 'bbox':'%s,%s,%s,%s' %(bbox[0], bbox[2], bbox[1], bbox[3])} )
        qurl = '%s%s' %(_tnm_product_url, tnm_qurl)
        self._dataset_results = self._fetch_json(qurl)

    def _fetch_results(self):
        for i in self._dataset_results['items']:
            self.fetch_file(i['downloadURL'])

    def _print_results(self):
        for i in self._dataset_results['items']:
            print i['downloadURL']

if __name__ == '__main__':
    
    extent = None
    dformats = ['IMG']
    want_index = False
    want_list = False
    dtypes = [1]

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-R' or arg == '--region':
            extent = map(float, sys.argv[i+1].split("/"))
            i = i + 1

        elif arg == '--dataset' or arg == '-d':
            dtypes = map(int, sys.argv[i+1].split(":"))
            i = i + 1

        elif arg == '--format' or arg == '-f':
            dformats = sys.argv[i+1].split(",")
            i = i + 1

        elif arg == '--index' or arg == '-x':
            want_index = True

        elif arg == '--list-only' or arg == '-l':
            want_list = True

        elif arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '--version' or arg == '-v':
            print('tnmfetch.py v.%s' %(_version))
            print(_license)
            sys.exit(1)

        else:
            print(_usage)
            sys.exit(0)

        i = i + 1

    if extent is None and want_index is False:
        print(_usage)
        sys.exit(0)

    # --

    tnm = tnm()    

    if want_index:
        tnm.print_datasets()
    else:
        tnm._query_dataset(dtypes, extent, dformats)
        if want_list: tnm._print_results()
        else: tnm._fetch_results()
    #--

### End
