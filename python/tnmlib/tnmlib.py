#!/usr/bin/env python
### tnmlib.py
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
### Description:
##
## https://viewer.nationalmap.gov/help/documents/TNMAccessAPIDocumentation/TNMAccessAPIDocumentation.pdf
##
### Code:

import os
import sys
import urllib
import urllib2
import json

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

_tnm_api_url = "http://viewer.nationalmap.gov/tnmaccess/"
_tnm_dataset_url = "http://viewer.nationalmap.gov/tnmaccess/api/datasets?"
_tnm_product_url = "http://viewer.nationalmap.gov/tnmaccess/api/products?"

class tnm:
    def __init__(self):
        self._datasets = self._fetch_datasets()
        self._dataset_results = None

    def _fetch_json(self, tnm_url):
        print('%s' %(tnm_url))
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
        # Open the url
        try:
            f = urllib2.urlopen(url)
            print "downloading " + url

            dirname = "./" + os.path.dirname(url)[7:] + "/"
            if not os.path.exists(dirname):
                os.makedirs(dirname)

            # Open our local file for writing
            with open(dirname + os.path.basename(url), "wb") as local_file:
                #with open(os.path.basename(url), "wb") as local_file:
                local_file.write(f.read())
        except urllib2.HTTPError, e:
            print "HTTP Error:", e.code, url
        except urllib2.URLError, e:
            print "URL Error:", e.reason, ulr

    def _query_dataset(self, dtype, bbox, formats):

        try: 
            dtags = self._datasets[dtype[0]]
            sbDTag = self._datasets[dtype[0]]['sbDatasetTag']
            if len(dtype) > 1:
                dtag = dtags.keys()[dtype[1]]
                sbDTag = self._datasets[dtype[0]]['tags'][dtag]['sbDatasetTag']
        except: print "invalid index"

        if formats is not None:
            tnm_qurl = urllib.urlencode( {'datasets':sbDTag, 'bbox':'%s,%s,%s,%s' %(bbox[0], bbox[2], bbox[1], bbox[3]), 'prodFormats':",".join(formats)} )
        else:
            tnm_qurl = urllib.urlencode( {'datasets':sbDTag, 'bbox':'%s,%s,%s,%s' %(bbox[0], bbox[2], bbox[1], bbox[3])} )
        qurl = '%s%s' %(_tnm_product_url, tnm_qurl)
        self._dataset_results = self._fetch_json(qurl)

    def _fetch_results(self):
        for i in self._dataset_results['items']:
            self.fetch_file(i['downloadURL'])
