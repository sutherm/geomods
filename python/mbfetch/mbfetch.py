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
import urllib
import urllib2

_version = '0.1.1'

_license = """
version %s
Copyright (c) 2011, 2017, 2018 Matthew Love <matthew.love@colorado.edu>

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

mbfetch.py [-region xmin xmax ymin ymax] [-list-only]

Options:
  -region\tSpecifies the desired input region; xmin xmax ymin ymax
  -list_only\tStop processing once the .lst files are generated
  -process\tGenerate a shell script to convert the downloaded data to standard xyz.

  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
mbfetch.py -region -90.75 -88.1 28.7 31.25

mbfetch.py v.%s 
""" %(_version)

_mb_data_url = "https://data.ngdc.noaa.gov/platforms/"
_mb_search_url = "https://maps.ngdc.noaa.gov/mapviewer-support/multibeam/files.groovy?"

class mb_results:
    def __init__(self, bounds):
        self._verbose = False
        self._cancel = False
        self._surveys = []
        self._survey_list = []
        self.bounds = bounds

        self.fetch_results()
        
    def _fcancel(self):
        self._cancel = True

    def _fcanceled(self):
        self._cancel = False

    def fetch_results(self):

        results_url = "%sgeometry=%f,%f,%f,%f" %(_mb_search_url,self.bounds[0],self.bounds[2],self.bounds[1],self.bounds[3])
        
        # URL Encode the data so it can be posted to the web service
        req = urllib2.Request(results_url)
        response = urllib2.urlopen(req)

        try:
            print('mblib: Fetching multibeam results for area: %s,%s,%s,%s' %(str(self.bounds[0]),str(self.bounds[1]),str(self.bounds[2]),str(self.bounds[3])))
            results = response.read()
            
            response.close()
            print('mblib: Received multibeam search results.')
        except: print('mblib: There is a problem - aborting this attempt!')
        self._survey_list = results.split("\n")[:-1]

    def parse_results(self, local):
        for res in self._survey_list:
            survey = res.split(" ")[0].split("/")[6]
            if survey not in self._surveys:
                self._surveys.append(survey)

            sf = open(survey + ".lst", 'a')

            if local: 
                sf.write("." + res)
            else:
                data_url = _mb_data_url + "/".join(res.split("/")[3:])
                sf.write(data_url.split(" ")[0])

            sf.write("\n")
            sf.close()

    def fetch(self):
        for res in self._survey_list:
            survey = res.split(" ")[0].split("/")[6]
            dirname = "/".join(res.split(" ")[0].split("/")[:-1])
            if not os.path.exists("." + dirname):
                os.makedirs("." + dirname)

            data_url = _mb_data_url + "/".join(res.split("/")[3:])
            data_path = "." + res.split(" ")[0]

            if not os.path.exists(data_path):
                print data_url.split(" ")[0] + " --> " + data_path

                f = urllib2.urlopen(data_url.split(" ")[0])
                outf = open(data_path, 'wb')
                outf.write(f.read())
                f.close()
                outf.close()

    def shell_proc(self):
        proc_n = 'mb_lst2xyz.sh'
        proc_file = open(proc_n, 'w')
        proc_file.write('#!/bin/sh\n\n')
        proc_file.write('### Code: \n\n')
        proc_file.write('')
        proc_file.write('mkdir xyz\n')
        proc_file.write('for i in *.lst; do\n')
        proc_file.write('\tmblist -F-1 -D3 -I$i | gmt gmtselect -R %s/%s/%s/%s | awk \'{print $1,$2,$3}\' > xyz/$(basename $i .lst).xyz;\n' %(self.bounds[0],self.bounds[1],self.bounds[2],self.bounds[3]))
        proc_file.write('done\n')
        proc_file.write('')
        proc_file.write('### End\n')
        proc_file.close()
        os.chmod(proc_n, 0o775)

if __name__ == '__main__':
    
    extent = None
    lst_only = False
    want_process = False

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
            print(_usage)
            sys.exit(1)

        elif arg == '-version' or arg == '--version':
            print('mbfetch.py v.%s' %(_version))
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
    mbr = mb_results(extent)
    mbr.parse_results(True)

    if lst_only:
        pass
    else:
        mbr.fetch()
        if want_process:
            mbr.shell_proc()
    #--

### End