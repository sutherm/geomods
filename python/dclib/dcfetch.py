#!/usr/bin/env python
### dcfetch.py
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
## Access elevation data from NOAA's Digital Coast.
##
### Code:

import os
import sys
import dclib

import os
import sys
import ftplib
import urllib
import urllib2
from xml.dom import minidom
import dcbounds
import csv

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

dcfetch.py [-region xmin xmax ymin ymax]

Options:
  -region\tSpecifies the desired input region; xmin xmax ymin ymax

  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
dcfetch.py -region -90.75 -88.1 28.7 31.25

dcfetch.py v.%s
""" %(_version)

_dc_ftp_url_full = "ftp://coast.noaa.gov"
_dc_ftp_url = "coast.noaa.gov"

_out_dir = os.getcwd()
#_out_dir = os.path.dirname(os.path.realpath(__file__))

def _set_out_dir(out_dir):
    global _out_dir
    _out_dir = out_dir

class dc_bounds:
    def __init__(self):
        self.surveys = dcbounds.dc_surveys
        self._slist = []
        self.dcftp = dc_ftp()
        self._bounds = []
        
    def _append_list(self, dataset_id, survey_xml, survey_url):
        
        this_bounding = self.dcftp._get_xml_extents(survey_xml)
        this_title = self.dcftp._get_xml_title(survey_xml)
        this_pdate = self.dcftp._get_xml_pubdate(survey_xml)
        s_entry = [dataset_id, this_title, this_bounding, survey_url, this_pdate]
        print s_entry
        self.surveys.append(s_entry)

    def _update(self):
        self.filelist = self.dcftp.ftp.nlst()
        for i in self.filelist:
            if i == 'lidar1_z' or i == 'lidar2_z':
                self.dcftp.ftp.cwd(i)
                geoids = self.dcftp.ftp.nlst()
                geoid_dir = self.dcftp.ftp.pwd()
                for geoid in geoids:
                    if geoid == 'geoid12a' or geoid == 'geoid12b':
                        self.dcftp.ftp.cwd(geoid)
                        self.dcftp.ftp.cwd("data")
                        data_dir = self.dcftp.ftp.pwd()
                        datalist = self.dcftp.ftp.nlst()
                        #self.ftp.dir()
                        for dataset in datalist:
                            try:
                                self.dcftp.ftp.cwd(dataset)
                                dc_files = self.dcftp.ftp.nlst()
                                for dc_file in dc_files:
                                    if ".xml" in dc_file:
                                        xml_url = _dc_ftp_url_full + self.dcftp.ftp.pwd() + "/" + dc_file
                                        dc_xml = self.dcftp._fetch_xml(xml_url)
                                        self._append_list(dataset, dc_xml, _dc_ftp_url_full + self.dcftp.ftp.pwd() + "/")
                            except:
                                print "Failt"
                            self.dcftp.ftp.cwd(data_dir)
                        self.dcftp.ftp.cwd(geoid_dir)
                self.dcftp._home()
        self._write()

    def _write(self):
        dc_ufile = open(os.path.join(_out_dir,"dcbounds.py"), 'w')
        print dc_ufile
        dc_ufile.write("dc_surveys = ")
        dc_ufile.write(str(self.surveys))
        dc_ufile.close()

    def pib(self, p, b):
        if p[0] >= b[0] and \
                p[0] <= b[1] and \
                p[1] >= b[2] and \
                p[1] <= b[3]:
            return True
        else: return False

    def bib(self, b, sb):
        if self.pib([b[0],b[3]], sb) or \
           self.pib([b[0],b[2]], sb) or \
           self.pib([b[1],b[3]], sb) or \
           self.pib([b[1],b[2]], sb) or \
           self.pib([sb[2],sb[1]], b) or \
           self.pib([sb[2],sb[0]], b) or \
           self.pib([sb[3],sb[1]], b) or \
           self.pib([sb[3],sb[0]], b):
            return True
        else: return False

    def bfilter(self, b):
        self._slist=[]
        self._bounds = b
        for survey in self.surveys:
            sb = survey[2]
            if self.bib(b, sb):
                self._slist.append(survey)

    def survey_files(self):
        for survey in self._slist:
            tile_list = []
            self.dcftp.ftp.cwd(survey[3].split(".gov")[1])
            datalist = self.dcftp.ftp.nlst()
            print self.dcftp.ftp.pwd()
            print("total tiles in survey: %s\n" %(len(datalist)))
            for dc_file in datalist:
                if ".csv" in dc_file:
                    dc_csv = self.dcftp._fetch_csv(survey[3] + dc_file)
                    for tile in dc_csv:
                        try:
                            tb = (float(tile[1]),float(tile[2]),float(tile[3]),float(tile[4]))
                            if len(self._bounds) > 1:
                                if self.bib(self._bounds, tb):
                                    tile_list.append(tile)
                        except:
                            pass
                for tile in tile_list:
                    self.dcftp.fetch_file(tile[0])
            print("total tiles in bounds: %s\n" %(len(tile_list)))

class dc_ftp:
    def __init__(self):
        self.ftp = ftplib.FTP(_dc_ftp_url)
        self.ftp.login()
        self.ftp.cwd("/pub/DigitalCoast")
        self.parent = self.ftp.pwd()

    def _home(self):
        self.ftp.cwd(self.parent)

    def _list(self):
        self.ftp.retrlines("LIST")

    def fetch_file2(self, url):
        print "downloading " + url
        
        dirname = "./" + os.path.dirname(url)[6:] + "/"
        if not os.path.exists(dirname):
            os.makedirs(dirname)
            
        with open(dirname + os.path.basename(url), "wb") as local_file:
            self.ftp.retrbinary('RETR %s' % url, local_file.write)

    def fetch_file(self, filename):
        print "downloading " + self.ftp.pwd() + "/" + filename
        
        dirname = "." + self.ftp.pwd()
        print dirname
        if not os.path.exists(dirname):
            os.makedirs(dirname)
            
        with open(dirname + "/" + os.path.basename(filename), "wb") as local_file:
            self.ftp.retrbinary('RETR %s' % filename, local_file.write)


    def _fetch_xml(self, url):
        print('%s' %(url))
        #req = urllib2.Request(url)
        response = urllib2.urlopen(url)
        results = response.read()
        response.close()

        return minidom.parseString(results)

    def _fetch_csv(self, url):
        print('scanning %s' %(url))
        #req = urllib2.Request(url)
        response = urllib2.urlopen(url)
        results = response.read()
        response.close()
        
        csv_results = csv.reader(results.split('\n'), delimiter=",")

        return list(csv_results)
        
    def _get_xml_extents(self, xml):
        bounding = xml.getElementsByTagName("bounding")
        for node in bounding:
            wl = node.getElementsByTagName("westbc")[0].firstChild.nodeValue
            el = node.getElementsByTagName("eastbc")[0].firstChild.nodeValue
            sl = node.getElementsByTagName("southbc")[0].firstChild.nodeValue
            nl = node.getElementsByTagName("northbc")[0].firstChild.nodeValue
        return map(float, [wl,el,sl,nl])

    def _get_xml_title(self, xml):
        title = xml.getElementsByTagName("title")
        return title[0].firstChild.nodeValue

    def _get_xml_pubdate(self, xml):
        pdate = xml.getElementsByTagName("pubdate")
        return pdate[0].firstChild.nodeValue

if __name__ == '__main__':
    
    extent = None
    want_index = False

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                      float(sys.argv[i+3]),float(sys.argv[i+4]))
            i = i + 4

        elif arg == '-help' or arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '-version' or arg == '--version':
            print('dcfetch.py v.%s' %(_version))
            print(mblib._license)
            sys.exit(1)

        else:
            print(_usage)
            sys.exit(1)

        i = i + 1

    if extent is None:
        print(_usage)
        sys.exit(1)

    # --

    dcb = dclib.dc_bounds()
    print len(dcb.surveys)
    dcb.bfilter(extent)
    print len(dcb._slist)
    dcb.survey_files()

    #dc._update()
    #--

### End


