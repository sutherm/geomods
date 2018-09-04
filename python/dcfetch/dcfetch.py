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
import ftplib
import urllib
import urllib2
from xml.dom import minidom
import csv

try:
    import osgeo.ogr as ogr
except ImportError:
    try:
        import ogr
    except ImportError:
        sys.exit('''
fetch: Sorry: You must have the Python GDAL/OGR bindings for Shapefile support,
Get them here: http://trac.osgeo.org/gdal/wiki/GdalOgrInPython''')

_version = '0.0.3'

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
  -list_only\tOutput a list of urls
  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
dcfetch.py -region -90.75 -88.1 28.7 31.25

dcfetch.py v.%s
""" %(_version)

_dc_ftp_url_full = "ftp://coast.noaa.gov"
_dc_ftp_url = "coast.noaa.gov"

fetchdata = "/usr/local/share/geomods/fetch/"
if "/" not in fetchdata:
    fetchdata = ""

class dc_bounds:
    def __init__(self, extent):
        self.dcftp = dc_ftp()
        self.bounds = extent
        self.bounds2geom()
        
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
            self.make_gmt()

    def create_polygon(self, coords):          
        ring = ogr.Geometry(ogr.wkbLinearRing)
        for coord in coords:
            ring.AddPoint(coord[1], coord[0])

        # Create polygon
        poly = ogr.Geometry(ogr.wkbPolygon)
        poly.AddGeometry(ring)
        return poly.ExportToWkt()

    def make_gmt(self):
        driver = ogr.GetDriverByName('GMT')
        ds = driver.CreateDataSource("digital_coast.gmt")
        layer = ds.CreateLayer("digital_coast", None, ogr.wkbPolygon)
        
        layer.CreateField(ogr.FieldDefn('Name', ogr.OFTString))
        defn = layer.GetLayerDefn()

        layer.CreateField(ogr.FieldDefn('ID', ogr.OFTString))
        defn = layer.GetLayerDefn()

        layer.CreateField(ogr.FieldDefn('Data', ogr.OFTString))
        defn = layer.GetLayerDefn()

        layer.CreateField(ogr.FieldDefn('Date', ogr.OFTString))
        defn = layer.GetLayerDefn()

        for surv in self.surveys:
            b1 = [[surv[2][2], surv[2][0]],
                  [surv[2][2], surv[2][1]],
                  [surv[2][3], surv[2][1]],
                  [surv[2][3], surv[2][0]],
                  [surv[2][2], surv[2][0]]]
            poly = self.create_polygon(b1)
            feat = ogr.Feature( layer.GetLayerDefn())
            feat.SetField("Name", str(surv[1]))
            feat.SetField("ID", str(surv[0]))
            feat.SetField("Data", str(surv[3]))
            feat.SetField("Date", str(surv[4]))

            geom = ogr.CreateGeometryFromWkt(poly)
            feat.SetGeometry(geom)
            layer.CreateFeature(feat)
            feat = geom = None  # destroy these

        ds = layer = feat = geom = None
    
    def bounds2geom(self):
        b1 = [[self.bounds[2], self.bounds[0]],
              [self.bounds[2], self.bounds[1]],
              [self.bounds[3], self.bounds[1]],
              [self.bounds[3], self.bounds[0]],
              [self.bounds[2], self.bounds[0]]]
              
        self.boundsGeom = ogr.CreateGeometryFromWkt(self.create_polygon(b1))

    def search_gmt(self):
        gmt1 = ogr.Open(fetchdata + "digital_coast.gmt")
        layer = gmt1.GetLayer(0)
        for feature1 in layer:
            geom = feature1.GetGeometryRef()
            if self.boundsGeom.Intersects(geom):
                surv_url = feature1.GetField("Data")            
                tile_list = []
                self.dcftp.ftp.cwd(surv_url.split(".gov")[1])
                datalist = self.dcftp.ftp.nlst()
                print self.dcftp.ftp.pwd()
                for dc_file in datalist:
                    if ".csv" in dc_file:
                        dc_csv = self.dcftp._fetch_csv(surv_url + dc_file)
                        for tile in dc_csv:
                            try:
                                tb = [float(tile[1]),float(tile[2]),float(tile[3]),float(tile[4])]
                                t1 = [[tb[2], tb[0]],
                                      [tb[2], tb[1]],
                                      [tb[3], tb[1]],
                                      [tb[3], tb[0]],
                                      [tb[2], tb[0]]]
              
                                tile_geom = ogr.CreateGeometryFromWkt(self.create_polygon(t1))
                                if self.boundsGeom.Intersects(tile_geom):
                                    tile_list.append(tile)
                            except:
                                pass
                    for tile in tile_list:
                        print surv_url + tile[0]
                        #self.dcftp.fetch_file(tile[0])

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
        if not os.path.exists(dirname):
            os.makedirs(dirname)
            
        with open(dirname + "/" + os.path.basename(filename), "wb") as local_file:
            self.ftp.retrbinary('RETR %s' % filename, local_file.write)


    def _fetch_xml(self, url):
        print('%s' %(url))
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

    #dcb = dclib.dc_bounds()
    #print len(dcb.surveys)
    #dcb.bfilter(extent)
    #print len(dcb._slist)
    #dcb.survey_files()

    dcb = dc_bounds(extent)
    #dcb._update()
    dcb.search_gmt()
    #--

### End


