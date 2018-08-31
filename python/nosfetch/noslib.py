#!/usr/bin/env python
### noslib.py
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
import nos_bounds

_version = '1.0.4'

_license = """
version %s
Copyright (c) 2012, 2013, 2014, 2016, 2017, 2018 Matthew Love <matthew.love@colorado.edu>

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

_nos_dtypes = ['BAG', 'DR', 'Smooth_Sheets', 'TIDES', 'project_sketches', 'Bottom_Samples', 'XML', 'GEODAS', 'GeoImagePDF', 'XYZ']
_nos_directories = ["B00001-B02000/", "D00001-D02000/", "F00001-F02000/", "H00001-H02000/", "H02001-H04000/", "H04001-H06000/", "H06001-H08000/", "H08001-H10000/", "H10001-H12000/", "H12001-H14000/", "L00001-L02000/", "L02001-L04000/", "T00001-T02000/", "W00001-W02000/"]

_nos_extentions = {'BAG':[".bag.gz"],
                   'GEODAS': [".xyz.gz", ".a93.gz"],
                   'XYZ': [".xyz.gz", ".a93.gz"],
                   'DR':[".pdf"],
                   'Smooth_Sheets':[".pdf",".sid.gz",".tif.gz"],
                   'TIDES':[".pdf"],
                   'project_sketches':[".jpg"],
                   'Bottom_Samples':[".kml", ".txt.", ".pdf"],
                   'XML':[".xml"],
                   'GeoImagePDF':[".pdf"],
                   'ALL':[".xyz.gz",".bag.gz",".pdf",".sid.gz",".tif.gz",".jpg",".kml",".a93"]}

_nos_xml_base_url = "https://www.ngdc.noaa.gov/metadata/published/NOAA/NESDIS/NGDC/MGG/NOS/"
_nos_xml_end_url = "iso/xml/"
_nos_bd_url = "https://data.ngdc.noaa.gov/platforms/ocean/nos/coast/"
_nos_data_url = "https://www.ngdc.noaa.gov/nos/"

#_out_dir = os.path.dirname(os.path.realpath(__file__))
_out_dir = os.getcwd()

def _set_out_dir(out_dir):
    global _out_dir
    _out_dir = out_dir

class nosBounds:

    def __init__(self, append=True, nb_file="nos_bounds.py"):
        self._append = append
        self._nb_file = nb_file
        self.nos_directories = _nos_directories
        self._reset()

    def _reset(self):
        reload(nos_bounds)
        if self._append: self.s_list = nos_bounds.nos_surveys
        else: self.s_list = []

    def _reload(self):
        reload(nos_bounds)

    def _readDir(self, nosdir):
        nos_dir = urllib2.urlopen(_nos_xml_base_url+nosdir+_nos_xml_end_url).readlines()
        return nos_dir

    def _itemInSurveys(self, survey_id):
        for s in nos_bounds.nos_surveys:
            if survey_id in s:
                return True
        return False

    def _appendItem(self, survey_id):
        s = nosSurvey(survey_id)
        #print s._id
        #print s._xml_url
        if s._valid:
            print("noslib: Appending survey: %s" %(s._id))
            s_entry = [s._id, s._extents, s._date]
            self.s_list.append(s_entry)

    def _updateItem(self, item):
        sis = False
        if '.xml' in item:
            survey_id = item[item.index(".xml\">"):item.index("</a>")][6:-4]
            if self._itemInSurveys(survey_id): sis = True
            if not sis: self._appendItem(survey_id)
            sis = False
        
    def _updateLines(self, item):
        sis = False
        if ".xml" in item:
            survey_id = item[item.index(".xml\">"):item.index("</a>")][6:-4]
            #print survey_id
            if self._itemInSurveys(survey_id): sis = True
            if not sis: self._appendItem(survey_id)
            sis = False
    
    def _updateDir(self, nosdir):
        sis = False
        survey_lines = self._readDir(nosdir)
        for h,item in enumerate(survey_lines):
            if ".xml" in item:
                survey_id = item[item.index(".xml\">"):item.index("</a>")][6:-4]
                if self._itemInSurveys(survey_id): sis = True
                if not sis: self._appendItem(survey_id)
                sis = False
        
    def _update(self):
        sis = False
        for j in self.nos_directories:
            survey_lines = urllib2.urlopen(_nos_xml_base_url+j+_nos_xml_end_url).readlines()
            for h,item in enumerate(survey_lines):
                if ".xml" in item:
                    survey_id = item[item.index(".xml\">"):item.index("</a>")][6:-4]
                    if self._itemInSurveys(survey_id): sis = True
                    if not sis: self._appendItem(survey_id)
                    sis = False

    def _write(self):
        nos_ufile = open(os.path.join(_out_dir,"nos_bounds.py"), 'w')
        print nos_ufile
        nos_ufile.write("nos_surveys = ")
        nos_ufile.write(str(self.s_list))
        nos_ufile.close()

class nosSurvey:

    def __init__(self, surveyID):
        
        self._verbose = False
        self._id = surveyID
        self.ndirs = _nos_directories        
        self.dtypes = _nos_dtypes

        self._directory = self.surveyDirectory()
        self._xml_url = _nos_xml_base_url+self._directory+_nos_xml_end_url+self._id+".xml"
        self._data_url = _nos_data_url+self._directory+self._id+".html"
        self._dir_url = _nos_bd_url+self._directory+self._id+"/"
        self._valid = self.surveyp()
        #self._valid = True

        self._dtypes = self.which_nos()
        self._extents = self.get_extents()
        self._title = self.get_title()
        self._date = self.get_date()
        self._datums = self.get_datums()
        self._resolution = self.get_resolution()
        self._cancel = False

    def _fcancel(self):
        self._cancel = True

    def _fcanceled(self):
        self._cancel = False
        
    def floatp(self, val):
        try: 
            float(val)
            return True
        except: return False
        
    def surveyp(self):
        try:
            self._dir_lines = urllib2.urlopen(self._dir_url).readlines()
            self._xml_lines = urllib2.urlopen(self._xml_url).read()
            self._xml_doc = minidom.parseString(self._xml_lines)
            #self.get_extents()
            return True
        except: 
            self._dir_lines = []
            self._xml_lines = ""
            self._xml_doc = None
            return False

    # output is [survey Letter, survey Number, survey NLetter]
    def processSurveyID(self):
        si = self._id
        sl = si[0]
        sn = int(si[1:6])*0.00001
        snl = si[6:]

        if snl == "IA" or snl == "IB":
            snl = ""
        return [sl,sn,snl]

    def surveyDirectory(self):

        sl,sn,sn1 = self.processSurveyID()

        if sl.upper() == "B": return self.ndirs[0]
        elif sl.upper() == "D": return self.ndirs[1]
        elif sl.upper() == "F": return self.ndirs[2]
        elif sl.upper() == "H":
            if sn <= 0.02: return self.ndirs[3]
            elif sn > 0.02 and sn <= 0.04: return self.ndirs[4]
            elif sn > 0.04 and sn <= 0.06: return self.ndirs[5]
            elif sn > 0.06 and sn <= 0.08: return self.ndirs[6]
            elif sn > 0.08 and sn <= 0.10: return self.ndirs[7]
            elif sn > 0.10 and sn <= 0.12: return self.ndirs[8]
            elif sn > 0.12 and sn <= 0.14: return self.ndirs[9]
        elif sl.upper() == "L":
            if sn <= 0.02: return self.ndirs[10]
            elif sn > 0.02 and sn <= 0.04: return self.ndirs[11]
        elif sl.upper() == "T": return self.ndirs[12]
        elif sl.upper() == "W": return self.ndirs[13]
            
    def which_nos(self):
        survey_types = {}

        if self._valid: survey_types['XML'] = [[self._xml_url], [self._id+".xml"]]
        for item in self._dir_lines:
            for _dt in _nos_dtypes:
                if _dt in item:
                    _dturls, _dtnames = [], []
                    _dtlines = urllib2.urlopen(self._dir_url+_dt).readlines()
                    for _dtline in _dtlines:
                        for exts in _nos_extentions[_dt]:
                            if exts in _dtline:
                                _dtfile = _dtline[_dtline.index(exts+"\">"):_dtline.index("</a>")][len(exts)+2:]
                                _dturls.append(self._dir_url+_dt+"/"+_dtfile)
                                _dtnames.append(_dtfile)
                    survey_types[_dt] = [_dturls,_dtnames]
        return survey_types

    def get_extents(self):
        # Extent
        wl, el, sl, nl = -9999, -9999, -9999, -9999
        if self._valid: bounding = self._xml_doc.getElementsByTagName("gmd:EX_GeographicBoundingBox")
        else: bounding = []
        if len(bounding) > 1:
            for node in bounding:
                wl_xgc = node.getElementsByTagName("gmd:westBoundLongitude")
                for i in wl_xgc:
                    wl = i.getElementsByTagName("gco:Decimal")[0].firstChild.nodeValue
                el_xgc = node.getElementsByTagName("gmd:eastBoundLongitude")
                for i in el_xgc:
                    el = i.getElementsByTagName("gco:Decimal")[0].firstChild.nodeValue
                sl_xgc = node.getElementsByTagName("gmd:southBoundLatitude")
                for i in sl_xgc:
                    sl = i.getElementsByTagName("gco:Decimal")[0].firstChild.nodeValue
                nl_xgc = node.getElementsByTagName("gmd:northBoundLatitude")
                for i in nl_xgc:
                    nl = i.getElementsByTagName("gco:Decimal")[0].firstChild.nodeValue
        return [wl,el,sl,nl]

    def get_title(self):
        xmlt = "None"
        if self._valid: xml_ident = self._xml_doc.getElementsByTagName("gmd:MD_DataIdentification")
        else: xml_ident = []
        for t in xml_ident:
            xml_citation = t.getElementsByTagName("gmd:CI_Citation")
            xml_title = xml_citation[0].getElementsByTagName("gmd:title")
            xmlt = xml_title[0].getElementsByTagName("gco:CharacterString")[0].firstChild.nodeValue
        return xmlt

    def get_date(self):
        xmld = "Unknown"
        if self._valid: xml_ident = self._xml_doc.getElementsByTagName("gmd:MD_DataIdentification")
        else: xml_ident = []
        for t in xml_ident:
            xml_citation = t.getElementsByTagName("gmd:CI_Citation")
            xml_date = xml_citation[0].getElementsByTagName("gmd:date")
            for n in xml_date:
                xmld = n.getElementsByTagName("gco:Date")[0].firstChild.nodeValue
        return xmld

    def get_datums(self):
        hdatum, vdatum = "Unknown", "Unknown"
        if self._valid: datums = self._xml_doc.getElementsByTagName("gmd:MD_ReferenceSystem")
        else: datums = []
        if len(datums) >= 1:
            datum_strings=[]
            for node in datums:
                datum_strings.append(node.getElementsByTagName("gco:CharacterString")[0].firstChild.nodeValue)
            if len(datum_strings) == 1:
                hdatum = datum_strings[0].strip()
            if len(datum_strings) == 2:
                hdatum = datum_strings[0].strip()
                vdatum = datum_strings[1].strip()
        return [hdatum, vdatum]

    def get_resolution(self):
        # Resolution
        xmres = "Unknown"
        if self._valid: xml_resolution = self._xml_doc.getElementsByTagName("gmd:spatialResolution")
        else: xml_resolution = []
        if len(xml_resolution) >= 1:
            for node in xml_resolution:
                if node.nodeValue:
                    xmres = node.getElementsByTagName("gco:Integer")[0].firstChild.nodeValue
        return xmres

    def fetch(self, dtype):
        if self._cancel: self._fcanceled()
        if dtype in self._dtypes:
            for h,i in enumerate(self._dtypes[dtype][0]):
                if self._cancel:
                    self._fcanceled()
                    break
                f = urllib2.urlopen(i)
                #print("Fetching %s" %(self._dtypes[dtype][1][h]))
                outf = open(os.path.join(_out_dir,self._dtypes[dtype][1][h]), 'wb')
                outf.write(f.read())
                f.close()
                outf.close()
    
class nosLib:

    def __init__(self):

        self._verbose = False
        self._dtypes = _nos_dtypes
        self.surveys = nos_bounds.nos_surveys
        self._cancel = False

    def _fcancel(self):
        self._cancel = True

    def _fcanceled(self):
        self._cancel = False
        
    def dic_key_list(self, dic):
        key_list = []
        for i in dic:
            key_list.append(i)
        return key_list

    def process_ns(self):
        ol = []
        for i in self.surveys:
            if "_" in i[0]:
                ol.append(i[0].split("_")[0])
            elif "." in i[0]:
                ol.append(i[0].split(".")[0])
        return ol

    def _reload_bounds(self):
        reload(nos_bounds)
    
    def _reset(self):
        self.surveys = nos_bounds.nos_surveys

    def _set_dtypes(self, args):
        self._dtypes = []
        for arg in args:
            self._dtypes.append(arg)
        
    def _reset_dtypes(self):
        self._dtypes = _nos_dtypes

    # b = [minx,maxx,miny,maxy]
    def pib(self, p, b):
        if p[0] >= b[0] and \
                p[0] <= b[1] and \
                p[1] >= b[2] and \
                p[1] <= b[3]:
            return True
        else: return False
        
    def bfilter(self, b):
        sib=[]
        for survey in self.surveys:
            sb = survey[1]
            if self.pib([b[0],b[3]], sb) or \
                    self.pib([b[0],b[2]], sb) or \
                    self.pib([b[1],b[3]], sb) or \
                    self.pib([b[1],b[2]], sb) or \
                    self.pib([sb[2],sb[1]], b) or \
                    self.pib([sb[2],sb[0]], b) or \
                    self.pib([sb[3],sb[1]], b) or \
                    self.pib([sb[3],sb[0]], b):
                sib.append(survey)
        self.surveys = sib

    def yrfilter(self, start_year=0, end_year=3000):
        sidr = []
        for survey in self.surveys:
            sy = survey[2]
            if sy >= start_year and sy <= end_year:
                sidr.append(survey)
        self.surveys = sidr
        
    def fetch(self):
        if self._cancel: self._fcanceled()
        for h, i in enumerate(self.surveys):
            if self._cancel:
                self._fcanceled()
                break
            s = nosSurvey(i[0])
            if self._verbose: print("noslib: Fetching %s datatypes for survey: %s" %(self._dtypes, i[0]))
            for dt in self._dtypes:
                if dt in s._dtypes:
                    s.fetch(dt)

### End
