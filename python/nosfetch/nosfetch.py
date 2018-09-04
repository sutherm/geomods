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
from os.path import expanduser
import sys
import urllib2
from xml.dom import minidom
import nos_bounds
import threading
import webbrowser
import Tkinter as tk
import tkFileDialog

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

_out_dir = os.getcwd()

def _set_out_dir(out_dir):
    global _out_dir
    _out_dir = out_dir

def dic_key_list(dic):
    key_list = []
    for i in dic:
        key_list.append(i)
    return key_list

_usage = """

usage: nosfetch.py [-region xmin xmax ymin ymax] [-survey survyeID] [-data datatype] [-list-only] [-process] [-update] [-verbose]

Options:
  -region\tSpecifies the desired input region; xmin xmax ymin ymax')
  -survey\tFetch a specific survey, enter the surveyID here; this will also accept a file with a list of surveyIDs
  -data\t\tSpecify the data type to download; separate datatypes with a `,`
       \t\tPossible types include: %s
  -list_only\tOnly fetch a list of surveys in the given region.
  -process\tGenerate a shell script to convert the downloaded BAG or GEODAS/XYZ data to standard xyz.
  -update\tUpdate the stored list of surveys.
  -verbose\tIncrease verbosity

  -gui\t\tLaunch the nosfetch GUI.

  -help\t\tPrint the usage text
  -version\tPrint the version information

Example:
nosfetch.py -region -90.75 -88.1 28.7 31.25 -data XYZ,BAG

nos_fetch.py v.%s 
""" %(dic_key_list(_nos_extentions), _version)

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
    ## TODO: Fix these for proper bounds check
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

class Application(tk.Frame):

    def __init__(self, master=None, *args, **kwargs):
        tk.Frame.__init__(self, master)
        self.grid(sticky=tk.N+tk.S+tk.E+tk.W)

        self.s = None

        self._ngdc_url="https://www.ngdc.noaa.gov/mgg/bathymetry/hydro.html"
        
        self.sv = tk.StringVar()
        self.sv.trace("w", lambda name, index, mode, var=self.sv: self._entryUpdate(self.sv))

        self.dtGEODAS = tk.IntVar()
        #self.dtXYZ = tk.IntVar()
        self.dtBAG = tk.IntVar()
        self.dtDR = tk.IntVar()
        self.dtSmooth_Sheets = tk.IntVar()
        self.dtTIDES = tk.IntVar()
        self.dtproject_sketches = tk.IntVar()
        self.dtBottom_Samples = tk.IntVar()
        #self.dtHYD93 = tk.IntVar()
        self.dtXML = tk.IntVar()
        self.dtGeoImagePDF = tk.IntVar()
        self.dtt = True

        self.downloading = tk.StringVar(value="idle")

        self.data_types = {'BAG':self.dtBAG, 'DR':self.dtDR, 'Smooth_Sheets':self.dtSmooth_Sheets, 'TIDES':self.dtTIDES, 'project_sketches': self.dtproject_sketches, 'Bottom_Samples':self.dtBottom_Samples, 'XML':self.dtXML, 'GEODAS':self.dtGEODAS, 'GeoImagePDF':self.dtGeoImagePDF, 'XYZ':self.dtGEODAS}
        
        self.nl = nosLib()
        
        self.create_widgets()
        self._listBoxUpdate()
        self.resetDataType()
        self.resetFetchDir()

    def create_widgets(self):
        top=self.winfo_toplevel()
        top.rowconfigure(0, weight=1)
        top.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)
        self.columnconfigure(0, weight=1)

        ## Menu
        self.menuBar = tk.Menu(top, relief=tk.FLAT)
        top['menu'] = self.menuBar
        
        self.fileMenu = tk.Menu(self.menuBar, relief=tk.FLAT, tearoff=0)
        self.surveyMenu = tk.Menu(self.menuBar, relief=tk.FLAT, tearoff=0)
        self.optionsMenu = tk.Menu(self.menuBar, relief=tk.FLAT, tearoff=0)
        self.helpMenu = tk.Menu(self.menuBar, relief=tk.FLAT, tearoff=0)

        self.menuBar.add_cascade(label="File", menu=self.fileMenu)
        self.menuBar.add_cascade(label="Survey", menu=self.surveyMenu)
        self.menuBar.add_cascade(label="Options", menu=self.optionsMenu)
        self.menuBar.add_cascade(label="Help", menu=self.helpMenu)
        
        self.fileMenu.add_command(label="Fetch Listed Surveys", command=self.fetchl)
        self.fileMenu.add_separator()
        self.fileMenu.add_command(label="Save Survey List", state=tk.DISABLED)
        self.fileMenu.add_command(label="Generate Info Table", state=tk.DISABLED)
        self.fileMenu.add_separator()
        self.fileMenu.add_command(label="Quit", command=self.quit)

        self.surveyMenu.add_command(label="Survey Report", command=self.si, state=tk.DISABLED)
        self.surveyMenu.add_command(label="Survey Metadata", command=self.sm, state=tk.DISABLED)
        self.surveyMenu.add_separator()
        self.surveyMenu.add_command(label="Fetch Survey", command=self.fetchs, state=tk.DISABLED)
        
        self.optionsMenu.add_command(label="Set Fetch Directory", command=self.setFetchDir)
        self.optionsMenu.add_command(label="Set Datatypes", command=self.setDataType)
        self.optionsMenu.add_command(label="Reset Datatypes", command=self.resetDataType)
        self.optionsMenu.add_separator()
        self.optionsMenu.add_command(label="Filter Surveys by Region", command=self.rfilter)
        self.optionsMenu.add_command(label="Filter Surveys by Year", command=self.yrfilter)
        self.optionsMenu.add_command(label="Reset Surveys", command=self.resetSurveys)
        self.optionsMenu.add_separator()
        self.optionsMenu.add_command(label="Update Survey DB", command=self.updateSurveys)

        self.helpMenu.add_command(label='About', command=self.about_self)
        self.helpMenu.add_command(label='About NOS Data', command=self.sa)
        
        ## Survey Frame
        self.surveyFrame = tk.Frame(self, padx=5, pady=5)
        self.yScroll = tk.Scrollbar(self.surveyFrame, orient=tk.VERTICAL)
        self.surveyEntry = tk.Entry(self.surveyFrame,textvariable=self.sv)
        self.surveyListBox = tk.Listbox(self.surveyFrame, yscrollcommand=self.yScroll.set, width=60)
       
        self.surveyFrame.rowconfigure(1, weight=1)
        self.surveyFrame.columnconfigure(0, weight=1)
        self.surveyFrame.grid(column=0, columnspan=2,row=0, sticky=tk.N+tk.S+tk.E+tk.W)
        self.surveyEntry.grid(column=0,row=0, sticky=tk.N+tk.S+tk.E+tk.W)
        self.surveyListBox.grid(column=0, row=1, rowspan=2, sticky=tk.N+tk.S+tk.E+tk.W)
        self.surveyListBox.bind('<<ListboxSelect>>', self._listSelect)
        self.yScroll.grid(column=1, row=0, rowspan=3,sticky=tk.N+tk.S+tk.E+tk.W)

        self.yScroll['command'] = self.surveyListBox.yview

        ## status bar        
        self.statusBar = tk.Frame(self, padx=5, pady=5)
        self.statusBar.grid(row=1, column=0, columnspan=2, sticky=tk.N+tk.S+tk.E+tk.W)

        self.statusBar.rowconfigure(0, weight=1)
        self.statusBar.columnconfigure(1, weight=1)
        self.infoLabel = tk.Label(self.statusBar, textvariable=self.downloading)
        self.infoLabel.grid(column=0, row=0, sticky=tk.N+tk.S+tk.E+tk.W)
        self.listLabel = tk.Label(self.statusBar, text=str(len(self.nl.surveys)))
        self.listLabel.grid(column=1, row=0, sticky=tk.E)

        #self.listLabel.bind('<Button-1>', self.stopFetch)

    def _reload_noslib(self):
        self.nl = nosLib()
        
    def _entryUpdate(self, sv):
        tmp_sv = sv.get()
        nslist=[]
        self.nl._reset()
        for i in self.nl.surveys:
            if tmp_sv.upper() in i[0]:
                nslist.append(i)
        self.nl.surveys = nslist
        self._listBoxUpdate()
        
    def _listBoxUpdate(self):
        self.surveyListBox.delete(0,self.surveyListBox.size())
        for h,i in enumerate(self.nl.surveys):
            self.surveyListBox.insert(tk.END, "%s (%s) %s" %(i[0], i[2], i[1]))
            if h&1:
                self.surveyListBox.itemconfig(h, background='gray90')
            
        self.listLabel.config(text=str(len(self.nl.surveys)))

    def _listSelect(self, evt):
        # Note here that Tkinter passes an event object to onselect()
        #w = evt.widget
        if self.surveyListBox.curselection():
            self.surveyMenu.entryconfigure(0,state=tk.NORMAL)
            self.surveyMenu.entryconfigure(1,state=tk.NORMAL)
            self.surveyMenu.entryconfigure(3,state=tk.NORMAL)
            print("Survey: %s" %(self.nl.surveys[int(self.surveyListBox.curselection()[0])][0]))
        
    def _onListSelect(self):
        # Note here that Tkinter passes an event object to onselect()
        #w = evt.widget
        if self.surveyListBox.curselection():
            index = int(self.surveyListBox.curselection()[0])
            value = self.nl.surveys[index]
            self.s = nosSurvey(value[0])
            return True
        else: return False

    def yrfilter(self):
        self.yw = tk.Toplevel(class_="nosfetch")
        self.yw.title("Filter by Year")
        self.by = tk.IntVar(value=0)
        self.ey = tk.IntVar(value=3000)

        self.yearFrame = tk.LabelFrame(self.yw, text="Year", padx=5, pady=5)
        tk.Entry(self.yearFrame, width=10, textvariable=self.by).grid(column=0,row=0, sticky=tk.W)
        tk.Entry(self.yearFrame, width=10, textvariable=self.ey).grid(column=1,row=0, sticky=tk.E)
        tk.Button(self.yearFrame, text="Filter", command=self.yrfilterSurveys).grid(column=0,row=3, columnspan=2,sticky=tk.N+tk.S+tk.E+tk.W)
        
        self.yearFrame.grid(column=0,row=0,columnspan=2, sticky=tk.N+tk.S+tk.E+tk.W)
        
    def rfilter(self):
        self.rw = tk.Toplevel(class_="nosfetch")
        self.rw.title("Filter by Region")
        self.bee = tk.StringVar(value="-90")
        self.bew = tk.StringVar(value="-89")
        self.ben = tk.StringVar(value="40")
        self.bes = tk.StringVar(value="30")

        ## Region Frame
        self.boundsFrame = tk.LabelFrame(self.rw, text="Region", padx=5, pady=5)
        tk.Entry(self.boundsFrame, width=10, textvariable=self.bee).grid(column=0,row=1, sticky=tk.W)
        tk.Entry(self.boundsFrame, width=10, textvariable=self.bew).grid(column=1,row=1, sticky=tk.E)
        tk.Entry(self.boundsFrame, width=10, textvariable=self.ben).grid(column=0,row=0,columnspan=2, sticky=tk.N)
        tk.Entry(self.boundsFrame, width=10, textvariable=self.bes).grid(column=0,row=2,columnspan=2, sticky=tk.S)
        tk.Button(self.boundsFrame, text="Filter", command=self.filterSurveys).grid(column=0,row=3, columnspan=2,sticky=tk.N+tk.S+tk.E+tk.W)

        self.boundsFrame.grid(column=0,row=0,columnspan=2, sticky=tk.N+tk.S+tk.E+tk.W)

    def fetchSurveys(self, fsurveys):
        def callback():
            self.downloading.set("fetching")
            for h,i in enumerate(fsurveys):
                s = nosSurvey(i[0])
                for dt in self.nl._dtypes:
                    #if dt in s._dtypes:
                    s.fetch(dt)
            self.downloading.set("idle")
        t = threading.Thread(target=callback)
        t.start()

    def fetchs(self):
        if self.surveyListBox.curselection():
            index = int(self.surveyListBox.curselection()[0])
            fsurveys = [self.nl.surveys[index]]
            
            self.fetchSurveys(fsurveys)

    def fetchl(self):
        print("Fetching %d Surveys" %(len(self.nl.surveys)))
        self.fetchSurveys(self.nl.surveys)

    def about_self(self):
        self.about = tk.Toplevel(class_="about")
        self.about.title("About nosgui")

        ## About Frame
        self.aboutFrame = tk.LabelFrame(self.about, text="About", padx=5, pady=5)
        self.aboutText = tk.Text(self.aboutFrame)
        self.aboutText.grid(row=0,column=0)
        self.aboutText.insert(tk.INSERT, " \n\
 [ nosGUI ] \n\
------------ \n\
\n\
Fetch and query NOS hydrographic data.\n\
\n\
------------\n\
\n\
\n\
Send questions or comments, etc to <matthew.love@colorado.edu>\n\
")
        self.aboutFrame.grid(column=0,row=0, sticky=tk.N+tk.S+tk.E+tk.W)

    def sa(self):
        print(self._ngdc_url)
        webbrowser.open(self._ngdc_url)
        
    def si(self):
        is_sel = self._onListSelect()
        print(self.s._data_url)
        print(self.s._valid)
        if is_sel:
            if self.s:
                webbrowser.open(self.s._data_url)
        
    def sm(self):
        is_sel = self._onListSelect()
        print(self._ngdc_url)
        if is_sel:
            if self.s:
                webbrowser.open(self.s._xml_url)

    def resetSurveys(self):
        self.nl._reset()
        self.sv.set("")
        self._listBoxUpdate()
        
    def filterSurveys(self):
        self.rw.destroy()
        extent = [float(self.bee.get()),float(self.bew.get()),float(self.bes.get()),float(self.ben.get())]
        self.nl.bfilter(extent)
        self._listBoxUpdate()

    def yrfilterSurveys(self):
        self.yw.destroy()
        self.nl.yrfilter(self.by.get(), self.ey.get())
        self._listBoxUpdate()

    def resetDataType(self):
        self.nl._reset_dtypes()
        for i in self.nl._dtypes:
            self.data_types[i].set(1)
        print("Datatypes: %s" %(self.nl._dtypes))

    def toggleDataTypes(self, evt):
        for i in self.data_types:
            if self.dtt: dti = 0
            else: dti = 1
            self.data_types[i].set(dti)
        self.dtt = not self.dtt
            
    def setDataType(self):
        self.w = tk.Toplevel(class_="test")
        self.w.title("Datatype")
        self.typeCheckFrame = tk.LabelFrame(self.w, text="Data Type")
        self.typeCheckFrame.bind('<Button-1>', self.toggleDataTypes)
        self.typeCheckFrame.grid(row=0, column=0, sticky=tk.N+tk.S+tk.E+tk.W)
        
        for h,i in enumerate(self.data_types):
            if h <= 4:
                tk.Checkbutton(self.typeCheckFrame, text=i, indicatoron=1, variable=self.data_types[i]).grid(column=0,row=h,sticky=tk.W)
            else: tk.Checkbutton(self.typeCheckFrame, text=i, indicatoron=1, variable=self.data_types[i]).grid(column=1,row=h-5,sticky=tk.W)
        tk.Checkbutton(self.typeCheckFrame, text="XYZ", indicatoron=1, variable=self.data_types["GEODAS"]).grid(column=1,row=len(self.data_types)-5,sticky=tk.W)
            
        self.typeCheckOK = tk.Button(self.w, text="OK", command=self._typeCheckChange)        
        self.typeCheckOK.grid(sticky=tk.N+tk.S+tk.E+tk.W)

    def _typeCheckChange(self):
        self.w.destroy()
        dts = []
        for i in self.data_types:
            if self.data_types[i].get() == 1:
                dts.append(i)
        self.nl._set_dtypes(dts)
        print("Datatypes: %s" %(self.nl._dtypes))

    def setFetchDir(self):
        nosDir = tkFileDialog.askdirectory(title="Select A Folder", mustexist=0)
        if nosDir: _set_out_dir(nosDir)
        print("Fetch Directory: %s" %(_out_dir))

    def resetFetchDir(self):
        #noslib._set_out_dir(expanduser("~"))
        _set_out_dir(os.getcwd())
        print("Fetch Directory: %s" %(_out_dir))

    def runUpdate(self):
        def callback():
            nbOb = nosBounds()
            self.downloading.set("updating")
            for i in _nos_directories:
                sl = nbOb._readDir(i)
                for j in sl:
                    nbOb._updateLines(j)
            nbOb._write()
            self.downloading.set("idle")
        t = threading.Thread(target=callback)
        t.start()

    def updateSurveys(self):
        print("updating")
        self.runUpdate()

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

if __name__ == '__main__':

    extent = None
    fetch_list = None
    lst_only = False
    proc = False
    want_update = False
    want_gui = False
    dtype="ALL"
    verbose=False

    nos_file = 'fetch_nos_surveys'
    nl = nosLib()

    i = 1
    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg == '-region':
            extent = (float(sys.argv[i+1]),float(sys.argv[i+2]),
                      float(sys.argv[i+3]),float(sys.argv[i+4]))
            i = i + 4

        elif arg == '-list_only':
            lst_only = True

        elif arg == '-data':
            dtype = sys.argv[i+1]
            i = i + 1
            
        elif arg == '-survey':
            fetch_list = sys.argv[i+1]
            i = i + 1

        elif arg == '-process':
            proc = True

        elif arg == '-update':
            want_update = True

        elif arg == '-gui':
            want_gui = True

        elif arg == '-verbose':
            verbose = True

        elif arg == '-help' or arg == '--help' or arg == '-h':
            print(_usage)
            sys.exit(1)

        elif arg == '-version' or arg == '--version':
            print('nos_fetch.py v.%s' %(_version))
            print(_license)
            sys.exit(1)

        else:
            print(_usage)
            sys.exit(0)

        i = i + 1

    if want_gui:
        root = tk.Tk()
        app = Application(root)
        app.master.title('NOS-Fetch')
        root.mainloop()
        sys.exit(1)

    bounds = extent

    if extent is None and fetch_list is None and want_update is False:
        print(_usage)
        sys.exit(0)
    
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
        nbOb = nosBounds("nos_bounds.py.update")
        for i in _nos_directories:
            sl = nbOb._readDir(i)
            for j in sl:
                nbOb._updateLines(j)
        nbOb._write()

    elif fetch_list:
        s = nosSurvey(fetch_list)
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
