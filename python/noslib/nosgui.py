#!/usr/bin/env python
### nosgui.py
##
## Copyright (c) 2012, 2013, 2014, 2016, 2017, 2018 Matthew Love
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

import sys
import os
from os.path import expanduser
import threading
import webbrowser
import Tkinter as tk
import tkFileDialog
import noslib

_nosgui_version = '0.1.4'

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
        
        self.nl = noslib.nosLib()
        
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
        reload(noslib)
        self.nl = noslib.nosLib()
        
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
            self.s = noslib.nosSurvey(value[0])
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
                s = noslib.nosSurvey(i[0])
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
        if nosDir: noslib._set_out_dir(nosDir)
        print("Fetch Directory: %s" %(noslib._out_dir))

    def resetFetchDir(self):
        #noslib._set_out_dir(expanduser("~"))
        noslib._set_out_dir(os.getcwd())
        print("Fetch Directory: %s" %(noslib._out_dir))

    def runUpdate(self):
        def callback():
            nbOb = noslib.nosBounds()
            self.downloading.set("updating")
            for i in noslib._nos_directories:
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
        
if __name__ == '__main__':
    root = tk.Tk()
    app = Application(root)
    app.master.title('NOS-Fetch')
    #imgicon = tk.PhotoImage(file=os.path.join('./','favicon.ico'))
    #app.master.tk.call('wm', 'iconphoto', app.master._w, imgicon)
    #app.master.iconbitmap(os.path.join('./', 'favicon.ico'))
    root.mainloop()
