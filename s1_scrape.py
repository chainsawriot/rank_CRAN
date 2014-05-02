# -*- coding: utf-8 -*-
from bs4 import BeautifulSoup
import urllib2
import csv
import time
import re
import pickle

def getTr():
    firstpageurl = "http://cran.rstudio.org/web/packages/available_packages_by_name.html"
    htmldata = urllib2.urlopen(firstpageurl).read()
    htmldatasoup = BeautifulSoup(htmldata)
    meat = htmldatasoup.find("table", summary = "Available CRAN packages by name.")
    alltr = [goodtr for goodtr in meat.find_all("tr") if len(goodtr.find_all("td")) > 0]
    return alltr

def extractPkg(package_name):
    res = {}
    urltofetch = "http://cran.rstudio.org/web/packages/" + package_name + "/index.html"
    error = 0
    while error <= 3:
        try:
            pkgdata = BeautifulSoup(urllib2.urlopen(urltofetch).read())
            break
        except:
            error += 1
            print "retry in 3s"
            time.sleep(3)
    if error >= 3:
        return None
    res['long_desc'] = pkgdata.p.get_text()
    pkgtr = pkgdata.find_all("tr")
    for tr in pkgtr:    
        trName = re.sub(ur"[^a-zA-Z]","", tr.td.string)
        if trName in ["Depends", "Suggests", "Imports", "Inviews", "Version", "License"]:
            res[trName] = tr.find_all("td")[1].get_text()
            #print trName + " is add."
    return res

def extractData(canTr, slptime = 1):
    package_name = canTr.a.string
    print "Scraping: " + package_name
    desc = canTr.find_all("td")[1].string
    res = extractPkg(package_name)
    if res is None:
        return {"package_name": package_name, "failed": True}
    res['package_name'] = package_name
    res['desc'] = desc
    #print res
    time.sleep(slptime)
    return res

def cleanRow(res):
    row = []
    for x in ["package_name","desc","long_desc","Depends", "Suggests", "Imports", "Inviews", "Version", "License"]:
        if res.has_key(x):
            row.append(re.sub("\\n", " ", res[x]).encode("utf-8"))
        else:
            row.append("")
    return row


def loop(csvfilename):
    failed = []
    f = open(csvfilename, "w")
    csvwriter = csv.writer(f)
    csvwriter.writerow(["package_name","desc","long_desc","Depends", "Suggests", "Imports", "Inviews", "Version", "License"])
    for canTr in getTr():
        res = extractData(canTr)
        if res.has_key("failed"):
            print res['package_name'] + " scrapping failed."
            failed.append(res['package_name'])
        else:
            csvwriter.writerow(cleanRow(res))
            print res['package_name'] + " is extracted."
    pickle.dump(failed, open("failed.p", "wb"))
    f.close()
loop("cran_data.csv")
