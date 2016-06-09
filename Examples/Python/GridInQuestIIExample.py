# Grid InQuest II example calling LibGIQ.dll from Python.
#
# Copyright (C) 2016 Paul Michell, Michell Computing.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Library General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at your
# option) any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
# for more details.

# Setup required library imports.
import sys
import os
from math import radians as deg2rad
from ctypes import *


# Load the GIQ dynamic library.
if sys.platform.startswith('win'):
  LibName = "GIQ.dll"
elif sys.platform.startswith('linux'):
  LibName = "libgiq.so"
elif sys.platform.startswith('darwin'):
  LibName = "libGIQ.dylib"

# Construct the GIQ library file path.
LibFolder = os.path.split(os.path.realpath(__file__))[0]
LibPath = os.path.join(LibFolder, LibName)

# Load the GIQ dynamic library.
if sys.platform.startswith('win'):
  GIQLib = WinDLL(LibPath)
elif sys.platform.startswith('linux'):
  GIQLib = cdll.LoadLibrary(LibPath)
elif sys.platform.startswith('darwin'):
  GIQLib = cdll.LoadLibrary(LibPath)
else:
  print("Unsupported OS")
  exit()

# Define the library coordinate structure.
class coordinates(Structure):
     _fields_ = [("x", c_double),
                 ("y", c_double),
                 ("z", c_double)]

# Reference the library convert function.
Convert = GIQLib.ConvertCoordinates
Convert.argtypes = [c_int, c_int, c_int, c_int, 
                    POINTER(coordinates),
                    POINTER(coordinates), POINTER(c_int)]
Convert.restype = bool

# Setup the calling parameter values.
SRIDSource = c_int(4937) # ETRS89 Geodetic.
RevisionSource = c_int(0) # No revision required.
SRIDTarget = c_int(2157) # Irish Transverse Mercator. 
RevisionTarget = c_int(2015) # Revision for ITM/GM15.  This can also be 2002 for ITM/GM02.
Source = coordinates(deg2rad(-7), deg2rad(53), 100) # Longitude, Latitude, Altitude.
Target = coordinates(0, 0, 0)
Datum = c_int(13) # Malin Head datum.
CallOK = bool(False)

# Call coordinate converter.
CallOK = Convert(SRIDSource, SRIDTarget, RevisionSource, RevisionTarget, Source, Target, Datum)

# Output the result.
if CallOK:
  print("Conversion result...")
  print("ITM Easting: "+str(Target.x))
  print("ITM Northing: "+str(Target.y))
  print("Elevation: "+str(Target.z))
  Datums = {
            0: "None",
            1: "Ordnance Datum Newlyn",
            2: "St. Marys",
            3: "Douglas02",
            4: "Stornoway",
            5: "St. Kilda",
            6: "Lerwick",
            7: "Newlyn",
            8: "FairIsle",
            9: "FlannanIsles",
            10: "North Rona",
            11: "Sule Skerry",
            12: "Foula",
            13: "Malin Head",
            14: "Belfast",
            15: "Offshore",
           }
  print("Vertical Datum: "+Datums[Datum.value])
else:
  print("Error converting coordinates")
