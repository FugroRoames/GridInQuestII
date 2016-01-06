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
import os
from math import radians as deg2rad
from ctypes import *

# Load the LibGIQ dynamic library.
SourceFolder = os.path.dirname(__file__)
giqdll = WinDLL(os.path.join(SourceFolder, "LibGIQ.dll"))

# Define the library coordinate structure.
class coordinates(Structure):
     _fields_ = [("x", c_double),
                 ("y", c_double),
                 ("z", c_double)]

# Reference the library convert function.
convert = giqdll.ConvertCoordinates
convert.argtypes = [c_int, c_int, POINTER(coordinates),
                    POINTER(coordinates), POINTER(c_int)]
convert.restype = bool

# Setup the calling parameter values.
SRIDsource = c_int(4937) # ETRS89 Geodetic
SRIDtarget = c_int(2157) # Irish Transverse Mercator (ITM/GM15)
source = coordinates(deg2rad(-7), deg2rad(53), 100) # Longitude, Latitude, Altitude
target = coordinates(0, 0, 0)
datum = c_int(0)
callok = bool(False)

# Call coordinate converter.
callok = convert(SRIDsource, SRIDtarget, source, target, datum)

# Output the result.
if callok:
  print("Conversion results:")
  print("X: "+str(target.x))
  print("Y: "+str(target.y))
  print("Z: "+str(target.z))
else:
  print("Error converting coordinates")
