Unit TransMain;

{ Transformation Utility Main Unit.

  Copyright (C) 2015 Paul Michell, Michell Computing.

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details. }

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  SysUtils, Math, Geometry, Geodesy, ETRS, BNG, ITM, IG;

Procedure ProcessTransformation(Const InputFileName: String; Const OutputFileName: String);

Implementation

Procedure ProcessTransformation(Const InputFileName: String; Const OutputFileName: String);
Begin
  WriteLn('File input from: '+InputFileName);
  WriteLn('File output to: '+OutputFileName);
  WriteLn('Available Coordinate Systems: ');
  WriteLn(CoordinateSystems.AvailableSystemsList);
  WriteLn(ITMCoordinateSystem.PreferredVerticalDatum);
End;

End.

