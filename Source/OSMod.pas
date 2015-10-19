Unit OSMod;

{$IFDEF FPC}
  {$MODE OBJFPC}
  {$LONGSTRINGS ON}
{$ENDIF}

Interface

Uses
  Math, Geometry, Geodesy;

{ Define OS TNO2 polynomial accuracy. }
{$DEFINE OSTNO2RES_1KM}  { WARNING: This option will add 20Mb data table to the executable! }
//{$DEFINE OSTNO2RES_10KM}
//{$DEFINE OSTNO2RES_100KM}

{ Include requested TN02 array data. }
{$IFDEF OSTNO2RES_1KM}
  {$INCLUDE OSTN02R1K.inc}
  {$DEFINE OSTNO2TABLEUSED}
{$ELSE}
  {$IFDEF OSTNO2RES_10KM}
    {$INCLUDE OSTN02R10K.inc}
    {$DEFINE OSTNO2TABLEUSED}
  {$ELSE}
    {$IFDEF OSTNO2RES_100KM}
      {$INCLUDE OSTN02R100K.inc}
      {$DEFINE OSTNO2TABLEUSED}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

Type TVerticalDatumModel = (OSGM02, OSVRF10);

{$IFDEF OSTNO2TABLEUSED}
Procedure BilinearInterpolate(Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);
{$ENDIF}

Implementation

{$IFDEF OSTNO2TABLEUSED}
Procedure BilinearInterpolate(Const Coordinates: TCoordinates; Const GridScale: TCoordinate; Var EastOffset, NorthOffset, GeoidHeight: TCoordinate);
Var
  InvGridScale: TCoordinate;
  X1, X2, Y1, Y2: Integer;
  SE0, SE1, SE2, SE3: TCoordinate;
  SN0, SN1, SN2, SN3: TCoordinate;
  SG0, SG1, SG2, SG3: TCoordinate;
  T, TI, U, UI: TCoordinate;
Begin
  InvGridScale := 1/GridScale;
  X1 := Trunc(Coordinates.Easting*InvGridScale);
  Y1 := Trunc(Coordinates.Northing*InvGridScale);
  X2 := X1+1;
  Y2 := Y1+1;
  With OSTN02Data[X1, Y1] Do
    Begin
      SE0 := Easting;
      SN0 := Northing;
      SG0 := Elevation;
    End;
  With OSTN02Data[X2, Y1] Do
    Begin
      SE1 := Easting;
      SN1 := Northing;
      SG1 := Elevation;
    End;
  With OSTN02Data[X2, Y2] Do
    Begin
      SE2 := Easting;
      SN2 := Northing;
      SG2 := Elevation;
    End;
  With OSTN02Data[X1, Y2] Do
    Begin
      SE3 := Easting;
      SN3 := Northing;
      SG3 := Elevation;
    End;
  T := (Coordinates.Easting-(X1*GridScale))*InvGridScale;
  TI := (1-T);
  U := (Coordinates.Northing-(Y1*GridScale))*InvGridScale;
  UI := (1-U);
  EastOffset := (TI*UI*SE0)+(T*UI*SE1)+(T*U*SE2)+(TI*U*SE3);
  NorthOffset := (TI*UI*SN0)+(T*UI*SN1)+(T*U*SN2)+(TI*U*SN3);
  GeoidHeight := (TI*UI*SG0)+(T*UI*SG1)+(T*U*SG2)+(TI*U*SG3);
End;
{$ENDIF}

End.

