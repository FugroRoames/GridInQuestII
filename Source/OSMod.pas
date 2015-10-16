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

Implementation

End.

