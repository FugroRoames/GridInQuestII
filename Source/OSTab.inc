{ Define OS TNO2 polynomial accuracy. }
{$DEFINE OSTNO2RES_1KM}  { WARNING: This option will add 20Mb data table to the executable! }
//{$DEFINE OSTNO2RES_10KM}
//{$DEFINE OSTNO2RES_100KM}

{ Include requested TN02 array data. }
{$IFDEF OSTNO2RES_1KM}
  {$DEFINE OSTNO2TABLEUSED}
{$ENDIF}
{$IFDEF OSTNO2RES_10KM}
  {$DEFINE OSTNO2TABLEUSED}
{$ENDIF}
{$IFDEF OSTNO2RES_100KM}
  {$DEFINE OSTNO2TABLEUSED}
{$ENDIF}

