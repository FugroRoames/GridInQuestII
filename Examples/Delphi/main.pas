unit main;

// Minimal Grid Inquest II DLL example for Delphi
// Original conversion by David Wilbourn, DW Consulting

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl;

type
   TCoordinates = record
     X, Y, Z : Double;
   end;
  TConvertCoords =  function (SRIDSource, SRIDTarget, RevisionSource, RevisionTarget : integer; var Source: TCoordinates; var Target: TCoordinates; var Datum: integer):Boolean; stdcall;

  TForm4 = class(TForm)
    edLong: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edLat: TEdit;
    cbDatum: TComboBox;
    Label3: TLabel;
    cbSRIDSource: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    cbSRIDTarget: TComboBox;
    edTargetX: TEdit;
    edTargetY: TEdit;
    Label6: TLabel;
    edAlt: TEdit;
    edTargetZ: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Button1: TButton;
    FileListBox1: TFileListBox;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    function LatLongToOSTN15(Source : TCoordinates; var Target: TCoordinates):Boolean;
    { Public declarations }
  end;

var
  Form4: TForm4;
  SRIDSource : Integer;
  RevisionSource: Integer;
  SRIDTarget: integer;
  RevisionTarget: integer;
  Datum: Integer;
  dllHandle : cardinal;
  ConvertCoords : TConvertCoords;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  S: TCoordinates;
  T: TCoordinates;
begin
  if not(fileexists('GIQ.dll')) then
    begin
    showmessage('GIQ.DLL not found in Application path.');
    exit;
    end;

  FileListBox1.Directory := ExtractFilePath(application.ExeName);
  if FileListBox1.Count = 0 then
    begin
    showmessage('DAT files not found in Application path.');
    exit;
    end;

  SRIDSource := strtoint(copy(cbSRIDSource.Text,0,pos(' ',cbSRIDSource.Text)-1));
  SRIDTarget := strtoint(copy(cbSRIDTarget.Text,0,pos(' ',cbSRIDTarget.Text)-1));
  Datum := strtoint(copy(cbDatum.Text,0,pos(':',cbDatum.Text)-1));
  RevisionSource := 0; // Use latest revision.
  RevisionTarget := 0; // Use latest revision.

  S.X := strtofloat(edLong.Text);
  S.Y := strtofloat(edLat.Text);
  S.Z := strtofloat(edAlt.Text);
  if LatLongToOSTN15(S,T) then
    begin
    edTargetX.Text := floattostr(T.X);
    edTargetY.Text := floattostr(T.Y);
    edTargetZ.Text := floattostr(T.Z);
    end
  else
    showmessage('Conversion failed.');
end;


procedure TForm4.Button2Click(Sender: TObject);
begin
  cbdatum.Itemindex := 13;
  cbSRIDSource.ItemIndex := 1;
  cbSRIDTarget.ItemIndex := 6;
  edLong.Text := '-7';
  edlat.Text := '53';
  edalt.Text := '100';
end;

procedure TForm4.Button3Click(Sender: TObject);
begin
  cbdatum.Itemindex := 0;
  cbSRIDSource.ItemIndex := 1;
  cbSRIDTarget.ItemIndex := 5;
  edLong.Text := '-0.140590';
  edlat.Text := '51.501850';
  edalt.Text := '6';
end;

function TForm4.LatLongToOSTN15(Source : TCoordinates; var Target: TCoordinates):Boolean;
begin
  Source.X := math.DegToRad(Source.X);
  Source.Y := math.DegToRad(Source.Y);

  Target.X := 0;
  Target.Y := 0;
  Target.Z := 0;

  result := false;
  dllHandle := LoadLibrary('GIQ.dll');
  if dllHandle <> 0 then
    begin
    @ConvertCoords := GetProcAddress(dllHandle, 'ConvertCoordinates');
    if Assigned (ConvertCoords) then
      begin
      result := ConvertCoords(SRIDSource,SRIDTarget,RevisionSource,RevisionTarget,Source,Target,Datum);  //call the function
      end;
    FreeLibrary(dllHandle);
    end;
end;

end.
