unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

function StringListCompareStrings(List: TStringList; Index1, Index2: Integer): Integer;
var
  First: Integer;
  Second: Integer;
begin
  First := StrToInt(List[Index1]);
  Second := StrToInt(List[Index2]);
  if First>Second then
   Result := 1
  else if First=Second then
   Result := 0
  else if First<Second then
   Result := -1;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
SL: TStringList;
begin
SL := TStringlist.Create;

SL.Append('3');
SL.Append('4');
SL.Append('1');
SL.Append('2');
SL.Append('5');

SL.CustomSort(StringListCompareStrings);

Memo1.Lines.AddStrings(SL);

SL.Free;

end;

end.
