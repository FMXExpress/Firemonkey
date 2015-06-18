unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, Fmx.stdctrls;

type
  TForm1 = class(TForm)
    procedure FormTouch(Sender: TObject; const Touches: TTouches;
      const Action: TTouchAction);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  i: Integer;
begin
  Form1.DeleteChildren;

  if Action<>TTouchAction.Up then
  begin
    for i := 0 to Length(Touches)-1 do
    begin
      with Tlabel.create(Self) do
      begin
        Parent:=Self;
        StyledSettings:=StyledSettings-[TstyledSetting.Size];
        Autosize:=True;
        TextSettings.Font.Size:=40;
        Text:='LABEL '+inttostr(i);
        TextSettings.Trimming:=TTextTrimming.None;
        TextSettings.WordWrap:=False;
        Position.Point:=Touches[i].Location;
      end;
    end;

  end;
end;

end.
