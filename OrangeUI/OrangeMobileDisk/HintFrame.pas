unit HintFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uSkinFireMonkeyControl, uSkinFireMonkeyImage, uSkinFireMonkeyLabel;

type
  TFrameHint = class(TFrame)
    Timer1: TTimer;
    SkinFMXImage1: TSkinFMXImage;
    SkinFMXLabel1: TSkinFMXLabel;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ConnectHint(Hint:String);
    { Public declarations }
  end;

var
  GlobalFrameHint:TFrameHint;


procedure ShowHintFrame(AParent:TFmxObject;AHint:String;Interval:Integer=500);


implementation

{$R *.fmx}

procedure ShowHintFrame(AParent:TFmxObject;AHint:String;Interval:Integer);
begin
  if GlobalFrameHint=nil then
  begin
    GlobalFrameHint:=TFrameHint.Create(Application);
  end;
  GlobalFrameHint.Visible:=True;
  GlobalFrameHint.Parent:=AParent;
  if AParent<>nil then
  begin
    if AParent is TControl then
    begin
      GlobalFrameHint.Position.X:=(TControl(AParent).Width-GlobalFrameHint.Width)/2;
      GlobalFrameHint.Position.Y:=TControl(AParent).Height-120;
    end
    else if AParent is TForm then
    begin
      GlobalFrameHint.Position.X:=(TForm(AParent).Width-GlobalFrameHint.Width)/2;
      GlobalFrameHint.Position.Y:=TForm(AParent).Height-120;
    end;
  end;
  GlobalFrameHint.Timer1.Interval:=Interval;
  GlobalFrameHint.ConnectHint(AHint);
end;


{ TFrameConnectHint }

procedure TFrameHint.ConnectHint(Hint:String);
begin
  Self.Timer1.Enabled:=False;
  Self.Opacity:=1;
  Self.SkinFMXLabel1.Caption:=Hint;
  Self.Timer1.Enabled:=True;
end;

procedure TFrameHint.Timer1Timer(Sender: TObject);
begin
  if Opacity-0.1>0 then
  begin
    Opacity:=Opacity-0.1;
  end
  else
  begin
    Self.Visible:=False;
    Self.Timer1.Enabled:=False;
  end;
end;

end.

