unit MessageHintFrame;

interface

uses
  System.SysUtils,uFuncCommon, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  uBufferBitmap,Math,
  uSkinLabelType,
  uComponentType,
  uSkinFireMonkeyControl, uSkinFireMonkeyImage, uSkinFireMonkeyLabel;

type
  TFrameMessageHint = class(TFrame)
    SkinFMXImage1: TSkinFMXImage;
    lblMessageHint: TSkinFMXLabel;
    tmrBringToFont: TTimer;
    tmrAlpha: TTimer;
    procedure tmrAlphaTimer(Sender: TObject);
    procedure tmrBringToFontTimer(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ShowHint(Parent:TFmxObject;Hint:String;X,Y:Integer);
    { Public declarations }
  end;

var
  GlobalMessageHintFrame:TFrameMessageHint;

procedure ShowMessageHintFrame(Parent:TFmxObject;Hint:String;X:Integer=-1;Y:Integer=-1);overload;
procedure ShowMessageHintFrame(Parent:TFmxObject;HintList:TStringList;X:Integer=-1;Y:Integer=-1);overload;

implementation

{$R *.fmx}
procedure ShowMessageHintFrame(Parent:TFmxObject;HintList:TStringList;X:Integer=-1;Y:Integer=-1);
var
  I: Integer;
  Hint:String;
begin
  Hint:='';
  for I := 0 to HintList.Count-1 do
  begin
    Hint:=Hint+HintList[I]+#13#10;
  end;
  ShowMessageHintFrame(Parent,Hint,X,Y);
end;

procedure ShowMessageHintFrame(Parent:TFmxObject;Hint:String;X,Y:Integer);
begin
  if GlobalMessageHintFrame=nil then
  begin
    GlobalMessageHintFrame:=TFrameMessageHint.Create(Application);
  end;
  GlobalMessageHintFrame.ShowHint(Parent,Hint,X,Y);
end;

{ TFrameConnectHint }

procedure TFrameMessageHint.ShowHint(Parent:TFmxObject;Hint:String;X,Y:Integer);
begin


  Self.tmrAlpha.Enabled:=False;
  Self.Opacity:=1;
  Self.lblMessageHint.Caption:=Hint;
  Self.Width:=GetStringWidth(Hint,RectF(0,0,Ceil(Width),Ceil(Height)),
    TSkinLabelDefaultMaterial(Self.lblMessageHint.SelfOwnMaterial).DrawCaptionParam)+30;
  Self.Height:=GetStringHeight(Hint,RectF(0,0,Ceil(Width),Ceil(Height)),
    TSkinLabelDefaultMaterial(Self.lblMessageHint.SelfOwnMaterial).DrawCaptionParam)+20;




  Self.Parent:=Parent;
  Self.Visible:=True;

  if X=-1 then
  begin
    Self.Position.X:=(GetControlParentWidth(Parent)-Self.Width)/2;
  end
  else
  begin
    Self.Position.X:=X;
  end;

  if Y=-1 then
  begin
//    Self.Position.Y:=GetControlParentHeight(Parent)-120;
    Self.Position.Y:=(GetControlParentHeight(Parent)-Self.Height)/2;
  end
  else
  begin
    Self.Position.Y:=Y;
  end;


  Self.BringToFront;


  Self.tmrAlpha.Enabled:=True;
end;

procedure TFrameMessageHint.tmrAlphaTimer(Sender: TObject);
begin
  if Opacity-0.1>0 then
  begin
    Opacity:=Opacity-0.1;
  end
  else
  begin
    Self.Visible:=False;
    Self.tmrAlpha.Enabled:=False;
  end;
end;

procedure TFrameMessageHint.tmrBringToFontTimer(Sender: TObject);
begin
  Self.BringToFront;
end;

end.
