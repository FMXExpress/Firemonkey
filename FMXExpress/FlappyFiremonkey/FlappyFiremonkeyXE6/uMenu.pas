unit uMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Ani, FMX.Effects, FMX.Platform, fOpen;

type
  TMenuForm = class(TForm)
    PlayBTN: TButton;
    Image1: TImage;
    GroundA: TImage;
    Ground: TRectangle;
    Image2: TImage;
    Rectangle1: TRectangle;
    LogoLBL: TLabel;
    GlowEffect3: TGlowEffect;
    FMonkeyA: TImage;
    FloatAnimation1: TFloatAnimation;
    SiteBTN: TButton;
    StyleBookW: TStyleBook;
    procedure PlayBTNClick(Sender: TObject);
    procedure SiteBTNClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  MenuForm: TMenuForm;

implementation

{$R *.fmx}

uses uGame;

procedure TMenuForm.PlayBTNClick(Sender: TObject);
begin
GameForm.ResetGame;
MenuForm.Hide;
{$IFDEF MSWINDOWS}
GameForm.ShowModal;
{$ELSE}
GameForm.Show;
{$ENDIF}
end;

procedure TMenuForm.SiteBTNClick(Sender: TObject);
begin
TMisc.Open('http://www.fmxexpress.com/');
end;

function TMenuForm.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.FinishedLaunching: begin end;
    TApplicationEvent.BecameActive:
    begin
    end;
    TApplicationEvent.WillBecomeInactive:
    begin
    end;
    TApplicationEvent.EnteredBackground:
    begin
    end;
    TApplicationEvent.WillBecomeForeground:
    begin
    end;
    TApplicationEvent.WillTerminate: begin
    end;
    TApplicationEvent.LowMemory: begin end;
    TApplicationEvent.TimeChange: begin end;
    TApplicationEvent.OpenURL: begin end;
  end;
  Result := True;
end;

end.
