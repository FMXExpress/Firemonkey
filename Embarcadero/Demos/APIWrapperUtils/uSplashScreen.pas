unit uSplashScreen;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TSpashScreenForm = class(TForm)
    timSplashScreen: TTimer;
    Image1: TImage;
    Label1: TLabel;
    procedure timSplashScreenTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SpashScreenForm: TSpashScreenForm = nil;

procedure ShowSplashScreen;
procedure HideSplashScreen;

implementation

{$R *.fmx}

procedure ShowSplashScreen;
begin
  if (not Assigned(SpashScreenForm)) then
    SpashScreenForm := TSpashScreenForm.Create(Application);
  SpashScreenForm.Show;
  SpashScreenForm.timSplashScreen.Enabled := False;
  SpashScreenForm.timSplashScreen.Enabled := True;
end;

procedure HideSplashScreen;
begin
  if (Assigned(SpashScreenForm)) then
  begin
    SpashScreenForm.Close;
  end;
end;

procedure TSpashScreenForm.timSplashScreenTimer(Sender: TObject);
begin
  timSplashScreen.Enabled := False;
  HideSplashScreen;
end;

end.
