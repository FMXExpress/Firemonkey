unit UMotionManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeCMMotionManager,
  FMX.StdCtrls, FMX.Objects, FMX.TMSNativeUICore, Math;

type
  TForm1185 = class(TForm)
    TMSFMXNativeCMMotionManager1: TTMSFMXNativeCMMotionManager;
    Button1: TButton;
    Panel1: TPanel;
    Ellipse1: TEllipse;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1185: TForm1185;

implementation

{$R *.fmx}

procedure TForm1185.Button1Click(Sender: TObject);
begin
  if TMSFMXNativeCMMotionManager1.DeviceMotionAvailable then
  begin
    TMSFMXNativeCMMotionManager1.StartDeviceMotionUpdates(
    procedure (AData: TTMSFMXNativeCMDeviceMotion)
    begin
      Ellipse1.Position.X := Max(0,Min(Panel1.Width - Ellipse1.Width, Ellipse1.Position.X + (AData.Attitude.Roll * 20)));
      Ellipse1.Position.Y := Max(0,Min(Panel1.Height - Ellipse1.Height, Ellipse1.Position.Y + (AData.Attitude.Pitch * 20)));
      if PtInRect(RectF(Rectangle1.Position.X, Rectangle1.Position.Y, Rectangle1.Position.X + Rectangle1.Width,
        Rectangle1.Position.Y + Rectangle1.Height), PointF(Ellipse1.Position.X, Ellipse1.Position.Y)) then
        Label2.Text := inttostr(strtoint(Label2.Text) + 1)
      else
        Label2.Text := '0';
    end
    );
  end
  else
    ShowMessage('Device Motion is not available on this device');
end;

end.
