unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Button1: TButton;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
uses FMX.Platform;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FullScreen := not FullScreen;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  FMXFullScreenWindowService : IFMXFullScreenWindowService;
begin
  FMXFullScreenWindowService := IFMXFullScreenWindowService(TPlatformServices.Current.GetPlatformService(IFMXFullScreenWindowService));
  Button2.Text := FMXFullScreenWindowService.GetFullScreen(Self).ToString();
end;

end.
