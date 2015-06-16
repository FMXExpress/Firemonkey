unit Unit1;

interface

uses
  FMX.Forms, FMX.Edit, FMX.StdCtrls, System.Sensors.Components, FMX.Controls, System.Classes,
  FMX.Types, System.Sensors;

type
  TForm1 = class(TForm)
    LocationSensor1: TLocationSensor;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    AniIndicator1: TAniIndicator;
    procedure Button1Click(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses System.SysUtils;

procedure TForm1.Button1Click(Sender: TObject);
begin
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  LocationSensor1.OnLocationChanged := LocationSensor1LocationChanged;
  LocationSensor1.Active := True;
end;

procedure TForm1.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  AniIndicator1.Enabled := False;
  AniIndicator1.Visible := False;
  Edit1.Text := FloatToStr(NewLocation.Latitude);
  Edit2.Text := FloatToStr(NewLocation.Longitude);
end;

end.
