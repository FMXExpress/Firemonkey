unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Effects,
  FMX.Filter.Effects, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    ImageControl1: TImageControl;
    Layout1: TLayout;
    TrackBar1: TTrackBar;
    HueAdjustEffect1: THueAdjustEffect;
    TrackBar2: TTrackBar;
    ContrastEffect1: TContrastEffect;
    TrackBar3: TTrackBar;
    Layout2: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
ContrastEffect1.Enabled := False;
HueAdjustEffect1.Enabled := True;
HueAdjustEffect1.Hue := TrackBar1.Value;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
HueAdjustEffect1.Enabled := False;
ContrastEffect1.Enabled := True;
ContrastEffect1.Contrast := TrackBar2.Value;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
HueAdjustEffect1.Enabled := False;
ContrastEffect1.Enabled := True;
ContrastEffect1.Brightness := TrackBar3.Value;
end;

end.
