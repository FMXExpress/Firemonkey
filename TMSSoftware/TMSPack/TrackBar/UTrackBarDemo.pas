unit UTrackBarDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSTrackBar, FMX.Edit;

type
  TForm1 = class(TForm)
    TMSFMXTrackBar1: TTMSFMXTrackBar;
    TMSFMXTrackBar2: TTMSFMXTrackBar;
    cbtick: TCheckBox;
    cbfmt: TCheckBox;
    cbShowTick: TCheckBox;
    SpinBox1: TSpinBox;
    Label1: TLabel;
    Label2: TLabel;
    SpinBox2: TSpinBox;
    Label3: TLabel;
    lblVal: TLabel;
    Button1: TButton;
    procedure cbfmtChange(Sender: TObject);
    procedure cbtickChange(Sender: TObject);
    procedure cbShowTickChange(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure SpinBox2Change(Sender: TObject);
    procedure TMSFMXTrackBar1ValueChanging(Sender: TObject; Value: Single);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.cbfmtChange(Sender: TObject);
begin
  if cbfmt.IsChecked then
    TMSFMXTrackBar1.ValueFormat := '%g'
  else
    TMSFMXTrackBar1.ValueFormat := '%g db'
end;

procedure TForm1.cbShowTickChange(Sender: TObject);
begin
  TMSFMXTrackBar1.ShowTickMarks := cbShowTick.IsChecked;
end;

procedure TForm1.cbtickChange(Sender: TObject);
begin
  TMSFMXTrackBar1.SnapToTickMarks := cbtick.IsChecked;
end;

procedure TForm1.SpinBox1Change(Sender: TObject);
begin
  TMSFMXTrackBar1.Minimum := SpinBox1.Value;
  SpinBox2.Min := spinbox1.Value;
end;

procedure TForm1.SpinBox2Change(Sender: TObject);
begin
  TMSFMXTrackBar1.Maximum := SpinBox2.Value;
  SpinBox1.Max := spinbox2.Value;
end;

procedure TForm1.TMSFMXTrackBar1ValueChanging(Sender: TObject; Value: Single);
begin
  lblVal.Text := Format('%.2f',[TMSFMXTrackBar1.Value]);

  TMSFMXTrackBar2.Value := TMSFMXTrackBar1.Value;
end;

end.
