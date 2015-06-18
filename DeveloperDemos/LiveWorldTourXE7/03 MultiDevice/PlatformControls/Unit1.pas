unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Calendar,
  FMX.Controls.Presentation, FMX.Edit, FMX.StdCtrls, FMX.DateTimeCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Calendar1: TCalendar;
    Switch1: TSwitch;
    Label1: TLabel;
    ToolBar1: TToolBar;
    Label2: TLabel;
    procedure Switch1Switch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.iPhone.fmx IOS}
{$R *.iPad.fmx IOS}

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  if Switch1.IsChecked then
   begin
    Calendar1.ControlType := TPresentedControl.TControlType.Platform;
    Edit1.ControlType := TPresentedControl.TControlType.Platform;
    Edit2.ControlType := TPresentedControl.TControlType.Platform;
   end
  else
   begin
    Calendar1.ControlType := TPresentedControl.TControlType.Styled;
    Edit1.ControlType := TPresentedControl.TControlType.Styled;
    Edit2.ControlType := TPresentedControl.TControlType.Styled;
   end;
end;

end.
