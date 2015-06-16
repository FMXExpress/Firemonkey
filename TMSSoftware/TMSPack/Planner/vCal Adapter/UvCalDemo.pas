unit UvCalDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSPlanner, FMX.TMSPlannervCalAdapter, FMX.TMSBaseControl,
  FMX.TMSPlannerBase, FMX.TMSPlannerData;

type
  TForm1 = class(TForm)
    TMSFMXPlanner1: TTMSFMXPlanner;
    TMSFMXPlannervCalAdapter1: TTMSFMXPlannervCalAdapter;
    Label1: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TMSFMXPlannervCalAdapter1.Save;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  TMSFMXPlannervCalAdapter1.Active := true;
end;

end.
