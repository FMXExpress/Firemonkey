unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.Edit, FMX.StdCtrls,
  FMX.TMSGridCell, FMX.TMSCustomGrid;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Edit1ChangeTracking(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.Button1Click(Sender: TObject);
var
  fltr : TFilterData;

begin
  TMSFMXGrid1.Filter.Clear;
  fltr := TMSFMXGrid1.Filter.Add;

  fltr.Column := 1;
  fltr.Condition := 'A*';

  Edit1.Text := 'A';

  TMSFMXGrid1.ApplyFilter;
end;

procedure TForm719.Edit1ChangeTracking(Sender: TObject);
var
  fltr : TFilterData;
begin
  TMSFMXGrid1.UnHideRowsAll;

  TMSFMXGrid1.Filter.Clear;
  fltr := TMSFMXGrid1.Filter.Add;

  fltr.Column := 1;
  fltr.CaseSensitive := false;
  fltr.Condition := edit1.Text + '*';

  TMSFMXGrid1.ApplyFilter;
end;

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  TMSFMXGrid1.LoadFromCSV('..\..\cars.csv');
  TMSFMXGrid1.ColumnWidths[1] := 100;

  Edit1.SetFocus;
end;

end.
