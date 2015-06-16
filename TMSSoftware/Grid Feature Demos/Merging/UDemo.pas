unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
begin
  TMSFMXGrid1.MergeSelection(TMSFMXGrid1.Selection);
end;

procedure TForm719.Button2Click(Sender: TObject);
begin
  TMSFMXGrid1.SplitCell(TMSFMXGrid1.FocusedCell.Col, TMSFMXGrid1.FocusedCell.Row);
end;

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.LoadFromCSV('..\..\cars.csv');
  TMSFMXGrid1.SelectionMode := smCellRange;
  TMSFMXGrid1.DefaultColumnWidth := 100;
  TMSFMXGrid1.DefaultRowHeight := 30;
end;

end.
