unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, FMX.TMSGridCell,
  FMX.TMSCustomGrid, FMX.StdCtrls;

type
  TForm719 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InitGrid;
  end;

var
  Form719: TForm719;

implementation

{$R *.fmx}

procedure TForm719.FormCreate(Sender: TObject);
begin
  InitGrid;
end;

procedure TForm719.InitGrid;
begin
  TMSFMXGrid1.SelectionMode := smCellRange;
  TMSFMXGrid1.ColumnCount := 10;
  TMSFMXGrid1.Rowcount := 100;
  TMSFMXGrid1.Cells[4,4] := '1';
  TMSFMXGrid1.Cells[5,4] := '2';
  TMSFMXGrid1.Cells[4,5] := '3';
  TMSFMXGrid1.Cells[5,5] := '4';
end;

end.
