unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSGrid, UIConsts,
  FMX.TMSCustomGrid, FMX.ExtCtrls, FMX.StdCtrls;

type
  TForm4 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    subgrid: TTMSFMXGrid;
    procedure subgridstyled(sender: tobject);
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  TMSFMXGrid1.CellControls[1,2] := nil;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.BeginUpdate;
  TMSFMXGrid1.AddNode(1,1);
  TMSFMXGrid1.MergeCells(1,2,4,1);
  TMSFMXGrid1.RowHeights[2] := 128;
  TMSFMXGrid1.RowCount := 100;
  TMSFMXGrid1.ColumnCount := 10;
  TMSFMXGrid1.LinearFill;
  TMSFMXGrid1.EndUpdate;

  subgrid := TTMSFMXGrid.Create(Self);
  subgrid.Parent := nil;
  subgrid.Cells[1,1] := 'subgrid';
  subgrid.StyleLookup := 'TMSFMXGrid1Style1';
  subgrid.OnApplyStyleLookup := subgridstyled;
  {$if compilerversion > 25}
  subgrid.NeedStyleLookup;
  subgrid.ApplyStyleLookup;
  {$else}
  subgrid.UpdateStyle;
  {$ifend}
  subgrid.Visible := false;

  TMSFMXGrid1.CellControls[1,2] := subgrid;
end;

procedure TForm4.subgridstyled(sender: tobject);
begin
  subgrid.GetDefaultNormalLayout.Layout.Fill.Color := claYellow;
  subgrid.UpdateGridCells;
end;

end.


