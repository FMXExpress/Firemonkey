unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData,
  FMX.TMSCustomGrid, FMX.TMSGrid, FMX.TMSXUtil;

type
  TForm27 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    Panel1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    NodeButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure NodeButtonClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form27: TForm27;

implementation

{$R *.fmx}

procedure TForm27.Button1Click(Sender: TObject);
begin
  TMSFMXGrid1.SortData(1,sdAscending);

  TMSFMXGrid1.Options.Grouping.MergeHeader := true;
  TMSFMXGrid1.Options.Grouping.Summary := true;

  TMSFMXGrid1.Group(1);

  TMSFMXGrid1.GroupSum(6);
  TMSFMXGrid1.GroupAvg(5);
  TMSFMXGrid1.Options.Sorting.Mode := gsmNormal;

  NodeButton.Visible := true;

  Button1.Visible := false;
  Button2.Visible := true;
end;

procedure TForm27.Button2Click(Sender: TObject);
begin
  TMSFMXGrid1.UnGroup;
  TMSFMXGrid1.Options.Sorting.Mode := gsmNone;
  TMSFMXGrid1.UpdateGridCells;
  NodeButton.Visible := false;

  Button1.Visible := true;
  Button2.Visible := false;
end;

procedure TForm27.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  XCopyFile(XGetRootDirectory+'CARS.CSV', XGetDocumentsDirectory+'/CARS.CSV');
  TMSFMXGrid1.LoadFromCSV(XGetDocumentsDirectory+ '/CARS.CSV');
  TMSFMXGrid1.SortData(1, sdAscending);
  TMSFMXGrid1.Cells[1,0] := 'Brand';
  TMSFMXGrid1.Cells[2,0] := 'Type';
  TMSFMXGrid1.Cells[3,0] := 'CC';
  TMSFMXGrid1.Cells[4,0] := 'Hp';
  TMSFMXGrid1.Cells[5,0] := 'Cyl';
  TMSFMXGrid1.Cells[6,0] := 'Kw';
  TMSFMXGrid1.Cells[7,0] := 'Price';
  TMSFMXGrid1.Cells[8,0] := 'Country';
end;

procedure TForm27.NodeButtonClick(Sender: TObject);
begin
  if NodeButton.Tag = 0 then
  begin
    TMSFMXGrid1.CloseAllNodes;
    NodeButton.Tag := 1;
    NodeButton.Text := 'Open all nodes';
  end
  else
  begin
    TMSFMXGrid1.OpenAllNodes;
    NodeButton.Tag := 0;
    NodeButton.Text := 'Close all nodes';
  end;
end;

end.
