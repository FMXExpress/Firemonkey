unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSBaseControl,
  FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData, FMX.TMSCustomGrid,
  FMX.TMSGrid, FMX.StdCtrls, FMX.TMSXUtil;

type
  TForm25 = class(TForm)
    ToolBar1: TToolBar;
    Button1: TButton;
    Button2: TButton;
    TMSFMXGrid1: TTMSFMXGrid;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.fmx}

procedure TForm25.Button1Click(Sender: TObject);
begin
  TMSFMXGrid1.MergeSelection(TMSFMXGrid1.Selection);
end;

procedure TForm25.Button2Click(Sender: TObject);
begin
  TMSFMXGrid1.SplitCell(TMSFMXGrid1.FocusedCell.Col, TMSFMXGrid1.FocusedCell.Row);
end;

procedure TForm25.FormCreate(Sender: TObject);
begin
  //XCopyFile(XGetRootDirectory+'CARS.CSV', XGetDocumentsDirectory+'/CARS.CSV');
  TMSFMXGrid1.LoadFromCSV(GetHomePath + '/CARS.CSV');
  TMSFMXGrid1.SelectionMode := smCellRange;
  TMSFMXGrid1.DefaultColumnWidth := 150;
  TMSFMXGrid1.DefaultRowHeight := 40;
end;

end.
