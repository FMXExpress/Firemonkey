unit UGridFilter;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSBaseControl, FMX.TMSGridCell, FMX.TMSGridOptions, FMX.TMSGridData,
  FMX.StdCtrls, FMX.Objects, FMX.ListBox, FMX.Edit, FMX.Layouts, FMX.TMSGridFilter,
  FMX.TMSGrid, FMX.TMSCustomGrid, IOUtils, FMX.TMSButton, UIConsts, FMX.Ani;

type
  TForm1 = class(TForm)
    TMSFMXGrid1: TTMSFMXGrid;
    TMSFMXGridFilterPanel1: TTMSFMXGridFilterPanel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.IOOffset := Point(1,1);
  TMSFMXGrid1.Cells[1,0] := 'Brand';
  TMSFMXGrid1.Cells[2,0] := 'Type';
  TMSFMXGrid1.Cells[3,0] := 'CC';
  TMSFMXGrid1.Cells[4,0] := 'Hp';
  TMSFMXGrid1.Cells[5,0] := 'Cyl';
  TMSFMXGrid1.Cells[6,0] := 'Kw';
  TMSFMXGrid1.Cells[7,0] := 'Price';
  TMSFMXGrid1.Cells[8,0] := 'Country';

  {$IFDEF MSWINDOWS}
  TMSFMXGrid1.LoadFromCSV('..\..\CARS.CSV');
  {$ELSE}
  {$IFDEF ANDROID}
  TMSFMXGrid1.LoadFromCSV(TPath.GetDocumentsPath + PathDelim + 'CARS.CSV');
  {$ELSE}
  TMSFMXGrid1.LoadFromCSV(ExtractFilePath(ParamStr(0)) +'CARS.CSV');
  {$ENDIF}
  {$ENDIF}
  TMSFMXGridFilterPanel1.Init;
end;

end.
