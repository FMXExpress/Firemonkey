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
    Button1: TButton;
    StyleBook1: TStyleBook;
    Button2: TButton;
    StyleBook2: TStyleBook;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  TMSFMXGrid1.StyleLookup := 'TMSFMXGrid1Style1';
end;

procedure TForm719.Button2Click(Sender: TObject);
begin
  TMSFMXGrid1.StyleLookup := 'TMSFMXGrid1Style2';
end;

procedure TForm719.FormCreate(Sender: TObject);
begin
  TMSFMXGrid1.LoadFromCSV('..\..\cars.csv');
  TMSFMXGrid1.DefaultColumnWidth := 100;
  TMSFMXGrid1.DefaultRowHeight := 30;
  TMSFMXGrid1.SelectionMode := smCellRange;
  TMSFMXGrid1.StyleLookup := '';
end;

end.
