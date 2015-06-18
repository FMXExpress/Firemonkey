unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.IB, FireDAC.Phys.IBDef, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.BatchMove.Text,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.DataSet, FMX.StdCtrls, FireDAC.FMXUI.Wait,
  FireDAC.Comp.UI;

type
  TMainForm = class(TForm)
    FDBatchMove1: TFDBatchMove;
    FDBatchMoveDataSetReader1: TFDBatchMoveDataSetReader;
    MASTSQLConnection: TFDConnection;
    FDQOrders: TFDQuery;
    FDBatchMoveTextWriter1: TFDBatchMoveTextWriter;
    Button1: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  FDBatchMove1.Execute;
  ShowMessage('Done.');
end;

end.
