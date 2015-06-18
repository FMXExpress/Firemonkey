unit UntMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, System.Rtti, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Layouts, FMX.Grid, FMX.ListView, FMX.StdCtrls, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Bindings.Outputs, Fmx.Bind.Editors, Fmx.Bind.Grid,
  FireDAC.Stan.StorageJSON, FireDAC.Stan.StorageBin, Data.Bind.Grid,
  Data.Bind.Components, Data.Bind.DBScope,

  Data.FireDACJSONReflect, ClientModuleUnit1;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListView1: TListView;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    fdmemDepartamentos: TFDMemTable;
    fdmemDepartamento: TFDMemTable;
    fdmemDepartamentosDEPARTMENT: TStringField;
    BindDepartamentos: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkFillControlToField1: TLinkFillControlToField;
    FDStanStorageBinLink1: TFDStanStorageBinLink;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    fdmemFuncionario: TFDMemTable;
    fdmemDepartamentosDEPT_NO: TStringField;
    BindSourceDB1: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    BindSourceDB2: TBindSourceDB;
    LinkGridToDataSourceBindSourceDB2: TLinkGridToDataSource;
    procedure ListView1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    procedure GetDepartmentEmployees(const ADEPTO: string);
    procedure GetDepartmentNames;
    procedure UpdateDepartmentNames(const ADataSetList: TFDJSONDataSets);
    procedure UpdateDepartmentEmployees(ADataSetList: TFDJSONDataSets);
    procedure ApplyUpdates;
    function  GetDeltas: TFDJSONDeltas;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

const
  sEmployees  = 'Employees';
  sDepartment = 'Department';

procedure TForm2.ApplyUpdates;
var
  LDeltaList : TFDJSONDeltas;
begin
  LDeltaList := GetDeltas;
  ClientModule.ServerMethods1Client.ApplyChangtesDepartmentEmployess(LDeltaList);
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  GetDepartmentNames;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  ApplyUpdates;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  LDEPTNO: string;
begin
  if fdmemDepartamento.Active then
  begin
    LDEPTNO := fdmemDepartamento.FieldByName('DEPT_NO').AsString;
    GetDepartmentEmployees(LDEPTNO);
  end;
end;

function TForm2.GetDeltas: TFDJSONDeltas;
begin
  if fdmemDepartamento.State in dsEditModes then
    fdmemDepartamento.Post;


  if fdmemFuncionario.State in dsEditModes then
    fdmemFuncionario.Post;

  Result := TFDJSONDeltas.Create;

  TFDJSONDeltasWriter.ListAdd(Result, sEmployees, fdmemFuncionario);
  TFDJSONDeltasWriter.ListAdd(Result, sDepartment, fdmemDepartamento);
end;

procedure TForm2.GetDepartmentEmployees(const ADEPTO: string);
var
  LDataSetList : TFDJSONDataSets;
begin
  LDataSetList := ClientModule.ServerMethods1Client.GetDepartmentEmployees(ADEPTO);
  UpdateDepartmentEmployees(LDataSetList);
end;

procedure TForm2.GetDepartmentNames;
var
  LDataSetList : TFDJSONDataSets;
begin
  LDataSetList := ClientModule.ServerMethods1Client.GetDepartmentNames();
  UpdateDepartmentNames(LDataSetList);
end;

procedure TForm2.ListView1Change(Sender: TObject);
var
  LDEPTONO : string;
begin
  LDEPTONO := ListView1.Selected.Detail;
  GetDepartmentEmployees(LDEPTONO);
end;

procedure TForm2.UpdateDepartmentEmployees(ADataSetList: TFDJSONDataSets);
var
  LDataSet : TFDDataSet;
begin
  LDataSet := TFDJSONDataSetsReader.GetListValueByName(ADataSetList, sDepartment);
  fdmemDepartamento.Active := False;
  fdmemDepartamento.AppendData(LDataSet);

  LDataSet := TFDJSONDataSetsReader.GetListValueByName(ADataSetList, sEmployees);
  fdmemFuncionario.Active := False;
  fdmemFuncionario.AppendData(LDataSet);
end;

procedure TForm2.UpdateDepartmentNames(const ADataSetList: TFDJSONDataSets);
begin
  fdmemDepartamentos.Active := False;
  Assert(TFDJSONDataSetsReader.GetListCount(ADataSetList) = 1);
  fdmemDepartamentos.AppendData(TFDJSONDataSetsReader.GetListValue(ADataSetList, 0));
end;

end.
