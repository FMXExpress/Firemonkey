unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, System.Rtti, FMX.Controls.Presentation, FMX.Edit,
  FMX.Layouts, FMX.Grid, FMX.StdCtrls, FMX.Memo, FMX.ListView,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.MultiView, Data.Bind.Controls,
  Fmx.Bind.Navigator;

type
  TForm1 = class(TForm)
    lvTables: TListView;
    loTables: TLayout;
    loSQL: TLayout;
    ToolBar1: TToolBar;
    btnRunSQL: TButton;
    loData: TLayout;
    Grid1: TGrid;
    loDB: TLayout;
    edtDatabaseName: TEdit;
    btConnect: TButton;
    FDQuery1: TFDQuery;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceBindSourceDB1: TLinkGridToDataSource;
    MultiView1: TMultiView;
    ToolBar2: TToolBar;
    Label1: TLabel;
    Memo1: TMemo;
    ToolBar3: TToolBar;
    Label2: TLabel;
    Button1: TButton;
    ToolBar4: TToolBar;
    Label3: TLabel;
    ToolBar5: TToolBar;
    NavigatorBindSourceDB1: TBindNavigator;
    procedure btConnectClick(Sender: TObject);
    procedure btnRunSQLClick(Sender: TObject);
    procedure lvTablesItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses InterBaseDM, System.IOUtils;

procedure TForm1.btConnectClick(Sender: TObject);
var
  DBPath: string;
  TableName: string;
  I: Integer;
  SkipAddTables: Boolean;
begin
  if edtDatabaseName.Text.Contains(':') then begin
    DBPath := edtDatabaseName.Text;
    // For the demo, we only add tables and data to a local database
    SkipAddTables := True;
  end else begin
    SkipAddTables := False;
    {$IFNDEF MSWINDOWS}
    DBPath := TPath.GetDocumentsPath+PathDelim+'interbase'+PathDelim+edtDatabaseName.Text;
    {$ELSE}
    DBPath := 'c:\data\'+edtDatabaseName.Text;
    if not DirectoryExists('c:\data\') then
      ForceDirectories('c:\data\');
    {$ENDIF}
  end;

  dmInterBase.IBLiteDB.Connected := False;
  dmInterBase.IBLiteDB.Params.Values['Database'] := DBPath;
  dmInterBase.IBLiteDB.Connected := True;

  if not SkipAddTables then begin
    // Create some local data for demo purposes
    if not dmInterBase.TableExists('Foo') then begin
      dmInterBase.IBLiteDB.ExecSQL('CREATE TABLE FOO (ID Integer, FOO VarChar(10))');
      dmInterBase.Tables.Add('FOO');

      FDQuery1.SQL.Text := 'INSERT INTO FOO (ID, FOO) values (:ID, :FOO)';
      FDQuery1.Params.ArraySize := 10;
      for I := 0 to 9 do begin
        FDQuery1.ParamByName('ID').AsIntegers[I] := I;
        FDQuery1.ParamByName('FOO').AsStrings[I] := 'Foo '+I.ToString;
      end;
      FDQuery1.Execute(10);
    end;

    if not dmInterBase.TableExists('Fee') then begin
      dmInterBase.IBLiteDB.ExecSQL('CREATE TABLE FEE (ID Integer, FEE VarChar(10))');
      dmInterBase.Tables.Add('FEE');

      FDQuery1.SQL.Text := 'INSERT INTO FEE (ID, FEE) values (:ID, :FEE)';
      FDQuery1.Params.ArraySize := 10;
      for I := 0 to 9 do begin
        FDQuery1.ParamByName('ID').AsIntegers[I] := I+100;
        FDQuery1.ParamByName('FEE').AsStrings[I] := 'Fee '+(I+100).ToString;
      end;
      FDQuery1.Execute(10);
    end;
  end;

  lvTables.ClearItems;
  for TableName in dmInterBase.Tables do
    lvTables.Items.Add.Text := TableName;
end;

procedure TForm1.btnRunSQLClick(Sender: TObject);
begin
  if Memo1.Text.Trim.StartsWith('select',True) then
    FDQuery1.Open(Memo1.Text)
  else begin
    FDQuery1.SQL.Text := Memo1.Text;
    FDQuery1.ExecSQL;
  end;
end;

procedure TForm1.lvTablesItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  Memo1.Text := 'Select * from '+AItem.Text;
  btnRunSQLClick(nil);
end;

end.
