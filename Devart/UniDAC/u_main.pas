unit u_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.DB, MemDS, DBAccess,
  Uni, FMX.Gestures, FMX.Layouts, FMX.Memo, FMX.Objects, FMX.StdCtrls, FMX.Edit,
  FMX.ListBox, SQLiteUniProvider, PostgreSQLUniProvider, OracleUniProvider,
  MySQLUniProvider, UniProvider, InterBaseUniProvider, VirtualTable, CRBatchMove,
  SQLServerUniProvider, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, 
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components, 
  Data.Bind.DBScope;

type

  DataBases = (dbSQLite, dbOracle, dbMySQL, dbPostgreSQL, dbInterBase, dbSQLServer);

  TMainForm = class(TForm)
    UniConnection: TUniConnection;
    UniQuery: TUniQuery;
    Expander: TExpander;
    lbServer: TLabel;
    lbPort: TLabel;
    lbDatabase: TLabel;
    lbUserName: TLabel;
    lbPassword: TLabel;
    edServer: TEdit;
    edDatabase: TEdit;
    edUserName: TEdit;
    edPassword: TEdit;
    btnConnect: TButton;
    pMain: TPanel;
    lbFishName: TLabel;
    iPicture: TImage;
    pInfo: TPanel;
    Label1: TLabel;
    lbSpeciesName: TLabel;
    Label2: TLabel;
    lbCategory: TLabel;
    meDescription: TMemo;
    GestureManager: TGestureManager;
    cbProvider: TComboBox;
    lbProvider: TLabel;
    nbPort: TNumberBox;
    InterBaseUniProvider: TInterBaseUniProvider;
    MySQLUniProvider: TMySQLUniProvider;
    OracleUniProvider: TOracleUniProvider;
    PostgreSQLUniProvider: TPostgreSQLUniProvider;
    SQLiteUniProvider: TSQLiteUniProvider;
    SQLServerUniProvider: TSQLServerUniProvider;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkPropertyToFieldBitmap: TLinkPropertyToField;
    LinkControlToField1: TLinkControlToField;
    LinkPropertyToFieldText: TLinkPropertyToField;
    LinkPropertyToFieldText2: TLinkPropertyToField;
    LinkPropertyToFieldText3: TLinkPropertyToField;  
    procedure btnConnectClick(Sender: TObject);
    procedure pMainGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure cbProviderChange(Sender: TObject);
  private
    CurrentDB: DataBases;
    procedure CreateFishTableIfNeeded;
  end;

var
  MainForm: TMainForm;

const

  CreateLiteTableScript = 'CREATE TABLE FISH('#13#10 +
                          'ID INTEGER NOT NULL PRIMARY KEY,'#13#10 +
                          'CATEGORY VARCHAR(15),'#13#10 +
                          'SPECIES_NAME VARCHAR(40),'#13#10 +
                          'COMMON_NAME VARCHAR(30),'#13#10 +
                          'NOTES TEXT,'#13#10 +
                          'GRAPHIC BLOB)';

  CreateOraTableScript = 'CREATE TABLE FISH('#13#10 +
                         'ID NUMBER(4) NOT NULL PRIMARY KEY,'#13#10 +
                         'CATEGORY VARCHAR2(15),'#13#10 +
                         'SPECIES_NAME VARCHAR2(40),'#13#10 +
                         'COMMON_NAME VARCHAR2(30),'#13#10 +
                         'NOTES CLOB,'#13#10 +
                         'GRAPHIC BLOB)';

  CreateMyTableScript =  'CREATE TABLE FISH('#13#10 +
                         'ID INTEGER NOT NULL PRIMARY KEY,'#13#10 +
                         'CATEGORY VARCHAR(15),'#13#10 +
                         'SPECIES_NAME VARCHAR(40),'#13#10 +
                         'COMMON_NAME VARCHAR(30),'#13#10 +
                         'NOTES TEXT,'#13#10 +
                         'GRAPHIC BLOB)';

  CreatePgTableScript =  'CREATE TABLE FISH('#13#10 +
                         'ID INTEGER NOT NULL PRIMARY KEY,'#13#10 +
                         'CATEGORY CHARACTER(15),'#13#10 +
                         'SPECIES_NAME CHARACTER(40),'#13#10 +
                         'COMMON_NAME CHARACTER(30),'#13#10 +
                         'NOTES TEXT,'#13#10 +
                         'GRAPHIC BYTEA)';

  CreateIBTableScript =  'CREATE TABLE FISH('#13#10 +
                         'ID INTEGER NOT NULL PRIMARY KEY,'#13#10 +
                         'CATEGORY VARCHAR(15),'#13#10 +
                         'SPECIES_NAME VARCHAR(40),'#13#10 +
                         'COMMON_NAME VARCHAR(30),'#13#10 +
                         'NOTES BLOB SUB_TYPE 1,'#13#10 +
                         'GRAPHIC BLOB SUB_TYPE 0)';

  CreateMSTableScript =  'CREATE TABLE FISH ('#13#10 +
                         'ID INT IDENTITY,'#13#10 +
                         'CATEGORY VARCHAR(15),'#13#10 +
                         'SPECIES_NAME VARCHAR(40),'#13#10 +
                         'COMMON_NAME VARCHAR(30),'#13#10 +
                         'NOTES TEXT,'#13#10 +
                         'GRAPHIC IMAGE,'#13#10 +
                         'CONSTRAINT PK_FISH PRIMARY KEY (ID))';

implementation

{$R *.fmx}

{$IFDEF IOS}{$IFNDEF CPUARM}
  {$DEFINE IOSSIMULATOR}
{$ENDIF}{$ENDIF}

procedure TMainForm.CreateFishTableIfNeeded;
var
  tmpUniQuery: TUniQuery;
  FileName, CreateTableScript: string;
  tmpVirtualTable: TVirtualTable;
  tmpBatchMove: TCRBatchMove;
begin
  tmpUniQuery := TUniQuery.Create(nil);
  try
    tmpUniQuery.Connection := UniConnection;
    case CurrentDB of
      dbSQLite: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM SQLITE_MASTER WHERE UPPER(TYPE) = ''TABLE'' AND UPPER(NAME) = ''FISH''';
        CreateTableScript := CreateLiteTableScript;
      end;
      dbOracle: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM ALL_TABLES WHERE UPPER(TABLE_NAME) = ''FISH'' AND UPPER(OWNER) = UPPER(''' + UniConnection.Username + ''')';
        CreateTableScript := CreateOraTableScript;
      end;
      dbMySQL: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE UPPER(TABLE_NAME) = ''FISH'' AND UPPER(TABLE_SCHEMA) = UPPER(''' + UniConnection.Database + ''')';
        CreateTableScript := CreateMyTableScript;
      end;
      dbPostgreSQL: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM PG_TABLES WHERE UPPER(TABLENAME) = ''FISH'' AND UPPER(SCHEMANAME) = ''PUBLIC''';
        CreateTableScript := CreatePgTableScript;
      end;
      dbInterBase: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM RDB$RELATIONS WHERE UPPER(RDB$RELATION_NAME) = ''FISH''';
        CreateTableScript := CreateIBTableScript;
      end;
      dbSQLServer: begin
        tmpUniQuery.SQL.Text := 'SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N''FISH'') AND OBJECTPROPERTY(id, N''IsUserTable'') = 1';
        CreateTableScript := CreateMSTableScript;
      end;
      else
        Assert(False, 'Unknown database.');
    end;
    tmpUniQuery.Open;
    if tmpUniQuery.IsEmpty then begin
      tmpUniQuery.Close;
      tmpUniQuery.SQL.Text := CreateTableScript;
      tmpUniQuery.Execute;
      FileName := {$IF Defined(IOS) or Defined(ANDROID)}GetHomePath + PathDelim + {$IFDEF IOS}'Documents' + PathDelim + {$ENDIF}{$ENDIF}'fish.vtd';
      tmpVirtualTable := TVirtualTable.Create(nil);
      try
        tmpVirtualTable.LoadFromFile(FileName);
        tmpBatchMove := TCRBatchMove.Create(nil);
        try
          tmpUniQuery.SQL.Text := 'SELECT * FROM FISH';
          tmpBatchMove.FieldMappingMode := mmFieldName;
          tmpBatchMove.Source := tmpVirtualTable;
          tmpBatchMove.Destination := tmpUniQuery;
          tmpBatchMove.Execute;
        finally
          tmpBatchMove.Free;
        end;
      finally
        tmpVirtualTable.Free;
      end;
    end;
  finally
    tmpUniQuery.Free;
  end;
end;

procedure TMainForm.cbProviderChange(Sender: TObject);
begin
  CurrentDB := DataBases(cbProvider.ItemIndex);
  lbServer.Enabled := CurrentDB <> dbSQLite;
  edServer.Enabled := CurrentDB <> dbSQLite;
  lbPort.Enabled := CurrentDB in [dbMySQL, dbPostgreSQL, dbSQLServer];
  nbPort.Enabled := CurrentDB in [dbMySQL, dbPostgreSQL, dbSQLServer];
  case CurrentDB of
    dbMySQL: nbPort.Value := 3306;
    dbPostgreSQL: nbPort.Value := 5432;
    dbSQLite: edDatabase.Text := 'fish.db3';
    dbInterBase : edDatabase.Text := 'fish.gdb';
    dbSQLServer: nbPort.Value := 1433;
  end;
  lbUserName.Enabled := CurrentDB <> dbSQLite;
  edUserName.Enabled := CurrentDB <> dbSQLite;
  lbDatabase.Enabled := CurrentDB <> dbOracle;
  edDatabase.Enabled := CurrentDB <> dbOracle;
end;

procedure TMainForm.pMainGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if UniQuery.Active then
    if (EventInfo.GestureID = sgiLeft) and not UniQuery.Eof then begin
      UniQuery.Next;
    end
    else
    if (EventInfo.GestureID = sgiRight) and not UniQuery.Bof then begin
      UniQuery.Prior;
    end;
end;

procedure TMainForm.btnConnectClick(Sender: TObject);
var
  CurrentDir: string;
begin
  if UniConnection.Connected then begin
    UniConnection.Disconnect;
    btnConnect.Text := 'Connect';
  end
  else begin
    UniConnection.ProviderName := cbProvider.Items[cbProvider.ItemIndex];
    UniConnection.Server := edServer.Text;
  {$IF Defined(IOS) or Defined(ANDROID)}
    if (CurrentDB = dbSQLite) or ((CurrentDB = dbInterBase) and (edServer.Text = '')) then
      CurrentDir := GetHomePath + PathDelim{$IFDEF IOS} + 'Documents' + PathDelim{$ENDIF}
    else
  {$ENDIF}
      CurrentDir := '';
    UniConnection.Database := CurrentDir + edDatabase.Text;
    UniConnection.Username := edUserName.Text;
    if CurrentDB <> dbInterbase  then
      UniConnection.Port := Trunc(nbPort.Value)
    else
      UniConnection.DataTypeMap.AddFieldNameRule('notes', ftString, 10000);
    if CurrentDB = dbOracle then
      UniConnection.SpecificOptions.Values['Direct'] := 'True';
  {$IFNDEF IOSSIMULATOR}
    if CurrentDB = dbSQLite then begin
      UniConnection.SpecificOptions.Values['Direct'] := 'True';
      UniConnection.SpecificOptions.Values['ForceCreateDatabase'] := 'True';
    end;
  {$ENDIF}
    if CurrentDB = dbSQLServer then
      UniConnection.SpecificOptions.Values['OLEDBProvider'] := 'prDirect';
    if CurrentDB = dbSQLite then
      UniConnection.SpecificOptions.Values['EncryptionKey'] := edPassword.Text
    else
      UniConnection.Password := edPassword.Text;
    try
      UniConnection.Connect;
      CreateFishTableIfNeeded;
      UniQuery.Open;
      Expander.IsExpanded := False;
      btnConnect.Text := 'Disconnect';
    except
      UniConnection.Disconnect;
      raise;
    end;
  end;
end;

end.
