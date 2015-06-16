unit UDropBoxDataStoreDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomDropBoxDataStore,
  FMX.TMSCloudDropBoxDataStore, System.Rtti, FMX.Objects, FMX.ListBox, FMX.Edit,
  FMX.Layouts, FMX.Grid, FMX.TMSCloudListView, IOUtils;

type
  TForm12 = class(TForm)
    Panel1: TPanel;
    btConnect: TButton;
    btRemove: TButton;
    GroupBox1: TPanel;
    GroupBox2: TPanel;
    btSample: TButton;
    lvdatastores: TTMSFMXCloudListView;
    btCreate: TButton;
    edDataStore: TEdit;
    btSetMetaData: TButton;
    edTitle: TEdit;
    btDeleteDataStore: TButton;
    btGetByName: TButton;
    dsName: TEdit;
    cbRow: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    edTableName: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edField1: TEdit;
    edValue1: TEdit;
    edField2: TEdit;
    edValue2: TEdit;
    edField3: TEdit;
    edValue3: TEdit;
    edField4: TEdit;
    edValue4: TEdit;
    cb1: TCheckBox;
    cb2: TCheckBox;
    cb3: TCheckBox;
    cb4: TCheckBox;
    btDeleteRow: TButton;
    btInsertRow: TButton;
    btUpdateRow: TButton;
    btDeleteFields: TButton;
    btClear: TButton;
    Line1: TLine;
    edId: TEdit;
    TMSFMXCloudDropBoxDataStore1: TTMSFMXCloudDropBoxDataStore;
    procedure TMSFMXCloudDropBoxDataStore1ReceivedAccessToken(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btSampleClick(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure btDeleteDataStoreClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btGetByNameClick(Sender: TObject);
    procedure cbRowChange(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btUpdateRowClick(Sender: TObject);
    procedure btDeleteRowClick(Sender: TObject);
    procedure btInsertRowClick(Sender: TObject);
    procedure btDeleteFieldsClick(Sender: TObject);
    procedure btSetMetaDataClick(Sender: TObject);
    procedure lvdatastoresChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Opened: boolean;
    procedure FillData(Index: integer);
    procedure FillCheckedValue(Value: string);
    procedure ToggleControls;
    procedure ShowDataStores;
    procedure ShowRows;
  end;

var
  Form12: TForm12;

implementation

{$R *.fmx}

// PLEASE USE A VALID INCLUDE FILE THAT CONTAINS THE APPLICATION KEY & SECRET
// FOR THE DROPBOX SERVICE
// STRUCTURE OF THIS .INC FILE SHOULD BE
//
// const
//  DropBoxAppkey = 'xxxxxxxxx';
//  DropBoxAppSecret = 'yyyyyyyy';

{$I APPIDS.INC}

procedure TForm12.btClearClick(Sender: TObject);
begin
  edValue1.Text := '';
  edValue2.Text := '';
  edValue3.Text := '';
  edValue4.Text := '';
end;

procedure TForm12.btConnectClick(Sender: TObject);
var
  acc: boolean;
begin
  TMSFMXCloudDropBoxDataStore1.App.Key := DropBoxAppKey;
  TMSFMXCloudDropBoxDataStore1.App.Secret := DropBoxAppSecret;

  TMSFMXCloudDropBoxDataStore1.LoadTokens;

  acc := TMSFMXCloudDropBoxDataStore1.TestTokens;

  if acc then
  begin
    opened := true;
    ToggleControls;
    ShowDataStores;
  end
  else
    TMSFMXCloudDropBoxDataStore1.DoAuth;
end;

procedure TForm12.btCreateClick(Sender: TObject);
var
  ds: TDataStore;
begin
  if edDataStore.Text <> '' then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.CreateDataStore(edDataStore.Text);

    if not Assigned(ds) then
      ShowMessage('Error creating datastore')
    else
      ShowDataStores;
  end
  else
    ShowMessage('Specify name for datastore');
end;

procedure TForm12.btDeleteDataStoreClick(Sender: TObject);
begin
  if lvDataStores.ItemIndex >= 0 then
  begin
    if TMSFMXCloudDropBoxDataStore1.DeleteDataStore(TMSFMXCloudDropBoxDataStore1.DataStoreList[lvDataStores.ItemIndex]) then
      ShowDataStores;
  end;
end;

procedure TForm12.btDeleteFieldsClick(Sender: TObject);
var
  Fields: TStringList;
  ds: TDataStore;
  dr: TDataStoreRow;
begin
  ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];
  dr := ds.Rows[cbRow.ItemIndex];

  Fields := TStringList.Create;
  try
    if cb1.IsChecked then
      Fields.Add(dr.Fields[0].FieldName);
    if cb2.IsChecked then
      Fields.Add(dr.Fields[1].FieldName);
    if cb3.IsChecked then
      Fields.Add(dr.Fields[2].FieldName);
    if cb4.IsChecked then
      Fields.Add(dr.Fields[3].FieldName);

    if not ds.DeleteFields(dr, Fields) then
      ShowMessage(TMSFMXCloudDropBoxDataStore1.LastError);
  finally
    Fields.Free;
  end;
end;

procedure TForm12.btDeleteRowClick(Sender: TObject);
var
 ds: TDataStore;
begin
  if lvDataStores.ItemIndex >= 0 then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];
    if not ds.DeleteRecord(ds.Rows[cbRow.ItemIndex]) then
      ShowMessage(TMSFMXCloudDropBoxDataStore1.LastError)
    else
      ShowRows;
  end;
end;

procedure TForm12.btGetByNameClick(Sender: TObject);
var
  ds: TDataStore;
begin
  if dsName.Text <> '' then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.GetDataStoreByName(dsName.Text);
    if Assigned(ds) then
    begin
      lvDataStores.ItemIndex :=  TMSFMXCloudDropBoxDataStore1.DataStoreList.IndexOf(ds.ID);
      ShowMessage('Datastore <'+dsName.Text + '> found'#13'ID='+ds.ID+#13'Rev='+IntToStr(ds.Revision));
    end
    else
      ShowMessage('Datastore not found');
  end
  else
    ShowMessage('Please specify a datastore name');
end;

procedure TForm12.btInsertRowClick(Sender: TObject);
var
  ds: TDataStore;
  dr: TDataStoreRow;
begin
  if edTableName.Text = '' then
  begin
    ShowMessage('Please specify a tablename');
    Exit;
  end;

  if (lvDataStores.ItemIndex >= 0) then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];
    dr := ds.Rows.Add;
    dr.ID := ds.GetNewID;
    dr.TableName := edTableName.Text;

    if (edField1.Text <> '') and (edValue1.Text <> '') then
    begin
      dr.AddData(edField1.Text, edValue1.Text);
    end;

    if (edField2.Text <> '') and (edValue2.Text <> '') then
    begin
      dr.AddData(edField2.Text, edValue2.Text);
    end;

    if (edField3.Text <> '') and (edValue3.Text <> '') then
    begin
      dr.AddData(edField3.Text, edValue3.Text);
    end;

    if (edField4.Text <> '') and (edValue4.Text <> '') then
    begin
      dr.AddData(edField4.Text, edValue4.Text);
    end;

    if not ds.InsertRecord(dr) then
    begin
      ShowMessage(TMSFMXCloudDropBoxDataStore1.LastError);
    end
    else
      ShowRows;
  end;
end;

procedure TForm12.btRemoveClick(Sender: TObject);
begin
  TMSFMXCloudDropBoxDataStore1.ClearTokens();
  Opened := False;
  ToggleControls;
end;

procedure TForm12.btSampleClick(Sender: TObject);
var
  ds: TDataStore;
  dr: TDataStoreRow;

begin
  if Assigned(TMSFMXCloudDropBoxDataStore1.DataStoreList.FindByName('demo')) then
  begin
    ShowMessage('Demo datastore already exists');
    Exit;
  end;

  ds := TMSFMXCloudDropBoxDataStore1.CreateDataStore('demo');

  dr := ds.Rows.Add;
  dr.ID := ds.GetNewID;
  dr.TableName := 'Capitals';

  dr.Fields.AddPair('Name','Paris');
  dr.Fields.AddPair('Country','France');
  dr.Fields.AddPair('Citizens',10000000);
  dr.Fields.AddPair('Europe',true);
  ds.InsertRecord(dr);

  dr := ds.Rows.Add;
  dr.ID := ds.GetNewID;
  dr.TableName := 'Capitals';

  dr.Fields.AddPair('Name','Brussels');
  dr.Fields.AddPair('Country','Belgium');
  dr.Fields.AddPair('Citizens',3000000);
  dr.Fields.AddPair('Europe',true);

  ds.InsertRecord(dr);

  dr := ds.Rows.Add;
  dr.ID := ds.GetNewID;
  dr.TableName := 'Capitals';

  dr.Fields.AddPair('Name','Berlin');
  dr.Fields.AddPair('Country','Germany');
  dr.Fields.AddPair('Citizens',6000000);
  dr.Fields.AddPair('Europe',true);

  ds.InsertRecord(dr);

  dr := ds.Rows.Add;
  dr.ID := ds.GetNewID;
  dr.TableName := 'Capitals';

  dr.Fields.AddPair('Name','London');
  dr.Fields.AddPair('Country','United Kingdom');
  dr.Fields.AddPair('Citizens',11000000);
  dr.Fields.AddPair('Europe',true);

  ds.InsertRecord(dr);

  dr := ds.Rows.Add;
  dr.ID := ds.GetNewID;
  dr.TableName := 'Capitals';

  dr.Fields.AddPair('Name','Washington');
  dr.Fields.AddPair('Country','USA');
  dr.Fields.AddPair('Citizens',8000000);
  dr.Fields.AddPair('Europe',false);

  ds.InsertRecord(dr);

  ShowDataStores;
end;

procedure TForm12.btSetMetaDataClick(Sender: TObject);
var
  ds: TDataStore;
begin
  if (lvDataStores.ItemIndex >= 0) then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvDataStores.ItemIndex];
    ds.SetMetaData(edTitle.Text, Now);
    ShowDataStores;
  end;
end;

procedure TForm12.btUpdateRowClick(Sender: TObject);
var
  ds: TDataStore;
  dr: TDataStoreRow;
begin
  if edTableName.Text = '' then
  begin
    ShowMessage('Please specify a tablename');
    Exit;
  end;

  if lvDataStores.ItemIndex >= 0 then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];

    dr := ds.Rows[cbRow.ItemIndex];
    dr.ID := edID.Text;
    dr.TableName := edTableName.Text;

    if (edField1.Text <> '') and (edValue1.Text <> '') then
    begin
      dr.AddData(edField1.Text, edValue1.Text);
    end;

    if (edField2.Text <> '') and (edValue2.Text <> '') then
    begin
      dr.AddData(edField2.Text, edValue2.Text);
    end;

    if (edField3.Text <> '') and (edValue3.Text <> '') then
    begin
      dr.AddData(edField3.Text, edValue3.Text);
    end;

    if (edField4.Text <> '') and (edValue4.Text <> '') then
    begin
      dr.AddData(edField4.Text, edValue4.Text);
    end;

    if not ds.UpdateRecord(ds.Rows[cbRow.ItemIndex]) then
      ShowMessage(TMSFMXCloudDropBoxDataStore1.LastError)
    else
      ShowRows;
  end;
end;

procedure TForm12.cbRowChange(Sender: TObject);
begin
  FillData(cbRow.ItemIndex);
end;

procedure TForm12.FillCheckedValue(Value: string);
begin
  if cb1.IsChecked then
    edValue1.Text := Value;
  if cb2.IsChecked then
    edValue2.Text := Value;
  if cb3.IsChecked then
    edValue2.Text := Value;
  if cb4.IsChecked then
    edValue2.Text := Value;
end;

procedure TForm12.FillData(Index: integer);
var
  ds: TDataStore;
  dr: TDataStoreRow;
begin
  if cbRow.ItemIndex >= 0 then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];
    dr := ds.Rows[Index];

    edTableName.Text := dr.TableName;
    edId.Text := dr.ID;

    edField1.Text := '';
    edField2.Text := '';
    edField3.Text := '';
    edField4.Text := '';

    edValue1.Text := '';
    edValue2.Text := '';
    edValue3.Text := '';
    edValue4.Text := '';

    if dr.Fields.Count > 0 then
    begin
        edField1.Text := dr.Fields[0].FieldName;
        edValue1.Text := dr.Fields[0].Value.ToString;
    end;

    if dr.Fields.Count > 1 then
    begin
        edField2.Text := dr.Fields[1].FieldName;
        edValue2.Text := dr.Fields[1].Value.ToString;
    end;

    if dr.Fields.Count > 2 then
    begin
        edField3.Text := dr.Fields[2].FieldName;
        edValue3.Text := dr.Fields[2].Value.ToString;
    end;

    if dr.Fields.Count > 3 then
    begin
        edField4.Text := dr.Fields[3].FieldName;
        edValue4.Text := dr.Fields[3].Value.ToString;
    end;
  end;
end;

procedure TForm12.FormCreate(Sender: TObject);
begin
  Opened := false;
  TMSFMXCloudDropBoxDataStore1.Logging := true;
  TMSFMXCloudDropBoxDataStore1.LogLevel := llDetail;
  TMSFMXCloudDropBoxDataStore1.PersistTokens.Key := TPath.GetDocumentsPath + '/dropboxdatastore.ini';

  ToggleControls;
end;

procedure TForm12.lvdatastoresChange(Sender: TObject);
begin
  cbRow.Items.Clear;
  edField1.Text := '';
  edField2.Text := '';
  edField3.Text := '';
  edField4.Text := '';
  edValue1.Text := '';
  edValue2.Text := '';
  edValue3.Text := '';
  edValue4.Text := '';
  edTableName.Text := '';
  edID.Text := '';

  if Assigned(lvdatastores.Selected) then
    ShowRows;
end;

procedure TForm12.ShowDataStores;
var
  i: integer;
  ds: TDataStore;
  lv: TListItem;

begin
  TMSFMXCloudDropBoxDataStore1.GetDataStores();

  lvDataStores.Items.Clear;

  for I := 0 to TMSFMXCloudDropBoxDataStore1.DataStoreList.Count - 1 do
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[i];

    lv := lvDataStores.Items.Add;
    lv.Text := ds.DataStoreName;
    lv.SubItems.Add(IntToStr(ds.Revision));
    lv.SubItems.Add(ds.Title);
    lv.SubItems.Add(DateTimeToStr(ds.DateTime));
  end;

  if lvDataStores.Items.Count > 0 then
    lvDataStores.ItemIndex := 0;
end;

procedure TForm12.ShowRows;
var
  ds: TDataStore;
  i: integer;
begin
  if lvDataStores.ItemIndex >= 0 then
  begin
    ds := TMSFMXCloudDropBoxDataStore1.DataStoreList[lvdatastores.ItemIndex];
    ds.GetRecords;

    cbRow.Items.Clear;
    for i := 0 to ds.Rows.Count - 1 do
      cbRow.Items.Add(IntToStr(i + 1));

    if (cbRow.Items.Count > 0) then
    begin
      cbrow.ItemIndex := 0;
      FillData(cbRow.ItemIndex);
    end;
  end;
end;

procedure TForm12.TMSFMXCloudDropBoxDataStore1ReceivedAccessToken(
  Sender: TObject);
begin
  opened := true;
  ToggleControls();
  TMSFMXCloudDropBoxDataStore1.SaveTokens;
  ShowDataStores;
end;

procedure TForm12.ToggleControls;
begin
  btConnect.Enabled := not opened;
  btRemove.Enabled := opened;
  btCreate.Enabled := opened;
  btInsertRow.Enabled := opened;
  btDeleteRow.Enabled := opened;
  btUpdateRow.Enabled := opened;
  btSample.Enabled := opened;
  btDeleteDataStore.Enabled := opened;
  btGetByName.Enabled := opened;
  btSetMetaData.Enabled := opened;
  btDeleteFields.Enabled := opened;
  btClear.Enabled := opened;
  lvDataStores.Enabled := opened;
  cbRow.Enabled := opened;
  edField1.Enabled := opened;
  edField2.Enabled := opened;
  edField3.Enabled := opened;
  edField4.Enabled := opened;
  edValue1.Enabled := opened;
  edValue2.Enabled := opened;
  edValue3.Enabled := opened;
  edValue4.Enabled := opened;
  edTableName.Enabled := opened;
  edID.Enabled := opened;
end;

end.
