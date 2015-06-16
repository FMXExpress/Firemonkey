unit UiCloudDocument;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, rtti, Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSNativeMaciCloud, FMX.Layouts, FMX.ListBox, FMX.Objects,
  FMX.ListView.Types, FMX.Memo, FMX.ListView, IOUtils, FMX.TMSNativeNSCore;

type
  TForm1144 = class(TForm)
    TMSFMXNativeMaciCloudDocument1: TTMSFMXNativeMaciCloudDocument;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    ListView1: TListView;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    Panel4: TPanel;
    Panel5: TPanel;
    Memo1: TMemo;
    Button3: TButton;
    Image1: TImage;
    procedure TMSFMXNativeMaciCloudDocument1Initialized(Sender: TObject;
      ASuccess: Boolean);
    procedure TMSFMXNativeMaciCloudDocument1DocumentsLoaded(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TMSFMXNativeMaciCloudDocument1DocumentDeleted(Sender: TObject;
      AItem: TTMSFMXNativeMaciCloudDocumentItem; ASuccess: Boolean;
      AError: string);
    procedure TMSFMXNativeMaciCloudDocument1DocumentAdded(Sender: TObject;
      AItem: TTMSFMXNativeMaciCloudDocumentItem; ASuccess: Boolean;
      AError: string);
    procedure Button3Click(Sender: TObject);
    procedure TMSFMXNativeMaciCloudDocument1DocumentsRefreshed(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure TMSFMXNativeMaciCloudDocument1DocumentDataChanged(Sender: TObject;
      AItem: TTMSFMXNativeMaciCloudDocumentItem);
  private
    { Private declarations }
    ShowContent: Boolean;
  public
    { Public declarations }
    procedure LoadNotes;
    procedure UpdateActiveNote;
    function CreateNewNote: string;
    function GetNoteCount: Integer;
    procedure AddNoteToList(ANote: TTMSFMXNativeMaciCloudDocumentItem);
    procedure DeleteNoteFromList(ANote: TTMSFMXNativeMaciCloudDocumentItem);
  end;

var
  Form1144: TForm1144;

implementation

{$R *.fmx}

procedure TForm1144.AddNoteToList(ANote: TTMSFMXNativeMaciCloudDocumentItem);
var
  lv: TListViewItem;
begin
  if not Assigned(ANote) then
    Exit;

  lv := ListView1.Items.Add;
  lv.Text := ANote.FileSystemName;
  lv.Bitmap.Assign(Image1.Bitmap);
  UpdateActiveNote;
end;

procedure TForm1144.Button1Click(Sender: TObject);
var
  fn: String;
begin
  fn := CreateNewNote;
  if TFile.Exists(fn) then
  begin
    TMSFMXNativeMaciCloudDocument1.AddDocument(fn);
    ShowContent := True;
  end;
end;

procedure TForm1144.Button2Click(Sender: TObject);
begin
  if Assigned(ListView1.Selected) then
    TMSFMXNativeMaciCloudDocument1.DeleteDocument(ListView1.Selected.Text);
end;

procedure TForm1144.Button3Click(Sender: TObject);
var
  ANote: TTMSFMXNativeMaciCloudDocumentItem;
  dt: TMemoryStream;
begin
  if Assigned(ListView1.Selected) then
  begin
    ANote := TMSFMXNativeMaciCloudDocument1.DocumentByName[ListView1.Selected.Text];
    try
      dt := TMemoryStream.Create;
      Memo1.Lines.SaveToStream(dt);
      TMSFMXNativeMaciCloudDocument1.UpdateDocument(ANote, dt);
      dt.Free;
    finally

    end;
  end;
end;

function TForm1144.CreateNewNote: string;
var
  str: TStringList;
  fn: String;
begin
  Result := '';
  str := TStringList.Create;
  try
    str.Add('Hello World ' + FormatDateTime('dd/mm/yyyy hh:nn:ss:zzz', Now));
    fn := TPath.GetDocumentsPath + '/'+'Note ' + inttostr(GetNoteCount) + '.txt';
    str.SaveToFile(fn);
    Result := fn;
  finally
    str.Free;
  end;
end;

procedure TForm1144.DeleteNoteFromList(ANote: TTMSFMXNativeMaciCloudDocumentItem);
var
  I: Integer;
begin
  if not Assigned(ANote) then
    Exit;

  for I := ListView1.Items.Count - 1 downto 0 do
  begin
    if ListView1.Items[I].Text = ANote.FileSystemName then
    begin
      ListView1.Items.Delete(I);
      Break;
    end;
  end;

  ShowContent := True;
  UpdateActiveNote;
end;

function TForm1144.GetNoteCount: Integer;
var
  I: Integer;
  valid: Boolean;
begin
  Result := 1;
  valid := False;
  while not valid do
  begin
    valid := True;
    for I := 0 to TMSFMXNativeMaciCloudDocument1.DocumentCount - 1 do
    begin
      if TMSFMXNativeMaciCloudDocument1.DocumentByIndex[I].FileSystemName.Contains(inttostr(Result)) then
      begin
        valid := False;
        Break;
      end;
    end;

    if not valid then
      Inc(Result);
  end;
end;

procedure TForm1144.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  ShowContent := True;
  UpdateActiveNote;
end;

procedure TForm1144.LoadNotes;
var
  lv: TListViewItem;
  I: Integer;
  ANote: TTMSFMXNativeMaciCloudDocumentItem;
begin
  ListView1.ClearItems;
  ListView1.BeginUpdate;
  for I := 0 to TMSFMXNativeMaciCloudDocument1.DocumentCount - 1 do
  begin
    ANote := TMSFMXNativeMaciCloudDocument1.DocumentByIndex[I];
    lv := ListView1.Items.Add;
    lv.Text := ANote.FileSystemName;
    lv.Bitmap.Assign(Image1.Bitmap);
  end;
  ListView1.EndUpdate;
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1DocumentAdded(Sender: TObject;
  AItem: TTMSFMXNativeMaciCloudDocumentItem; ASuccess: Boolean; AError: string);
begin
  if ASuccess then
    AddNoteToList(AItem)
  else
    ShowMessage(AError);
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1DocumentDataChanged(
  Sender: TObject; AItem: TTMSFMXNativeMaciCloudDocumentItem);
begin
  ShowContent := True;
  UpdateActiveNote;
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1DocumentDeleted(Sender: TObject;
  AItem: TTMSFMXNativeMaciCloudDocumentItem; ASuccess: Boolean; AError: string);
begin
  if ASuccess then
    DeleteNoteFromList(AItem)
  else
    ShowMessage(AError);
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1DocumentsLoaded(Sender: TObject);
begin
  LoadNotes;
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  Memo1.Enabled := True;
  ListView1.Enabled := True;
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1DocumentsRefreshed(
  Sender: TObject);
begin
  LoadNotes;
end;

procedure TForm1144.TMSFMXNativeMaciCloudDocument1Initialized(Sender: TObject;
  ASuccess: Boolean);
begin
  if ASuccess then
    TMSFMXNativeMaciCloudDocument1.LoadDocuments
  else
    ShowMessage('iCloud is not available');
end;

procedure TForm1144.UpdateActiveNote;
var
  doc: TTMSFMXNativeMaciCloudDocumentItem;
  str: TStringList;
begin
  if not ShowContent then
    Exit;

  ShowContent := False;
  Memo1.Text := '';
  if Assigned(ListView1.Selected) then
  begin
    doc := TMSFMXNativeMaciCloudDocument1.DocumentByName[ListView1.Selected.Text];
    if Assigned(doc) then
    begin
      doc.Data.Position := 0;
      str := TStringList.Create;
      str.LoadFromStream(doc.Data);
      Memo1.Text := str.Text;
      str.Free;
    end;
  end;
end;

end.


