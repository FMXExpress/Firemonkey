unit dmMain;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  FireDAC.Phys.IBBase, FireDAC.Phys.IB, FireDAC.Phys.IBDef;

type
  TdtmdlMain = class(TDataModule)
    FDConnection: TFDConnection;
    FDPhysIBDriverLink1: TFDPhysIBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    qryBooks: TFDQuery;
    qryBooksBOOK_ID: TIntegerField;
    qryBooksTITLE: TStringField;
    qryBooksNOTE: TStringField;
    qryBooksTESTIMENT: TStringField;
    qryChapters: TFDQuery;
    qryChaptersCHAPTER_ID: TIntegerField;
    qryChaptersBOOK_ID: TIntegerField;
    qryChaptersCHAPTER_NUMBER: TIntegerField;
    qryChaptersTITLE: TStringField;
    dsBooks: TDataSource;
    qryChaptersBOOK_TITLE: TStringField;
    qryVerses: TFDQuery;
    dsChapter: TDataSource;
    qryVersesVERSE_NUMBER: TIntegerField;
    qryVersesVERSE_TEXT: TStringField;
    qryBooksSHORT_NAME: TStringField;
    procedure FDConnectionBeforeConnect(Sender: TObject);
    procedure qryChaptersAfterScroll(DataSet: TDataSet);
  private
    FAfterChapterChange: TNotifyEvent;
    { Private declarations }
  public
    { Public declarations }
    procedure OpenConnections;
    property OnAfterChapterChange : TNotifyEvent read FAfterChapterChange write FAfterChapterChange;
  end;

const
  // Update const DatabaseFileName with your database file name
  DatabaseFileName = 'BIBLE.IB';

var
  dtmdlMain: TdtmdlMain;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}
uses IOUtils;

procedure TdtmdlMain.FDConnectionBeforeConnect(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}
  // Here to make it easier to debug to my standard local DB store on WIndows.
  FDConnection.Params.Values['Database'] := TPath.GetDocumentsPath+PathDelim+'interbase'+PathDelim+DatabaseFileName;
  {$ENDIF}
  // FDConnection.Params.Values['SEPassword'] := 'secretpassword';
end;

procedure TdtmdlMain.OpenConnections;
begin
  qryBooks.Open;
  qryChapters.Open;
  qryVerses.Open;
end;

procedure TdtmdlMain.qryChaptersAfterScroll(DataSet: TDataSet);
begin
  if Assigned(FAfterChapterChange) then
    FAfterChapterChange(Self);
end;

end.
