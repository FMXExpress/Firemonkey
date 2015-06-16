unit dataData;

interface

uses
  {$IFDEF Windows}
   MidasLib,
  {$ENDIF}
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient;

type
  TdtmdlData = class(TDataModule)
    cdsIconData: TClientDataSet;
    dsIconData: TDataSource;
    cdsIconDataID: TIntegerField;
    cdsIconDataCategory: TStringField;
    cdsIconDataDescription: TStringField;
    cdsIconDataSearchTerms: TMemoField;
    cdsIconDataIcon: TBlobField;
    procedure DataModuleCreate(Sender: TObject);
    procedure cdsIconDataAfterPost(DataSet: TDataSet);
  private
    { Private declarations }
    function FileName : string;
  public
    { Public declarations }
  end;

var
  dtmdlData: TdtmdlData;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmdlData.cdsIconDataAfterPost(DataSet: TDataSet);
begin
  cdsIconData.SaveToFile();
end;

procedure TdtmdlData.DataModuleCreate(Sender: TObject);
begin
  cdsIconData.FileName := FileName;

  if not FileExists(FileName) then
    cdsIconData.CreateDataSet
  else
    cdsIconData.Active := True;
end;

function TdtmdlData.FileName: string;
begin
 Result := ExtractFilePath(ParamStr(0))+'iconData.cds';
end;

end.
