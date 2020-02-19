//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

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
