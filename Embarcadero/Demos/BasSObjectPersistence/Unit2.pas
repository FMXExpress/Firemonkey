unit Unit2;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.OpenSSL,
  REST.Backend.KinveyProvider, REST.Backend.ParseProvider;

{$REGION 'My Secret Keys'}
const
  KINVEY_App_Key = '---';
  KINVEY_App_Secret = '---';
  KINVEY_Master_Secret = '---';

  PARSE_App_Id = '---';
  PARSE_RESTApi_Key = '---';
  PARSE_Master_Key = '---';
{$ENDREGION}

type
  TDataModule2 = class(TDataModule)
    ParseProvider1: TParseProvider;
    KinveyProvider1: TKinveyProvider;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule2: TDataModule2;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule2.DataModuleCreate(Sender: TObject);
begin
  /// prepare the client-component for usage
  KinveyProvider1.AppKey := KINVEY_App_Key;
  KinveyProvider1.AppSecret := KINVEY_App_Secret;
  KinveyProvider1.MasterSecret := KINVEY_Master_Secret;

  ParseProvider1.ApplicationID := PARSE_App_Id;
  ParseProvider1.RestApiKey := PARSE_RESTApi_Key;
  ParseProvider1.MasterKey := PARSE_Master_Key;
end;

end.
