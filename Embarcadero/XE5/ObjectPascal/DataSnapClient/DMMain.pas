unit DMMain;

interface

uses
  System.SysUtils, System.Classes, Data.DBXDataSnap, IndyPeerImpl,
  Data.DBXCommon, Data.DB, Data.SqlExpr, IPPeerClient;

type
  TDM = class(TDataModule)
    DataSnapConn: TSQLConnection;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Connect( Server, Port, User, Password : String );
  end;

var
  DM: TDM;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TDM }

procedure TDM.Connect(Server, Port, User, Password: String);
begin
  DataSnapConn.Params.Values['HostName'] := Server;
  DataSnapConn.Params.Values['Port'] := Port;
  DataSnapConn.Params.Values['DSAuthenticationUser'] := User;
  DataSnapConn.Params.Values['DSAuthenticationPassword'] := Password;

  DataSnapConn.Open;


end;

end.
