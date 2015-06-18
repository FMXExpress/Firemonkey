unit uDMEmailChecker;

interface

uses
  System.SysUtils, System.Classes, IPPeerClient, REST.Backend.ServiceTypes,
  System.JSON, REST.Backend.EMSServices, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Backend.EndPoint, REST.Backend.EMSProvider;

type
  TDMEmailChecker = class(TDataModule)
    EMSProvider1: TEMSProvider;
    BackendEndpoint1: TBackendEndpoint;
    RESTResponse1: TRESTResponse;
  private
    { Private declarations }
  public
    function IsValidEmail(s: string): boolean;
  end;

var
  DMEmailChecker: TDMEmailChecker;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TDMEmailChecker }

function TDMEmailChecker.IsValidEmail(s: string): boolean;
begin
  BackendEndpoint1.Params[0].Value := s;
  BackendEndpoint1.Execute;
  Result := RESTResponse1.Content = 'true';
end;

end.
