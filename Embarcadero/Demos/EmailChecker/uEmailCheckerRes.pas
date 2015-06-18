unit uEmailCheckerRes;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes;

type
  [ResourceName('EmailChecker')]
  TEmailCheckerResource1 = class(TDataModule)
  published
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('{item}')]
    procedure GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

uses RegExpressionsUtil;

{$R *.dfm}

procedure TEmailCheckerResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  // Sample code
  AResponse.Body.SetValue(TJSONString.Create('EmailChecker'), True)
end;

procedure TEmailCheckerResource1.GetItem(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LItem: string;
begin
  LItem := ARequest.Params.Values['item'];

  if TRegularExpressionEngine.IsValidEmail(LItem) then
    AResponse.Body.SetValue(TJSONTrue.Create, True)
  else
    AResponse.Body.SetValue(TJSONFalse.Create, True);
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TEmailCheckerResource1));
end;

initialization
  Register;
end.


