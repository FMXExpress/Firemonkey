unit Windows.Services;

interface

uses
  Xplat.Services,
  FMX.Platform.Win, System.UITypes, FMX.Types;

type
  TWinPleaseWait = class(TInterfacedObject, IPleaseWaitService)
  private
    FCurrent: TCursor;
    FService: IFMXCursorService;
  private
    function GetService: IFMXCursorService;
    property Service: IFMXCursorService read GetService;
  public
    procedure StartWait;
    procedure StopWait;
  end;

implementation

uses
  FMX.Forms, FMX.Platform, Xplat.Inifiles;

{ TMacPleaseWait }

function TWinPleaseWait.GetService: IFMXCursorService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService, IInterface(FService)) then
    Result := FService
  else
    Result := nil;
end;

procedure TWinPleaseWait.StartWait;
begin
  if Assigned(Service) then
  begin
    FCurrent := Service.GetCursor;
    Service.SetCursor(crHourglass);
    Application.ProcessMessages;
  end;
end;

procedure TWinPleaseWait.StopWait;
begin
  if Assigned(Service) then
  begin
    Service.SetCursor(FCurrent);
    Application.ProcessMessages;
  end;
end;

initialization
  TPlatformServices.Current.AddPlatformService(IPleaseWaitService, TWinPleaseWait.Create);
  TPlatformServices.Current.AddPlatformService(IIniFileService, TXplatIniFile.Create);

finalization
  TPlatformServices.Current.RemovePlatformService(IPleaseWaitService);
  TPlatformServices.Current.RemovePlatformService(IIniFileService);

end.
