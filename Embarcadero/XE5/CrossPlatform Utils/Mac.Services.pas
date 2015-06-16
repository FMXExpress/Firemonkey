unit Mac.Services;

interface

uses
  Xplat.Services,
  MacApi.AppKit,
  FMX.Platform.Mac;

type
  TMacPleaseWait = class(TInterfacedObject, IPleaseWaitService)
  private
    FView: NSProgressIndicator;
  public
    procedure StartWait;
    procedure StopWait;
  end;

implementation

uses
  FMX.Forms, FMX.Platform, Apple.Utils, Macapi.CocoaTypes;

{ TMacPleaseWait }

procedure TMacPleaseWait.StartWait;
const
  cnHeight = 30;
  cnWidth = 30;
var
  lView: NSView;
begin
  lView := ActiveView;
  FView := TNSProgressIndicator.Create;
  FView.initWithFrame(MakeNSRect(
    ((lView.bounds.size.width / 2) - (cnWidth / 2)),
    ((lView.bounds.size.height / 2) - (cnHeight /2)),
    cnWidth,
    cnHeight));
  FView.setStyle(NSProgressIndicatorSpinningStyle);
  FView.setIndeterminate(True);
  lView.addSubview(FView);
  FView.startAnimation(PtrForObject(lView));
end;

procedure TMacPleaseWait.StopWait;
begin
  if Assigned(FView) then
  begin
    FView.stopAnimation(PtrForObject(ActiveView));
    FView.removeFromSuperview;
    FView := nil;
  end;
end;

initialization
  TPlatformServices.Current.AddPlatformService(IPleaseWaitService, TMacPleaseWait.Create);

finalization
  TPlatformServices.Current.RemovePlatformService(IPleaseWaitService);

end.
