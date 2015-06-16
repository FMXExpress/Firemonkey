unit iOS.Services;

interface

uses
  Xplat.Services,
  iOSApi.UiKit,
  FMX.Platform.iOS,
  System.Generics.Collections;

type
  TioSPleaseWait = class(TInterfacedObject, IPleaseWaitService)
  private
    FView: UIActivityIndicatorView;
    FCount: Integer;
  public
    procedure StartWait;
    procedure StopWait;
  end;

implementation

uses
  FMX.Forms, FMX.Platform, Apple.Utils;

{ TiOSPleaseWait }

procedure TioSPleaseWait.StartWait;
var
  lView: UIView;
begin
  AtomicIncrement(FCount);
  if FCount = 1 then
  begin
    lView := ActiveView;
    FView := TUIActivityIndicatorView.Create;
    FView.setCenter(lView.center);
    FView.setActivityIndicatorViewStyle(UIActivityIndicatorViewStyleGray);
    lView.addSubview(FView);
    SharedApplication.setNetworkActivityIndicatorVisible(True);
    FView.startAnimating;
  end;
end;

procedure TioSPleaseWait.StopWait;
begin
  AtomicDecrement(FCount);
  if (FCount = 0) and Assigned(FView) then
  begin
    FView.stopAnimating;
    FView.removeFromSuperview;
    SharedApplication.setNetworkActivityIndicatorVisible(False);
    FView := nil;
  end;
end;

initialization
  TPlatformServices.Current.AddPlatformService(IPleaseWaitService, TioSPleaseWait.Create);

finalization
  TPlatformServices.Current.RemovePlatformService(IPleaseWaitService);

end.
