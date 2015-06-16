unit Xplat.Utils;

interface

uses
  System.SysUtils, AnonThread, System.Generics.Collections, System.Classes,
  Xplat.Services;

type
  EAsyncException = class(Exception);

  TAsyncProgress<T> = class
  public
    class procedure Execute(AFunc: TFunc<T>; AOnFinished: TProc<T>;
      AOnError: TProc<Exception>);
  end;

function PleaseWaitService: IPleaseWaitService;
procedure ProgressProc(AProc: TProc);
procedure StartWait;
procedure StopWait;

implementation

uses
  FMX.Platform;

procedure ProgressProc(AProc: TProc);
begin
  StartWait;
  try
    AProc;
  finally
    StopWait;
  end;
end;

{ TAsyncProgress<T> }

class procedure TAsyncProgress<T>.Execute(AFunc: TFunc<T>;
  AOnFinished: TProc<T>; AOnError: TProc<Exception>);
var
  lThread: TAnonymousThread<T>;
begin
  StartWait;
  lThread := TAnonymousThread<T>.Create(
    //Method called in TThread.Execute
    AFunc,
    //Method called in TThread.OnTerminate if there was no exception raised in
    //the running thread. Will run in main thread.
    procedure(AResult: T)
    begin
      StopWait;
      if Assigned(AOnFinished) then
        AOnFinished(AResult);
    end,
    //Method called in TThread.OnTerminate if there was an exception raised in
    //the running thread. Will run in main thread.
    procedure(AError: Exception) begin
      StopWait;
      if Assigned(AOnError) then
        AOnError(AError);
    end);
end;

function PleaseWaitService: IPleaseWaitService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IPleaseWaitService) then
    Result := TPlatformServices.Current.GetPlatformService(IPleaseWaitService) as IPleaseWaitService
  else
    Result := nil;
end;

procedure ToggleWait(AStart: Boolean);
var
  lSvc: IPleaseWaitService;
begin
  lSvc := PleaseWaitService;
  if Assigned(lSvc) then
    if AStart then
      lSvc.StartWait
    else
      lSvc.StopWait;
end;

procedure StartWait;
begin
  ToggleWait(True);
end;

procedure StopWait;
begin
  ToggleWait(False);
end;

end.
