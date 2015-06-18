unit EventDispatcherThread;

interface

uses Windows, Classes;

type
  TEventDispatcherThread = class(TThread)
  private
    FskeletonEvent,
    FtargetWindow : cardinal;
  protected
    procedure Execute; override;

  public
    constructor createWith(targetWindow, skeletonEvent : cardinal);
  end;

implementation

uses Messages;

{ TEventDispatcherThread }

constructor TEventDispatcherThread.createWith(targetWindow,
  skeletonEvent: cardinal);
begin
  inherited Create(true);

  FskeletonEvent := skeletonEvent;
  FtargetWindow := targetWindow;

  Resume;
end;

procedure TEventDispatcherThread.Execute;
begin
  if (FtargetWindow = INVALID_HANDLE_VALUE) or
     (FskeletonEvent = INVALID_HANDLE_VALUE) then exit;

  while not terminated do begin
    if WaitForSingleObject(FskeletonEvent, 100) <> WAIT_OBJECT_0 then continue;

    SendMessage(FtargetWindow, WM_USER, integer(self), 0);
  end;
end;

end.
