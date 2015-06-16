unit AnonThread;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  EAnonymousThreadException = class(Exception);

  TAnonymousThread<T> = class(TThread)
  private
    class var
      CRunningThreads:TList<TThread>;
  private
    FThreadFunc: TFunc<T>;
    FOnErrorProc: TProc<Exception>;
    FOnFinishedProc: TProc<T>;
    FResult: T;
    FStartSuspended: Boolean;
  private
    procedure ThreadTerminate(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;
      AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False;
      AFreeOnTerminate: Boolean = True);

    class constructor Create;
    class destructor Destroy;
 end;

implementation

{$IFDEF MACOS}
uses
{$IFDEF IOS}
  iOSapi.Foundation
{$ELSE}
  MacApi.Foundation
{$ENDIF IOS}
  ;
{$ENDIF MACOS}

{ TAnonymousThread }

class constructor TAnonymousThread<T>.Create;
begin
  inherited;
  CRunningThreads := TList<TThread>.Create;
end;

class destructor TAnonymousThread<T>.Destroy;
begin
  CRunningThreads.Free;
  inherited;
end;

constructor TAnonymousThread<T>.Create(AThreadFunc: TFunc<T>; AOnFinishedProc: TProc<T>;
  AOnErrorProc: TProc<Exception>; ACreateSuspended: Boolean = False; AFreeOnTerminate: Boolean = True);
begin
  FOnFinishedProc := AOnFinishedProc;
  FOnErrorProc := AOnErrorProc;
  FThreadFunc := AThreadFunc;
  OnTerminate := ThreadTerminate;
  FreeOnTerminate := AFreeOnTerminate;
  FStartSuspended := ACreateSuspended;

  //Store a reference to this thread instance so it will play nicely in an ARC
  //environment. Failure to do so can result in the TThread.Execute method
  //not executing. See http://qc.embarcadero.com/wc/qcmain.aspx?d=113580
  CRunningThreads.Add(Self);

  inherited Create(ACreateSuspended);
end;

procedure TAnonymousThread<T>.Execute;
{$IFDEF MACOS}
var
  lPool: NSAutoreleasePool;
{$ENDIF}
begin
{$IFDEF MACOS}
  //Need to create an autorelease pool, otherwise any autorelease objects
  //may leak.
  //See https://developer.apple.com/library/ios/#documentation/Cocoa/Conceptual/MemoryMgmt/Articles/mmAutoreleasePools.html#//apple_ref/doc/uid/20000047-CJBFBEDI
  lPool := TNSAutoreleasePool.Create;
  try
{$ENDIF}
    FResult := FThreadFunc;
{$IFDEF MACOS}
  finally
    lPool.drain;
  end;
{$ENDIF}
end;

procedure TAnonymousThread<T>.ThreadTerminate(Sender: TObject);
var
  lException: Exception;
begin
  try
    if Assigned(FatalException) and Assigned(FOnErrorProc) then
    begin
      if FatalException is Exception then
        lException := Exception(FatalException)
      else
        lException := EAnonymousThreadException.Create(FatalException.ClassName);
      FOnErrorProc(lException)
    end
    else if Assigned(FOnFinishedProc) then
      FOnFinishedProc(FResult);
  finally
    CRunningThreads.Remove(Self);
  end;
end;

end.
