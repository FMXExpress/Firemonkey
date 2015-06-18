{==============================================================================|
| Project : Bauglir Library                                                    |
|==============================================================================|
| Content: Generic objects                                                     |
|==============================================================================|
| Copyright (c)2011-2012, Bronislav Klucka                                     |
| All rights reserved.                                                         |
| Source code is licenced under original 4-clause BSD licence:                 |
| http://licence.bauglir.com/bsd4.php                                          |
|                                                                              |
|                                                                              |
|==============================================================================|
|==============================================================================}
unit BClasses;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

uses
  {$IFDEF UNIX}
  cthreads,
  {$ELSE UNIX}
  windows,
  {$ENDIF}
  Classes, SysUtils, SyncObjs;

Type

  {:abstract(Basic library aware thread)
    See @BauglirInDll variable
  }
  TBThread = class(TThread)
  protected
    fSyncLock: TCriticalSection;
    procedure Synchronize(AMethod: TThreadMethod);
  public
    constructor Create(CreateSuspended: Boolean); 
    destructor Destroy; override;
  end;

var
  {:If @TRUE, than method passed TBThread.Synchronize will be executed directly,
    without synchronization, useful for libraries and cosole projects.
  }
  BauglirSynchronizeThreads: boolean = false;

implementation

{ TBThread }

constructor TBThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  fSyncLock := TCriticalSection.Create;
end;

destructor TBThread.Destroy;
begin
  fSyncLock.Free;
  inherited;
end;

procedure TBThread.Synchronize(AMethod: TThreadMethod);
begin
  //fSyncLock.Enter;
  if (BauglirSynchronizeThreads) or (GetCurrentThreadID = MainThreadID) then aMethod
  else inherited Synchronize(aMethod);
  //fSyncLock.Leave;
end;

end.

