//
// This unit is part of the GLScene Project, http://glscene.org
// Rewrite and simplify TGLCadencer.
// Wqyfavor    wqyfavor@qq.com

unit MSTimer;

interface

uses
   Classes,
   Windows,
   Messages,
   Forms;

type
   TProgressEvent = procedure(const deltaTime, newTime: Double) of object;

   TMSTimer = class
   private
      { Private Declarations }
      FTimeMultiplier: Double;
      lastTime, downTime, lastMultiplier: Double;
      FEnabled: Boolean;
      FSleepLength: Integer;
      FCurrentTime: Double;
      FOriginTime: Double;
      FLastDeltaTime: Double;
      FBaseTime: Double;
      FOnProgress: TProgressEvent;
      FProgressing: Integer;
      procedure SetCurrentTime(const Value: Double);
      function _FGetCurrentTime: Double;

   protected
      { Protected Declarations }
      function StoreTimeMultiplier: Boolean;
      procedure SetEnabled(const val: Boolean);
      procedure SetTimeMultiplier(const val: Double);

      function GetRawReferenceTime: Double;
   public
      { Public Declarations }
      constructor Create;
      destructor Destroy; override;

      procedure Progress;
      function GetCurrentTime: Double;

      function IsBusy: Boolean;
      procedure Reset;

      property BaseTime: Double read FBaseTime write FBaseTime;
      property OriginTime: Double read FOriginTime write FOriginTime;
      property CurrentTime: Double read _FGetCurrentTime write SetCurrentTime;

   published
      property Enabled: Boolean read FEnabled write SetEnabled default False;
      property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier stored StoreTimeMultiplier;
      property LastDeltaTime: Double read FLastDeltaTime write FLastDeltaTime;
      property SleepLength: Integer read FSleepLength write FSleepLength default -1;

      property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
   end;

var
   MSCadencer: TMSTimer = nil;

implementation
uses SysUtils;

var
   vCounterFrequency: Int64;

type
   TASAPHandler = class
      FWindowHandle: HWND;
      FTooFastCounter: Integer;
      FTimer: Cardinal;

      procedure Trigger;
      procedure WndProc(var Message: TMessage);

      constructor Create;
      destructor Destroy; override;
   end;

var
   vWMTickCadencer: Cardinal;
   vHandler: TASAPHandler;

constructor TASAPHandler.Create;
begin
   inherited Create;
   FWindowHandle := AllocateHWnd(WndProc);
   Trigger;
end;

destructor TASAPHandler.Destroy;
begin
   if FTimer <> 0 then
      KillTimer(FWindowHandle, FTimer);
   DeallocateHWnd(FWindowHandle);
   inherited Destroy;
end;

var
   vWndProcInLoop: Boolean;

procedure TASAPHandler.Trigger;
begin
   PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
end;

procedure TASAPHandler.WndProc(var Message: TMessage);
begin
   with Message do
   begin
      if Msg = WM_TIMER then
      begin
         KillTimer(FWindowHandle, FTimer);
         FTimer := 0;
      end;
      if (Msg <> WM_TIMER) and (Cardinal(GetMessageTime) = GetTickCount) then
      begin
         // if we're going too fast, "sleep" for 1 msec
         Inc(FTooFastCounter);
         if FTooFastCounter > 5000 then
         begin
            if FTimer = 0 then
               FTimer := SetTimer(FWindowHandle, 1, 1, nil);
            FTooFastCounter := 0;
         end;
      end
      else
         FTooFastCounter := 0;
      if FTimer <> 0 then
      begin
         Result := 0;
         Exit;
      end;
      if not vWndProcInLoop then
      begin
         vWndProcInLoop := True;
         try
            if (Msg = vWMTickCadencer) or (Msg = WM_TIMER) then
               if Assigned(MSCadencer) and MSCadencer.Enabled then
               begin
                  if MSCadencer.FProgressing = 0 then
                     if Application.Terminated then
                        MSCadencer.Enabled := False // force stop
                     else
                     begin
                        try
                           MSCadencer.Progress; // do stuff
                        except
                           Application.HandleException(Self);
                           MSCadencer.Enabled := False; // it faulted, stop it
                        end
                     end;
                  Trigger; // Infinite loop...
               end;
         finally
            vWndProcInLoop := False;
         end;
      end;
      Result := 0;
   end;
end;

constructor TMSTimer.Create;
begin
   downTime := GetRawReferenceTime;
   FOriginTime := downTime;
   FTimeMultiplier := 1;
   FSleepLength := -1;
   FBaseTime := 0.0;
   Enabled := False;
end;

destructor TMSTimer.Destroy;
begin
   Assert(FProgressing = 0);
   inherited Destroy;
end;

procedure TMSTimer.SetEnabled(const val: Boolean);
begin
   if FEnabled <> val then
   begin
      FEnabled := val;
      if FEnabled then
      begin
         FOriginTime := FOriginTime + GetRawReferenceTime - downTime;
         vHandler.Trigger;
      end
      else
         downTime := GetRawReferenceTime;
   end;
end;

procedure TMSTimer.SetTimeMultiplier(const val: Double);
var
   rawRef: Double;
begin
   if val <> FTimeMultiplier then
   begin
      if val = 0 then
      begin
         lastMultiplier := FTimeMultiplier;
         Enabled := False;
      end
      else
      begin
         rawRef := GetRawReferenceTime;
         if FTimeMultiplier = 0 then
         begin
            Enabled := True;
            FOriginTime := rawRef - (rawRef - FOriginTime) * lastMultiplier / val;
         end
         else
            FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier / val;
      end;
      FTimeMultiplier := val;
   end;
end;

function TMSTimer.StoreTimeMultiplier: Boolean;
begin
   Result := (FTimeMultiplier <> 1);
end;

function TMSTimer._FGetCurrentTime: Double;
begin
   Result := FCurrentTime + FBaseTime;
end;

procedure TMSTimer.Progress;
var
   deltaTime, newTime: Double;
begin
   if FProgressing < 0 then
      Exit;
   if Enabled then
   begin
      if SleepLength >= 0 then
         Sleep(SleepLength);
      Application.ProcessMessages;
   end;
   Inc(FProgressing);
   try
      if Enabled then
      begin
         // One of the processed messages might have disabled us
         if Enabled then
         begin
            newTime := GetCurrentTime;
            deltaTime := newTime - lastTime;
            FLastDeltaTime := deltaTime;
            lastTime := newTime;
            if Assigned(FOnProgress) then
               FOnProgress(deltaTime, newTime + FBaseTime);
         end;
      end;
   finally
      Dec(FProgressing);
   end;
end;

function TMSTimer.GetRawReferenceTime: Double;
var
   counter: Int64;
begin
   QueryPerformanceCounter(counter);
   Result := counter / vCounterFrequency;
end;

function TMSTimer.GetCurrentTime: Double;
begin
   Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
   FCurrentTime := Result;
end;

function TMSTimer.IsBusy: Boolean;
begin
   Result := (FProgressing <> 0);
end;

procedure TMSTimer.Reset;
begin
   lastTime := 0;
   downTime := GetRawReferenceTime;
   FOriginTime := downTime;
   FBaseTime := 0.0;
   GetCurrentTime;
end;

procedure TMSTimer.SetCurrentTime(const Value: Double);
begin
  { LastTime := Value - (FCurrentTime - LastTime);
   FOriginTime := FOriginTime + (FCurrentTime - Value);
   FCurrentTime := Value;   }
   Reset;
   FBaseTime := Value;
end;

initialization
   // Get our Windows message ID
   vWMTickCadencer := RegisterWindowMessage('TimerTick');
   vHandler := TASAPHandler.Create;
   // Preparation for high resolution timer
   if not QueryPerformanceFrequency(vCounterFrequency) then
      vCounterFrequency := 0;

finalization
   FreeAndNil(vHandler);
end.

