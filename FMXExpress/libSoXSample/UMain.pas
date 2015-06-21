unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.IOUtils, FMX.Layouts, FMX.Memo, FMX.StdCtrls,
  FMX.Media, FMX.Controls.Presentation, FMX.ScrollBox, GameAudioManager
{$IFDEF ANDROID}
, Posix.Dlfcn
{$ENDIF}
;

type
  TfrmMain = class(TForm)
    mLog: TMemo;
    btnTransform: TButton;
    Button1: TButton;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure btnTransformClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    LibSoX_Handle: THandle;
    Libsox_Path: String;
    function GetDataDirectory(Filename: String): String;
    procedure Log(Value: String);
    function RegisterSound(Filename: String): Integer;
    procedure UnRegisterSound(Index: Integer);
    procedure PlaySound(Index: Integer); overload;
    procedure PlaySound(AName: String); overload;
    procedure ConvertAUtoWAV(InFile, OutFile: String);
{$IFDEF ANDROID}
    procedure AndroidConvertAUtoWAV(InFile, OutFile: String);
{$ENDIF}
{$IF DEFINED(MACOS) AND DEFINED(IOS)}
    procedure IOSConvertAUtoWAV(InFile, OutFile: String);
{$ENDIF}
{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
    procedure OSXConvertAUtoWAV(InFile, OutFile: String);
{$ENDIF}
{$IFDEF MSWINDOWS}
    procedure WinConvertAUtoWAV(InFile, OutFile: String);
{$ENDIF}
  public
    AudioManager: TGameAudioManager;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

uses SoX;

procedure TfrmMain.PlaySound(Index: Integer);
begin
    AudioManager.PlaySound(Index);
end;

procedure TfrmMain.PlaySound(AName: String);
begin
    AudioManager.PlaySound(AName);
end;

procedure TfrmMain.UnRegisterSound(Index: Integer);
begin
    AudioManager.DeleteSound(Index);
end;

function TfrmMain.RegisterSound(Filename: String): Integer;
begin
  if FileExists(Filename) then
    Result := AudioManager.AddSound(Filename)
  else
    Result := -1;
end;

procedure TfrmMain.btnTransformClick(Sender: TObject);
begin
  ConvertAUtoWAV('bwoman.au','bwoman_new.wav');
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  FileName: String;
begin
  FileName := GetDataDirectory('bwoman.au');
  if FileExists(FileName) then
  begin
   RegisterSound(FileName);
   PlaySound(ChangeFileExt(ExtractFileName(FileName),''));
  end
  else
    mLog.Lines.Add('File ' + FileName + ' does not exists!');
end;

function TfrmMain.GetDataDirectory(Filename: String): String;
begin
{$IFDEF ANDROID}
  Result := TPath.Combine(TPath.GetDocumentsPath, Filename);
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF IOS}
  Result := TPath.Combine(TPath.GetDocumentsPath, Filename);
  {$ELSE}
  Result := ExtractFilePath(ParamStr(0)) + Filename;
  {$ENDIF}
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
  Result := ExtractFilePath(ParamStr(0)) + Filename;
{$ENDIF}
end;

procedure TfrmMain.Button2Click(Sender: TObject);
var
  FileName: String;
begin
  FileName := GetDataDirectory('bwoman_new.wav');
  if FileExists(FileName) then
  begin
   RegisterSound(FileName);
   PlaySound(ChangeFileExt(ExtractFileName(FileName),''));
  end
  else
    mLog.Lines.Add('File ' + FileName + ' does not exists!');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  AudioManager := TGameAudioManager.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  AudioManager.Free;
{$IFDEF ANDROID}
  if LibSoX_Handle <> 0 then
    dlclose(LibSoX_Handle);
{$ENDIF}
end;

{$IFDEF ANDROID}
procedure TfrmMain.AndroidConvertAUtoWAV(InFile, OutFile: String);

  procedure LogMessage(AMessage: String);
  begin
    mLog.BeginUpdate;
    mLog.Lines.Add(AMessage);
    mLog.EndUpdate;
    Application.ProcessMessages;
  end;

var
  s: String;
  AResult: Integer;
  SourceFile, TargetFile: MarshaledAString;
  in_, out_: psox_format_t;
  chain: psox_effects_chain_t;
  e: psox_effect_t;
  h: sox_effect_handler_t;
  args: array[0..9] of MarshaledAString;
  LastError: Integer;
  Marshaller: TMarshaller;
begin
  mLog.Lines.Clear;
  mLog.Lines.Add('LibDir: ' + TPath.GetDocumentsPath);

  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'liblpc10.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libgsm.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libogg.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libvorbis.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libvorbisenc.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libvorbisfile.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libFLAC.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libmp3lame.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libmad.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libpng.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libsmr.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libsmrx.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libsndfile.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libwavpack.so'))), RTLD_LAZY);
  dlopen(MarshaledAString(Marshaller.AsAnsi(TPath.Combine(TPath.GetDocumentsPath, 'libfmemopen.so'))), RTLD_LAZY);

  Libsox_Path := TPath.Combine(TPath.GetDocumentsPath, 'libsox.so');
  if FileExists(Libsox_Path) then
  begin
    mLog.Lines.Add ('Library file found: ' + Libsox_Path);
    LibSoX_Handle := THandle(dlopen(MarshaledAString(Marshaller.AsAnsi(Libsox_Path)), RTLD_LAZY));
    if LibSoX_Handle = 0 then
    begin
      s := dlerror;
      mLog.Lines.Add ('Cannot open library: ' + Libsox_Path);
      mLog.Lines.Add ('Error: ' + s);
    end
    else
      mLog.Lines.Add ('Opened library: ' + Libsox_Path);
  end
  else
    mLog.Lines.Add ('File not found: ' + Libsox_Path);

  if LibSoX_Handle <> 0 then
  begin
    sox_version := dlsym(LibSoX_Handle, 'sox_version');
    if not Assigned(sox_version) then
      mLog.Lines.Add('Cannot create function: sox_version')
    else
    begin
      s := sox_version;

      LogMessage(Format('sox_version=%s', [s]));

      SourceFile := MarshaledAString(Marshaller.AsAnsi(InFile));
      TargetFile := MarshaledAString(Marshaller.AsAnsi(OutFile));

      if FileExists(TargetFile) then
      begin
        LogMessage('Target file already exists');
        TFile.Delete(TargetFile);
        LogMessage('Existing target file deleted');
      end;

      if FileExists(SourceFile) then
      begin
        // All libSoX applications must start by initialising the SoX library
        sox_init := dlsym(LibSoX_Handle, 'sox_init');
        LogMessage('sox_init');

        AResult := sox_init;
        if (not Assigned(sox_init)) or (AResult <> Integer(SOX_SUCCESS)) then
          raise(Exception.Create('Can''t initialize SoX library'));

        sox_open_read := dlsym(LibSoX_Handle, 'sox_open_read');
        if not Assigned(sox_open_read) then
          raise(Exception.Create('Can''t initialize sox_open_read'));

        LogMessage('sox_open_read');

        // Open the input file (with default parameters)
        in_ := sox_open_read(SourceFile, nil, nil, nil);

        if (not Assigned(in_)) or (in_ = nil) then
          LogMessage('in_ does not assigned');

        sox_open_write := dlsym(LibSoX_Handle, 'sox_open_write');

        LogMessage('sox_open_write');

        if not Assigned(sox_open_write) then
          raise(Exception.Create('Can''t initialize sox_open_write'));
        // Open the output file; we must specify the output signal characteristics.
        // Since we are using only simple effects, they are the same as the input
        // file characteristics
        out_ := sox_open_write(TargetFile, @(in_^.signal), nil, nil, nil, nil);

        if (not Assigned(out_)) or (out_ = nil) then
          LogMessage('out_ does not assigned');

        sox_create_effects_chain := dlsym(LibSoX_Handle, 'sox_create_effects_chain');
        // Create an effects chain; some effects need to know about the input
        // or output file encoding so we provide that information here
        chain := sox_create_effects_chain(@in_^.encoding, @out_^.encoding);

        if not Assigned(chain) then
        begin
          mLog.Lines.Add('Effect chain does not assigned');
          Application.ProcessMessages;
        end;

        sox_create_effect := dlsym(LibSoX_Handle, 'sox_create_effect');
        sox_find_effect := dlsym(LibSoX_Handle, 'sox_find_effect');
        // The first effect in the effect chain must be something that can source
        // samples; in this case, we use the built-in handler that inputs
        // data from an audio file
        e := sox_create_effect(sox_find_effect('input'));
        args[0] := MarshaledAString(in_);

        sox_effect_options := dlsym(LibSoX_Handle, 'sox_effect_options');

        LastError := sox_effect_options(e, 1, args);
        if LastError <> Integer(SOX_SUCCESS) then
          raise(Exception.Create('sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS); LastError=' + LastError.ToString));

        sox_add_effect := dlsym(LibSoX_Handle, 'sox_add_effect');

        LogMessage('1');

        // This becomes the first `effect' in the chain
        sox_add_effect(chain, e, @(in_^.signal), @(in_^.signal));
        //if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
          //raise(Exception.Create('sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> SOX_SUCCESS'));

        LogMessage('2');

        // The last effect in the effect chain must be something that only consumes
        // samples; in this case, we use the built-in handler that outputs
        // data to an audio file
        h := sox_find_effect('output');

        LogMessage('3');

        if not Assigned(h) then
          LogMessage('4');

        e := sox_create_effect(h);

        LogMessage('5');

        args[0] := MarshaledAString(out_);

        LogMessage('6');

        LastError := sox_effect_options(e, 1, args);
        if LastError <> Integer(SOX_SUCCESS) then
          raise(Exception.Create('sox_effect_options(e, 1, args) <> SOX_SUCCESS; LastError=' + LastError.ToString));

        LogMessage('7');

        if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
          raise(Exception.Create('sox_add_effect(chain, e, &in->signal, &in->signal) <> SOX_SUCCESS'));

        sox_flow_effects := dlsym(LibSoX_Handle, 'sox_flow_effects');

        LogMessage('8');

        // Flow samples through the effects processing chain until EOF is reached
        sox_flow_effects(chain, nil, nil);

        LogMessage('9');

        sox_delete_effects_chain := dlsym(LibSoX_Handle, 'sox_delete_effects_chain');
        // All done; tidy up:
        sox_delete_effects_chain(chain);

        LogMessage('10');

        sox_close := dlsym(LibSoX_Handle, 'sox_close');
        if not Assigned(sox_close) then
          LogMessage('Can''t initialize sox_close');

        LogMessage('11');

        sox_close(out_);

        LogMessage('12');

        sox_close := dlsym(LibSoX_Handle, 'sox_close');

        if not Assigned(sox_close) then
          LogMessage('Can''t initialize sox_close 2');

        LogMessage('13');

        sox_close(in_);

        LogMessage('14');

        sox_quit := dlsym(LibSoX_Handle, 'sox_quit');

        LogMessage('15');

        if not Assigned(sox_quit) then
          LogMessage('Can''t initialize sox_quit');

        LogMessage('16');

        sox_quit;

        LogMessage('17');
      end;
    end;

    dlclose(LibSoX_Handle);
  end;
end;
{$ENDIF}

procedure TfrmMain.ConvertAUtoWAV(InFile, OutFile: String);
begin

{$IFDEF ANDROID}
AndroidConvertAUtoWAV(TPath.Combine(TPath.GetDocumentsPath, InFile),
  TPath.Combine(TPath.GetDocumentsPath, OutFile));
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS}
    IOSConvertAUtoWAV(TPath.Combine(TPath.GetDocumentsPath, InFile),
      TPath.Combine(TPath.GetDocumentsPath, Outfile));
  {$ELSE}
    OSXConvertAUtoWAV(ExtractFilePath(ParamStr(0)) + InFile,
      ExtractFilePath(ParamStr(0)) + OutFile);
  {$ENDIF}
{$ENDIF MACOS}

{$IFDEF MSWINDOWS}
WinConvertAUtoWAV(ExtractFilePath(ParamStr(0)) + InFile,
  ExtractFilePath(ParamStr(0)) + OutFile);
{$ENDIF}
end;

{$IF DEFINED(MACOS) AND DEFINED(IOS)}
procedure TfrmMain.IOSConvertAUtoWAV(InFile, OutFile: String);
var
  SourceFile, TargetFile: MarshaledAString;
	// sox_format_t *in, *out; /* input and output files */
  in_, out_: psox_format_t;
  // sox_effects_chain_t * chain;
  chain: psox_effects_chain_t;
  // sox_effect_t * e;
  e: psox_effect_t;
  // char *args[10]
  args: array[0..9] of MarshaledAString;
  LastError: Integer;
  Marshaller: TMarshaller;
  S: String;
begin
  //StringToOleStr()
  SourceFile := MarshaledAString(Marshaller.AsAnsi(InFile));
  TargetFile := MarshaledAString(Marshaller.AsAnsi(OutFile));

  if FileExists(MarshaledAString(SourceFile)) then
  begin
    // All libSoX applications must start by initialising the SoX library
    if sox_init <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('Can''t initialize SoX library'));

    // Open the input file (with default parameters)
    in_ := sox_open_read(SourceFile, nil, nil, nil);

    // Open the output file; we must specify the output signal characteristics.
    // Since we are using only simple effects, they are the same as the input
    // file characteristics
    out_ := sox_open_write(TargetFile, @in_^.signal, nil, nil, nil, nil);

    // Create an effects chain; some effects need to know about the input
    // or output file encoding so we provide that information here
    chain := sox_create_effects_chain(@in_^.encoding, @out_^.encoding);

    // The first effect in the effect chain must be something that can source
    // samples; in this case, we use the built-in handler that inputs
    // data from an audio file
    e := sox_create_effect(sox_find_effect('input'));
    //args[0] = (char *)in, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(in_);
    LastError := sox_effect_options(e, 1, args);
    if LastError <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS); LastError=' + LastError.ToString));

    // This becomes the first `effect' in the chain
    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> SOX_SUCCESS'));

    // The last effect in the effect chain must be something that only consumes
    // samples; in this case, we use the built-in handler that outputs
    // data to an audio file
    e := sox_create_effect(sox_find_effect('output'));
    //args[0] = (char *)out, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(out_);
    LastError := sox_effect_options(e, 1, args);
    if LastError <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> SOX_SUCCESS; LastError=' + LastError.ToString));

    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, &in->signal, &in->signal) <> SOX_SUCCESS'));

    // Flow samples through the effects processing chain until EOF is reached
    sox_flow_effects(chain, nil, nil);

    // All done; tidy up:
    sox_delete_effects_chain(chain);
    sox_close(out_);
    sox_close(in_);
    sox_quit;
  end;
end;
{$ENDIF}

procedure TfrmMain.Log(Value: String);
begin
  mLog.BeginUpdate;
  mLog.Lines.Append(Value);
  mLog.EndUpdate;
end;


{$IF DEFINED(MACOS) AND NOT DEFINED(IOS)}
procedure TfrmMain.OSXConvertAUtoWAV(InFile, OutFile: String);
var
  SourceFile, TargetFile: MarshaledAString;
	// sox_format_t *in, *out; /* input and output files */
  in_, out_: psox_format_t;
  // sox_effects_chain_t * chain;
  chain: psox_effects_chain_t;
  // sox_effect_t * e;
  e: psox_effect_t;
  // char *args[10]
  args: array[0..9] of MarshaledAString;
  Marshaller: TMarshaller;
  LastError: Integer;
begin
  SourceFile := MarshaledAString(Marshaller.AsAnsi(InFile));
  Log('Source file: ' + InFile);
  TargetFile := MarshaledAString(Marshaller.AsAnsi(OutFile));
  if FileExists(SourceFile) then
  begin
    // All libSoX applications must start by initialising the SoX library
    if sox_init <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('Can''t initialize SoX library'));

    // Open the input file (with default parameters)
    in_ := sox_open_read(SourceFile, nil, nil, nil);

    // Open the output file; we must specify the output signal characteristics.
    // Since we are using only simple effects, they are the same as the input
    // file characteristics
    out_ := sox_open_write(TargetFile, @in_^.signal, nil, nil, nil, nil);

    // Create an effects chain; some effects need to know about the input
    // or output file encoding so we provide that information here
    chain := sox_create_effects_chain(@in_^.encoding, @out_^.encoding);

    // The first effect in the effect chain must be something that can source
    // samples; in this case, we use the built-in handler that inputs
    // data from an audio file
    e := sox_create_effect(sox_find_effect('input'));
    //args[0] = (char *)in, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(in_);
    LastError := sox_effect_options(e, 1, args);
    if LastError <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS); LastError=' + LastError.ToString(LastError)));

    // This becomes the first `effect' in the chain
    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> SOX_SUCCESS'));

    // The last effect in the effect chain must be something that only consumes
    // samples; in this case, we use the built-in handler that outputs
    // data to an audio file
    e := sox_create_effect(sox_find_effect('output'));
    //args[0] = (char *)out, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(out_);
    LastError := sox_effect_options(e, 1, args);
    if LastError <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> SOX_SUCCESS; LastError=' + LastError.ToString(LastError)));

    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, &in->signal, &in->signal) <> SOX_SUCCESS'));

    // Flow samples through the effects processing chain until EOF is reached
    sox_flow_effects(chain, nil, nil);

    // All done; tidy up:
    sox_delete_effects_chain(chain);
    sox_close(out_);
    sox_close(in_);
    sox_quit;

    if FileExists(TargetFile) then
     Log('Target written: ' + TargetFile);
  end
  else
    Log('File does not exists');
end;
{$ENDIF}

{$IF DEFINED(MSWINDOWS)}
procedure TfrmMain.WinConvertAUtoWAV(InFile, OutFile: String);
var
  SourceFile, TargetFile: MarshaledAString;
	// sox_format_t *in, *out; /* input and output files */
  in_, out_: psox_format_t;
  // sox_effects_chain_t * chain;
  chain: psox_effects_chain_t;
  // sox_effect_t * e;
  e: psox_effect_t;
  // char *args[10]
  args: array[0..9] of MarshaledAString;
begin
  SourceFile := MarshaledAString(AnsiString(InFile));
  TargetFile := MarshaledAString(AnsiString(OutFile));
  if FileExists(SourceFile) then
  begin
    // All libSoX applications must start by initialising the SoX library
    if sox_init <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('Can''t initialize SoX library'));

    // Open the input file (with default parameters)
    in_ := sox_open_read(SourceFile, nil, nil, nil);

    // Open the output file; we must specify the output signal characteristics.
    // Since we are using only simple effects, they are the same as the input
    // file characteristics
    out_ := sox_open_write(TargetFile, @in_^.signal, nil, nil, nil, nil);

    // Create an effects chain; some effects need to know about the input
    // or output file encoding so we provide that information here
    chain := sox_create_effects_chain(@in_^.encoding, @out_^.encoding);

    // The first effect in the effect chain must be something that can source
    // samples; in this case, we use the built-in handler that inputs
    // data from an audio file
    e := sox_create_effect(sox_find_effect('input'));
    //args[0] = (char *)in, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(in_);
    if sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS)'));

    // This becomes the first `effect' in the chain
    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> SOX_SUCCESS'));

    // The last effect in the effect chain must be something that only consumes
    // samples; in this case, we use the built-in handler that outputs
    // data to an audio file
    e := sox_create_effect(sox_find_effect('output'));
    //args[0] = (char *)out, assert(sox_effect_options(e, 1, args) == SOX_SUCCESS);
    args[0] := MarshaledAString(out_);
    if sox_effect_options(e, 1, args) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_effect_options(e, 1, args) <> SOX_SUCCESS'));

    if sox_add_effect(chain, e, @in_^.signal, @in_^.signal) <> Integer(SOX_SUCCESS) then
      raise(Exception.Create('sox_add_effect(chain, e, &in->signal, &in->signal) <> SOX_SUCCESS'));

    // Flow samples through the effects processing chain until EOF is reached
    sox_flow_effects(chain, nil, nil);

    // All done; tidy up:
    sox_delete_effects_chain(chain);
    sox_close(out_);
    sox_close(in_);
    sox_quit;
  end;
end;
{$ENDIF}

end.
