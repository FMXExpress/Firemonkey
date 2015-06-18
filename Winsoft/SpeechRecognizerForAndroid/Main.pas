unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TFormMain = class(TForm)
    ButtonListen: TButton;
    StatusBar: TStatusBar;
    LabelStatus: TLabel;
    LabelResult: TLabel;
    procedure ButtonListenClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Androidapi.JNIBridge, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  Androidapi.Helpers, FMX.Helpers.Android, android.speech;

{$R *.fmx}

function ErrorMessage(Error: Integer): string;
begin
  case Error of
    TJSpeechRecognizer_ERROR_AUDIO: Result := 'Audio recording error.';
    TJSpeechRecognizer_ERROR_CLIENT: Result := 'Other client side errors.';
    TJSpeechRecognizer_ERROR_INSUFFICIENT_PERMISSIONS: Result := 'Insufficient permissions.';
    TJSpeechRecognizer_ERROR_NETWORK: Result := 'Other network related errors.';
    TJSpeechRecognizer_ERROR_NETWORK_TIMEOUT: Result := 'Network operation timed out.';
    TJSpeechRecognizer_ERROR_NO_MATCH: Result := 'No recognition result matched.';
    TJSpeechRecognizer_ERROR_RECOGNIZER_BUSY: Result := 'RecognitionService busy.';
    TJSpeechRecognizer_ERROR_SERVER: Result := 'Server sends error status.';
    TJSpeechRecognizer_ERROR_SPEECH_TIMEOUT: Result := 'No speech input.';
    else Result := 'Unknown error ' + IntToStr(Error);
  end;
end;

type
  TRecognitionListener = class(TJavaLocal, JRecognitionListener)
    procedure onBeginningOfSpeech; cdecl;
    procedure onBufferReceived(Buffer: TJavaArray<Byte>); cdecl;
    procedure onEndOfSpeech; cdecl;
    procedure onError(Error: Integer); cdecl;
    procedure onEvent(EventType: Integer; Params: JBundle); cdecl;
    procedure onPartialResults(PartialResults: JBundle); cdecl;
    procedure onReadyForSpeech(Params: JBundle); cdecl;
    procedure onResults(Results: JBundle); cdecl;
    procedure onRmsChanged(RmsdB: Single); cdecl;
  end;

procedure TRecognitionListener.onBeginningOfSpeech;
begin
  FormMain.ButtonListen.Enabled := False;
  FormMain.LabelStatus.Text := 'Beginning of speech';
end;

procedure TRecognitionListener.onBufferReceived(Buffer: TJavaArray<Byte>);
begin
end;

procedure TRecognitionListener.onEndOfSpeech;
begin
  FormMain.LabelStatus.Text := 'End of speech';
  FormMain.ButtonListen.Enabled := True;
end;

procedure TRecognitionListener.onError(Error: Integer);
begin
  FormMain.LabelStatus.Text := 'Error: ' + ErrorMessage(Error);
end;

procedure TRecognitionListener.onEvent(EventType: Integer; Params: JBundle);
begin
end;

procedure TRecognitionListener.onPartialResults(PartialResults: JBundle);
begin
end;

procedure TRecognitionListener.onReadyForSpeech(Params: JBundle);
begin
  FormMain.LabelStatus.Text := 'Ready for speech';
end;

procedure TRecognitionListener.onResults(Results: JBundle);
var
  ArrayList: JArrayList;
  I: Integer;
  Text: string;
begin
  Text := '';
  ArrayList := Results.getStringArrayList(StringToJString(TJSpeechRecognizer_RESULTS_RECOGNITION));
  for I := 0 to ArrayList.size - 1 do
    Text := Text + JStringToString(ArrayList.get(I).toString) + sLineBreak;

  FormMain.LabelResult.Text := Text;
  FormMain.LabelStatus.Text := 'Results';
end;

procedure TRecognitionListener.onRmsChanged(RmsdB: Single);
begin
end;

var
  SpeechRecognizer: JSpeechRecognizer;
  RecognitionListener: JRecognitionListener;

procedure TFormMain.ButtonListenClick(Sender: TObject);
begin
  FormMain.LabelResult.Text := '';
  FormMain.LabelStatus.Text := '';

  if not TJSpeechRecognizer.JavaClass.isRecognitionAvailable(SharedActivityContext) then
  begin
    ShowMessage('Speech recognition is not available');
    Exit;
  end;

  CallInUiThread(
    procedure
    begin
      if SpeechRecognizer = nil then
      begin
        SpeechRecognizer := TJSpeechRecognizer.JavaClass.createSpeechRecognizer(SharedActivityContext);
        RecognitionListener := TRecognitionListener.Create;
        SpeechRecognizer.setRecognitionListener(RecognitionListener);
      end;
      SpeechRecognizer.startListening(TJRecognizerIntent.JavaClass.getVoiceDetailsIntent(SharedActivityContext));
    end);
end;

end.
