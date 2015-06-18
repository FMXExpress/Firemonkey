unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Androidapi.JNI.TTS, FMX.StdCtrls, FMX.Layouts, FMX.Memo, AndroidAPI.JNIBridge,
  Androidapi.JNI.Speech, Androidapi.JNI.Os, Androidapi.JNI.GraphicsContentViewText;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private

  type
    TttsOnInitListener = class(TJavaLocal, JTextToSpeech_OnInitListener)
    private
      [weak] FParent : TForm1;
    public
      constructor Create(AParent : TForm1);
      procedure onInit(status: Integer); cdecl;
    end;

    TRecognitionListener = class(TJavaLocal, JRecognitionListener)
    private
      [weak] FParent : TForm1;
    public
      constructor Create(AParent : TForm1);
      procedure onBeginningOfSpeech; cdecl;
      procedure onBufferReceived(buffer: TJavaArray<byte>); cdecl;
      procedure onEndOfSpeech; cdecl;
      procedure onError(error: Integer); cdecl;
      procedure onEvent(eventType : Integer; params : JBundle); cdecl;
      procedure onPartialResults(partialResults : JBundle); cdecl;
      procedure onReadyForSpeech(params : JBundle); cdecl;
      procedure onResults(results : JBundle); cdecl;
      procedure onRmsChanged(rmsdB : Single); cdecl;
    end;

  private
    { Private declarations }
    ttsListener : TttsOnInitListener;
    tts : JTextToSpeech;
    RecListener : TRecognitionListener;
    SpeechRecognizer : JSpeechRecognizer;
    RecognizerIntent : JIntent;

    procedure SpeakOut;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

// List<ResolveInfo> activities = pm.queryIntentActivities(new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH), 0);

//  Intent intent = new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH);
//  intent.putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL, RecognizerIntent.LANGUAGE_MODEL_FREE_FORM);
//  intent.putExtra(RecognizerIntent.EXTRA_PROMPT, "Voice recognition Demo...");
//  startActivityForResult(intent, REQUEST_CODE);

//protected void onActivityResult(int requestCode, int resultCode, Intent data)
//{
//    if (requestCode == REQUEST_CODE && resultCode == RESULT_OK)
//    {
//        // Populate the wordsList with the String values the recognition engine thought it heard
//        ArrayList<String> matches = data.getStringArrayListExtra(RecognizerIntent.EXTRA_RESULTS);
//    }
//}

uses Androidapi.JNI.JavaTypes, FMX.Helpers.Android
{$IF CompilerVersion >= 27.0}
, Androidapi.Helpers
{$ENDIF}
;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SpeakOut;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  tts := TJTextToSpeech.JavaClass.init(SharedActivityContext, ttsListener);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  b : Boolean;

begin
  b := TJSpeechRecognizer.JavaClass.isRecognitionAvailable(SharedActivityContext);
  if b then
  begin
    SpeechRecognizer := TJSpeechRecognizer.JavaClass.createSpeechRecognizer(SharedActivityContext);
    RecListener := TRecognitionListener.Create(self);
    RecognizerIntent := TJRecognizerIntent.JavaClass.getVoiceDetailsIntent(SharedActivityContext);
    RecognizerIntent.putExtra(TJRecognizerIntent.JavaClass.EXTRA_LANGUAGE_MODEL, StringToJString('en-US'));
    SpeechRecognizer.setRecognitionListener(RecListener);
    Button3.Enabled := false;
    Button4.Enabled := true;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Button4.Text = 'Listen' then
    SpeechRecognizer.startListening(RecognizerIntent)
  else
    SpeechRecognizer.stopListening;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  ttsListener := TttsOnInitListener.Create(self);
end;

destructor TForm1.Destroy;
begin
  if Assigned(tts) then
  begin
    tts.stop;
    tts.shutdown;
    tts := nil;
  end;
  ttsListener := nil;
  inherited;
end;

procedure TForm1.SpeakOut;
var
  text : JString;
begin
  text := StringToJString(Memo1.Lines.Text);
  tts.speak(text, TJTextToSpeech.JavaClass.QUEUE_FLUSH, nil);
end;

{ TForm1.TttsOnInitListener }

constructor TForm1.TttsOnInitListener.Create(AParent: TForm1);
begin
  inherited Create;
  FParent := AParent
end;

procedure TForm1.TttsOnInitListener.onInit(status: Integer);
var
  Result : Integer;
begin
  if (status = TJTextToSpeech.JavaClass.SUCCESS) then
  begin
   result := FParent.tts.setLanguage(TJLocale.JavaClass.US);
   if (result = TJTextToSpeech.JavaClass.LANG_MISSING_DATA) or
      (result = TJTextToSpeech.JavaClass.LANG_NOT_SUPPORTED) then
     ShowMessage('This Language is not supported')
   else
   begin
     FParent.Button1.Enabled := true;
     FParent.button2.Enabled := false;
   end;
  end
  else
    ShowMessage('Initilization Failed!');
end;

{ TForm1.TRecognitionListener }

constructor TForm1.TRecognitionListener.Create(AParent: TForm1);
begin
  inherited Create;
  FParent := AParent;
end;

procedure TForm1.TRecognitionListener.onBeginningOfSpeech;
begin

end;

procedure TForm1.TRecognitionListener.onBufferReceived(
  buffer: TJavaArray<byte>);
var
  tmp : JString;
begin
  tmp := TJString.JavaClass.init(buffer);
  FParent.Memo1.Lines.Add(JStringToString(tmp));
end;

procedure TForm1.TRecognitionListener.onEndOfSpeech;
begin
  FParent.Button4.Text := 'Listen';
end;

procedure TForm1.TRecognitionListener.onError(error: Integer);
begin

end;

procedure TForm1.TRecognitionListener.onEvent(eventType: Integer;
  params: JBundle);
begin

end;

procedure TForm1.TRecognitionListener.onPartialResults(partialResults: JBundle);
begin

end;

procedure TForm1.TRecognitionListener.onReadyForSpeech(params: JBundle);
begin
  FParent.Button4.Text := 'Stop';
end;

procedure TForm1.TRecognitionListener.onResults(results: JBundle);
begin

end;

procedure TForm1.TRecognitionListener.onRmsChanged(rmsdB: Single);
begin

end;

end.
