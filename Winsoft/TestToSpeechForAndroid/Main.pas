unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Memo, FMX.StdCtrls, android.speech.tts, Androidapi.JNIBridge;

type
  TFormMain = class(TForm)
    ButtonSay: TButton;
    Memo: TMemo;
    procedure ButtonSayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    type
      TOnInitListener = class(TJavaLocal, JTextToSpeech_OnInitListener)
      private
        [weak] FFormMain: TFormMain;
      public
        constructor Create(FormMain: TFormMain);
        procedure onInit(Status: Integer); cdecl;
      end;
  private
    OnInitListener: TOnInitListener;
    TextToSpeech: JTextToSpeech;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Androidapi.Helpers, FMX.Helpers.Android;

{$R *.fmx}

constructor TFormMain.TOnInitListener.Create(FormMain: TFormMain);
begin
  inherited Create;
  FFormMain := FormMain;
end;

procedure TFormMain.TOnInitListener.onInit(Status: Integer);
begin
  if Status = TJTextToSpeech_SUCCESS then
    FFormMain.ButtonSay.Enabled := True;
end;

procedure TFormMain.ButtonSayClick(Sender: TObject);
begin
  TextToSpeech.speak(StringToJString(Memo.text), TJTextToSpeech_QUEUE_ADD, nil);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  OnInitListener := TOnInitListener.Create(Self);
  TextToSpeech := TJTextToSpeech.JavaClass.init(SharedActivityContext, OnInitListener);
end;

end.
