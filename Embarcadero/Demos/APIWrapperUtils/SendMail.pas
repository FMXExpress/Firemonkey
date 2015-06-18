unit SendMail;

interface

uses
{$IFDEF IOS}
  Macapi.ObjectiveC, Macapi.CoreFoundation, Macapi.Dispatch,
  iOSApi.CocoaTypes, iOSApi.Foundation, Posix.SysSocket;
{$ENDIF}
{$IFDEF ANDROID}
  FMX.Helpers.Android,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os;
{$ENDIF}

procedure CreateEmail(const Recipient, Subject, Content, FileToAttach: string);

implementation

procedure CreateEmail(const Recipient, Subject, Content, FileToAttach: string);
{$IFDEF ANDROID}
var
  Uri: Jnet_Uri;
  Intent: JIntent;
  Recipients: TJavaObjectArray<JString>;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Intent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);

  Recipients := TJavaObjectArray<JString>.Create(1);
  Recipients.Items[0] := StringToJString(Recipient);

  Intent.putExtra(TJIntent.JavaClass.EXTRA_EMAIL, Recipients);
  Intent.putExtra(TJIntent.JavaClass.EXTRA_SUBJECT, StringToJString(Subject));
  Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(Content));
  Intent.setType(StringToJString('plain/text'));

  SharedActivity.startActivity(TJIntent.JavaClass.createChooser(Intent,
    StrToJCharSequence('Which email app?')));

  if FileToAttach <> '' then
   begin
     Uri := TJnet_Uri.JavaClass.parse(StringToJString(FileToAttach));
     Intent.putExtra(TJIntent.JavaClass.EXTRA_STREAM,
        TJParcelable.Wrap((Uri as ILocalObject).GetObjectID));
   end;
{$ENDIF}
end;

end.
