unit HeaderFooterTemplate;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.MobilePreview, Androidapi.JNI.GraphicsContentViewText, FMX.Helpers.Android,
  Androidapi.JNI.JavaTypes, FMX.Platform.Android, AndroidApi.JniBridge, AndroidApi.Jni.App,
  AndroidAPI.jni.OS;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

{$R *.fmx}

procedure THeaderFooterForm.Button1Click(Sender: TObject);
{$IFDEF ANDROID}
var
  ShortcutIntent: JIntent;
  addIntent: JIntent;
  wIconIdentifier : integer;
  wIconResource : JIntent_ShortcutIconResource;
{$ENDIF}
begin
{$IFDEF ANDROID}

  ShortcutIntent := TJIntent.JavaClass.init(SharedActivityContext, SharedActivityContext.getClass);
  ShortcutIntent.setAction(TJIntent.JavaClass.ACTION_MAIN);

  addIntent := TJIntent.Create;
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_INTENT, TJParcelable.Wrap((shortcutIntent as ILocalObject).GetObjectID));// here we need to cast the intent as it's not done in delphi by default, not like java
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_NAME, StringToJString(Application.Title));
  addIntent.setAction(StringToJString('com.android.launcher.action.INSTALL_SHORTCUT'));
  // get icon resource identifier
  wIconIdentifier := SharedActivity.getResources.getIdentifier(StringToJString('ic_launcher'), StringToJString('drawable'), StringToJString('com.embarcadero.HeaderFooterApplication')); // if the app name change, you must change the package name
  wIconResource := TJIntent_ShortcutIconResource.JavaClass.fromContext(SharedActivityContext, wIconIdentifier);
  // set icon for shortcut
  addIntent.putExtra(TJIntent.JavaClass.EXTRA_SHORTCUT_ICON_RESOURCE, TJParcelable.Wrap((wIconResource as ILocalObject).GetObjectID));

  SharedActivityContext.sendBroadcast(addIntent);


{$ENDIF}

end;




end.
