unit Xplat.Services;

interface

uses
  System.Classes, System.IniFiles;

type
  IPleaseWaitService = interface
  ['{86D7671B-A5A0-4ADB-A4F9-E609A8246DC1}']
    procedure StartWait;
    procedure StopWait;
  end;

  IIniFileService = interface(IInterface)
    ['{6024D9FD-3B3B-4B5A-B767-D87993B2E32D}']
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Integer): Integer;
    procedure WriteInteger(const Section, Ident: string; Value: Integer);
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime);
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime);
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime);
    function ReadFloat(const Section, Ident: string; Default: Double): Double;
    procedure WriteFloat(const Section, Ident: string; Value: Double);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
  end;

implementation

uses
  FMX.Platform
{$IFDEF MACOS}
  //Services common to Apple Mac and iOS platforms
  , Apple.IniFiles
  {$IFDEF IOS}
  //iOS specific service implementations
  , iOS.Services
  {$ELSE}
  //Mac specific service implementations
  , Mac.Services
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
//Android specific service implementation
  , Android.Services
{$ENDIF}

{$IFDEF MSWINDOWS}
//Windows specific service implementations
  , Windows.Services
{$ENDIF}
;

end.
