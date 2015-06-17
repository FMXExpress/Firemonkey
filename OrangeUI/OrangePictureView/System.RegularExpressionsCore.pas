{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2015 Embarcadero Technologies, Inc. }
{                                                       }
{ Original Author: Jan Goyvaerts                        }
{                                                       }
{*******************************************************}

unit System.RegularExpressionsCore;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.RegularExpressionsAPI;

{$IFDEF POSIX}
  {$IF not defined(CPUARM)}
    {$DEFINE DYNAMIC_LIB}
  {$ENDIF}
{$ENDIF}

type
  TPerlRegExOptions = set of (
    preCaseLess,       // /i -> Case insensitive
    preMultiLine,      // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string
    preSingleLine,     // /s -> Dot matches any character, including \n (newline). Otherwise, it matches anything except \n
    preExtended,       // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out
    preAnchored,       // /A -> Successful match can only occur at the start of the subject or right after the previous match
    preUnGreedy,       // Repeat operators (+, *, ?) are not greedy by default (i.e. they try to match the minimum number of characters instead of the maximum)
    preNoAutoCapture   // (group) is a non-capturing group; only named groups capture
  );

type
  TPerlRegExState = set of (
    preNotBOL,         // Not Beginning Of Line: ^ does not match at the start of Subject
    preNotEOL,         // Not End Of Line: $ does not match at the end of Subject
    preNotEmpty        // Empty matches not allowed
  );

const
  // Maximum number of subexpressions (backreferences)
  // Subexpressions are created by placing round brackets in the regex, and are referenced by \1, \2, ...
  // In Perl, they are available as $1, $2, ... after the regex matched; with TPerlRegEx, use the Subexpressions property
  // You can also insert \1, \2, ... in the replacement string; \0 is the complete matched expression
  MAX_SUBEXPRESSIONS = 99;

// All implicit string casts have been verified to be correct
{. $WARN IMPLICIT_STRING_CAST OFF}

type
  TPerlRegExReplaceEvent = procedure(Sender: TObject; var ReplaceWith: string) of object;

type
  TPerlRegEx = class
  private    // *** Property storage, getters and setters
    FCompiled, FStudied: Boolean;
    FOptions: TPerlRegExOptions;
    FState: TPerlRegExState;
    FRegEx: string;
    FReplacement: TBytes;
    FSubject: TBytes;
    FStart, FStop: Integer;
    FOnMatch: TNotifyEvent;
    FOnReplace: TPerlRegExReplaceEvent;
    function GetMatchedText: string;
    function GetMatchedLength: Integer;
    function GetMatchedOffset: Integer;
    function InternalGetMatchedOffset: Integer;
    function InternalGetMatchedLength: Integer;
    procedure SetOptions(Value: TPerlRegExOptions);
    procedure SetRegEx(const Value: string);
    function GetGroupCount: Integer;
    function GetGroups(Index: Integer): string;
    function GetGroupLengths(Index: Integer): Integer;
    function GetGroupOffsets(Index: Integer): Integer;
    function InternalGetGroupLengths(Index: Integer): Integer;
    function InternalGetGroupOffsets(Index: Integer): Integer;
    procedure SetFSubject(const Value: TBytes);
    procedure SetSubject(const Value: string);
    function GetSubject: string;
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetFoundMatch: Boolean;
  private    // *** Variables used by PCRE
    Offsets: array[0..(MAX_SUBEXPRESSIONS+1)*3] of Integer;
    OffsetCount: Integer;
    FPCREOptions: Integer;
    FPattern: Pointer;
    FHints: Pointer;
    FCharTable: Pointer;
    FHasStoredGroups: Boolean;
    FStoredGroups: array of string;
    function GetSubjectLeft: string;
    function GetSubjectRight: string;
    procedure SetReplacement(const Value: string);
    function GetReplacement: string;
    function GetStart: Integer;
{$IFDEF DYNAMIC_LIB}
    class constructor Create;
    class destructor Destroy;
{$ENDIF POSIX}
  protected
    procedure CleanUp;
        // Dispose off whatever we created, so we can start over. Called automatically when needed, so it is not made public
    procedure ClearStoredGroups;
  public
    constructor Create;
        // Come to life
    destructor Destroy; override;
        // Clean up after ourselves
    class function EscapeRegExChars(const S: string): string;
        // Escapes regex characters in S so that the regex engine can be used to match S as plain text
    procedure Compile;
        // Compile the regex. Called automatically by Match
    procedure Study;
        // Study the regex. Studying takes time, but will make the execution of the regex a lot faster.
        // Call study if you will be using the same regex many times
    function Match: Boolean;
        // Attempt to match the regex, starting the attempt from the beginning of Subject
    function MatchAgain: Boolean;
        // Attempt to match the regex to the remainder of Subject after the previous match (as indicated by Start)
    function Replace: string;
        // Replace matched expression in Subject with ComputeReplacement.  Returns the actual replacement text from ComputeReplacement
    function ReplaceAll: Boolean;
        // Repeat MatchAgain and Replace until you drop.  Returns True if anything was replaced at all.
    function ComputeReplacement: string;
        // Returns Replacement with backreferences filled in
    procedure StoreGroups;
        // Stores duplicates of Groups[] so they and ComputeReplacement will still return the proper strings
        // even if FSubject is changed or cleared
    function NamedGroup(const Name: string): Integer;
        // Returns the index of the named group Name
    procedure Split(Strings: TStrings; Limit: Integer);
        // Split Subject along regex matches.  Capturing groups are ignored.
    procedure SplitCapture(Strings: TStrings; Limit: Integer); overload;
    procedure SplitCapture(Strings: TStrings; Limit: Integer; Offset : Integer); overload;
        // Split Subject along regex matches.  Capturing groups are added to Strings as well.
    property Compiled: Boolean read FCompiled;
        // True if the RegEx has already been compiled.
    property FoundMatch: Boolean read GetFoundMatch;
        // Returns True when Matched* and Group* indicate a match
    property Studied: Boolean read FStudied;
        // True if the RegEx has already been studied
    property MatchedText: string read GetMatchedText;
        // The matched text
    property MatchedLength: Integer read GetMatchedLength;
        // Length of the matched text
    property MatchedOffset: Integer read GetMatchedOffset;
        // Character offset in the Subject string at which MatchedText starts
    property Start: Integer read GetStart write SetStart;
        // Starting position in Subject from which MatchAgain begins
    property Stop: Integer read FStop write SetStop;
        // Last character in Subject that Match and MatchAgain search through
    property State: TPerlRegExState read FState write FState;
        // State of Subject
    property GroupCount: Integer read GetGroupCount;
        // Number of matched capturing groups
    property Groups[Index: Integer]: string read GetGroups;
        // Text matched by capturing groups
    property GroupLengths[Index: Integer]: Integer read GetGroupLengths;
        // Lengths of the text matched by capturing groups
    property GroupOffsets[Index: Integer]: Integer read GetGroupOffsets;
        // Character offsets in Subject at which the capturing group matches start
    property Subject: string read GetSubject write SetSubject;
        // The string on which Match() will try to match RegEx
    property SubjectLeft: string read GetSubjectLeft;
        // Part of the subject to the left of the match
    property SubjectRight: string read GetSubjectRight;
        // Part of the subject to the right of the match
  public
    property Options: TPerlRegExOptions read FOptions write SetOptions;
        // Options
    property RegEx: string read FRegEx write SetRegEx;
        // The regular expression to be matched
    property Replacement: string read GetReplacement write SetReplacement;
        // Text to replace matched expression with. \number and $number backreferences will be substituted with Groups
        // TPerlRegEx supports the "JGsoft" replacement text flavor as explained at http://www.regular-expressions.info/refreplace.html
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
        // Triggered by Match and MatchAgain after a successful match
    property OnReplace: TPerlRegExReplaceEvent read FOnReplace write FOnReplace;
        // Triggered by Replace and ReplaceAll just before the replacement is done, allowing you to determine the new string
  end;

{
  You can add TPerlRegEx instances to a TPerlRegExList to match them all together on the same subject,
  as if they were one regex regex1|regex2|regex3|...
  TPerlRegExList does not own the TPerlRegEx components, just like a TList
  If a TPerlRegEx has been added to a TPerlRegExList, it should not be used in any other situation
  until it is removed from the list
}

type
  TPerlRegExList = class
  private
    FList: TList<TPerlRegEx>;
    FSubject: TBytes;
    FMatchedRegEx: TPerlRegEx;
    FStart, FStop: Integer;
    function GetRegEx(Index: Integer): TPerlRegEx;
    procedure SetRegEx(Index: Integer; Value: TPerlRegEx);
    procedure SetSubject(const Value: string);
    function GetSubject: string;
    procedure SetStart(const Value: Integer);
    procedure SetStop(const Value: Integer);
    function GetCount: Integer;
    function GetStart: Integer;
    function GetStop: Integer;
  protected
    procedure UpdateRegEx(ARegEx: TPerlRegEx);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function Add(ARegEx: TPerlRegEx): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    function IndexOf(ARegEx: TPerlRegEx): Integer;
    procedure Insert(Index: Integer; ARegEx: TPerlRegEx);
  public
    function Match: Boolean;
    function MatchAgain: Boolean;
    property RegEx[Index: Integer]: TPerlRegEx read GetRegEx write SetRegEx;
    property Count: Integer read GetCount;
    property Subject: string read GetSubject write SetSubject;
    property Start: Integer read GetStart write SetStart;
    property Stop: Integer read GetStop write SetStop;
    property MatchedRegEx: TPerlRegEx read FMatchedRegEx;
  end;

  ERegularExpressionError = class(Exception);

function UnicodeIndexToUTF8(const S: string; AIndex: Integer): Integer;
function UTF8IndexToUnicode(const S: TBytes; AIndex: Integer): Integer;

implementation

uses
  System.RegularExpressionsConsts;

{ ********* Unit support routines ********* }

function UnicodeIndexToUTF8(const S: string; AIndex: Integer): Integer;
var
  I: Integer;
begin
  if AIndex > S.Length + 1 then
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [AIndex]);

  Result := 0;
  I := 0;
  while I < (AIndex - 1) do
  begin
    if S.Chars[I] <= #$007F then
      Inc(Result)
    else if S.Chars[I] <= #$7FF then
      Inc(Result, 2)
    else if IsLeadChar(S.Chars[I]) then
    begin
      Inc(I);
      Inc(Result, 4);
    end
    else
      Inc(Result, 3);
    Inc(I);
  end;
end;

function UTF8IndexToUnicode(const S: TBytes; AIndex: Integer): Integer;
var
  I: Integer;
begin
  if AIndex > Length(S) then
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [AIndex]);

  Result := 0;
  I := 0;
  while I < AIndex do
  begin
    Inc(Result);
    case S[I] of
      $00 .. $BF, $F8 .. $FF:
        Inc(I);
      $C0 .. $DF:
        Inc(I, 2);
      $E0 .. $EF:
        Inc(I, 3);
      $F0 .. $F7:
        begin
          Inc(I, 4);
          Inc(Result)
        end;
    end;
  end;
end;


function FirstCap(const S: string): string;
begin
  if S = '' then
    Result := ''
  else
  begin
    Result := UpperCase(S.Substring(0,1))+AnsiLowerCase(S.Substring(1,S.Length));
  end
end;

function InitialCaps(const S: string): string;
var
  I, J: Integer;
  Up: Boolean;
  Tmp: TCharArray;
begin
  Result := AnsiLowerCase(S);
  SetLength(Tmp, Result.Length);

  Up := True;
  J := 0;
  for I := Low(Result) to High(Result) do
  begin
    Tmp[J] := Result[I];
    case Result[I] of
      #0..'&', '(', '*', '+', ',', '-', '.', '?', '<', '[', '{', #$00B7:
        Up := True
      else
        if Up and (Result[I] <> '''') then
        begin
          Tmp[J] := UpperCase(Result[I], loUserLocale)[Low(string)];
          Up := False
        end
    end;
    Inc(J);
  end;
  Result := string.Create(Tmp, 0, J);
end;

function CopyBytes(const S: TBytes; Index, Count: Integer): TBytes;
var
  Len: Integer;
begin
  SetLength(Result, 0);
  Len := Length(S);
  if Len > 0 then
  begin
    if Index < 0 then Index := 0
    else if Index > Len then Count := 0;
    Len := Len - Index;
    if (Count > 0) and (Len > 0) then
    begin
      if Count > Len then Count := Len;
      SetLength(Result, Count);
      Move(S[Index], Result[0], Count);
    end;
  end;
end;

procedure InsertBytes(const Source: TBytes; var S: TBytes; Index: Integer);
var
  Len: Integer;
  Len1, Len2: Integer;
begin
  Len := Length(S);
  if Index < 0 then Index := 0
  else if Index > Len  then Index := Len ;

  Len1 := Length(Source);
  Len2 := Len - Index;

  SetLength(S, Index + Len1 + Len2);
  if Len1 > 0 then
  begin
    if Len2 > 0 then
      Move(S[Index], S[Index + Len1], Len2);
    Move(Source[0], S[Index], Len1);
  end;
end;

procedure DeleteBytes(var S: TBytes; Index, Count: Integer);
var
  Len, TailLen: Integer;
begin
  Len := Length(S);
  if (Index >= 0) and (Index < Len) then
  begin
    if Count > 0 then
    begin
      TailLen := Len - Index ;
      if Count > TailLen then Count := TailLen;
      if TailLen <> Count then Move(S[Index+Count], S[Index], TailLen - Count);
      SetLength(S, Len - Count);
    end;
  end;
end;

{ ********* TPerlRegEx ********* }

procedure TPerlRegEx.CleanUp;
begin
  FCompiled := False;
  FStudied := False;
  pcre_dispose(FPattern, FHints, nil);
  FPattern := nil;
  FHints := nil;
  ClearStoredGroups;
  OffsetCount := 0;
end;

procedure TPerlRegEx.ClearStoredGroups;
begin
  FHasStoredGroups := False;
  FStoredGroups := nil;
end;

procedure TPerlRegEx.Compile;
var
  Error: MarshaledAString;
  ErrorOffset: Integer;
  M: TMarshaller;
begin
  if FRegEx = '' then
    raise ERegularExpressionError.CreateRes(@SRegExMissingExpression);
  CleanUp;
  FPattern := pcre_compile(M.AsUTF8(FRegEx).ToPointer, FPCREOptions, @Error, @ErrorOffset, FCharTable);
  if FPattern = nil then
    raise ERegularExpressionError.CreateResFmt(@SRegExExpressionError, [ErrorOffset, string(Error)]);
  FCompiled := True
end;

(* Backreference overview:

Assume there are 13 backreferences:

Text        TPerlRegex    .NET      Java       ECMAScript
$17         $1 + "7"      "$17"     $1 + "7"   $1 + "7"
$017        $1 + "7"      "$017"    $1 + "7"   $1 + "7"
$12         $12           $12       $12        $12
$012        $1 + "2"      $12       $12        $1 + "2"
${1}2       $1 + "2"      $1 + "2"  error      "${1}2"
$$          "$"           "$"       error      "$"
\$          "$"           "\$"      "$"        "\$"
*)

function TPerlRegEx.ComputeReplacement: string;
var
  Mode: Byte;
  S, BR: TBytes;
  I, J, N: Integer;

  procedure ReplaceBackreference(Number: Integer);
  var
    Backreference: string;
  begin
    DeleteBytes(S, I, J-I);
    if Number <= GroupCount then
    begin
      Backreference := Groups[Number];
      if Backreference <> '' then
      begin
        // Ignore warnings; converting to UTF-8 does not cause data loss
        case Char(Mode) of
          'L', 'l': Backreference := AnsiLowerCase(Backreference);
          'U', 'u': Backreference := AnsiUpperCase(Backreference);
          'F', 'f': Backreference := FirstCap(Backreference);
          'I', 'i': Backreference := InitialCaps(Backreference);
        end;
        if Length(S) <> 0 then
        begin
          BR := TEncoding.UTF8.GetBytes(Backreference);
          InsertBytes(BR, S, I);
          I := I + Length(BR);
        end
        else
        begin
          S := TEncoding.UTF8.GetBytes(Backreference);
          I := MaxInt;
        end
      end;
    end
  end;

  procedure ProcessBackreference(NumberOnly, Dollar: Boolean);
  var
    Number, Number2: Integer;
  begin
    Number := -1;
    if (J < Length(S)) and (Ord(S[J]) in [Ord('0')..Ord('9')]) then
    begin
      // Get the number of the backreference
      Number := Ord(S[J]) - Ord('0');
      Inc(J);
      if (J < Length(S)) and (Ord(S[J]) in [Ord('0')..Ord('9')]) then
      begin
        // Expand it to two digits only if that would lead to a valid backreference
        Number2 := Number*10 + Ord(S[J]) - Ord('0');
        if Number2 <= GroupCount then
        begin
          Number := Number2;
          Inc(J)
        end;
      end;
    end
    else if not NumberOnly then
    begin
      if Dollar and (J < Length(S) - 1) and (S[J] = Ord('{')) then
      begin
        // Number or name in curly braces
        Inc(J);
        case Char(S[J]) of
          '0'..'9':
            begin
              Number := Ord(S[J]) - Ord('0');
              Inc(J);
              while (J < Length(S)) and (Ord(S[J]) in [Ord('0')..Ord('9')]) do
              begin
                Number := Number*10 + Ord(S[J]) - Ord('0');
                Inc(J)
              end;
            end;
          'A'..'Z', 'a'..'z', '_':
            begin
              Inc(J);
              while (J < Length(S)) and (Ord(S[J]) in [Ord('0')..Ord('9'), Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_')]) do
                Inc(J);
              if (J < Length(S)) and (S[J] = Ord('}')) then
              begin
                Number := NamedGroup(TEncoding.UTF8.GetString(CopyBytes(S, I+2, J-I-2)));
              end
            end;
        end;
        if (J >= Length(S)) or (S[J] <> Ord('}')) then
          Number := -1
        else
          Inc(J);
      end
      else if Dollar and (S[J] = Ord('_')) then
      begin
        // $_ (whole subject)
        DeleteBytes(S, I, J+1-I);
        InsertBytes(FSubject, S, I);
        I := I + Length(FSubject);
        Exit;
      end
      else
      case Char(S[J]) of
        '&':
          begin
            // \& or $& (whole regex match)
            Number := 0;
            Inc(J);
          end;
        '+':
          begin
            // \+ or $+ (highest-numbered participating group)
            Number := GroupCount;
            Inc(J);
          end;
        '`':
          begin
            // \` or $` (backtick; subject to the left of the match)
            DeleteBytes(S, I, J+1-I);
            // Insert SubjectLeft
            InsertBytes(CopyBytes(FSubject, 0, Offsets[0]), S, I);
            I := I + Offsets[0];
            Exit;
          end;
        '''':
          begin
            // \' or $' (straight quote; subject to the right of the match)
            DeleteBytes(S, I, J+1-I);
            // Insert SubjectRight
            InsertBytes(CopyBytes(FSubject, Offsets[1], MaxInt), S, I);
            I := I + Length(FSubject) - Offsets[1];
            Exit;
          end
      end;
    end;
    if Number >= 0 then
      ReplaceBackreference(Number)
    else
      Inc(I)
  end;

begin
  if Length(FReplacement) = 0 then Exit('');
  SetLength(S, Length(FReplacement));
  Move(FReplacement[0], S[0], Length(FReplacement));
  I := 0;
  while I < Length(S)-1 do
  begin
    case Char(S[I]) of
      '\':
        begin
          J := I + 1;
          // We let I stop one character before the end, so J cannot point
          // beyond the end of the UTF8String here
          if J >= Length(S) then
            raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [J]);
          case Char(S[J]) of
            '$', '\':
              begin
                DeleteBytes(S, I, 1);
                Inc(I);
              end;
            'g':
              begin
                if (J < Length(S)-2) and (S[J+1] = Ord('<')) and (Ord(S[J+2]) in [Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_')]) then
                begin
                  // Python-style named group reference \g<name>
                  J := J+3;
                  while (J <= Length(S)-1) and (Ord(S[J]) in [Ord('0')..Ord('9'), Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_')]) do
                    Inc(J);
                  if (J <= Length(S)-1) and (S[J] = Ord('>')) then
                  begin
                    N := NamedGroup(TEncoding.UTF8.GetString(CopyBytes(S, I+3, J-I-3)));
                    Inc(J);
                    Mode := Byte(#0);
                    if N > 0 then
                      ReplaceBackreference(N)
                    else
                      DeleteBytes(S, I, J-I);
                  end
                  else
                    I := J
                end
                else
                  I := I+2;
              end;
            'l', 'L', 'u', 'U', 'f', 'F', 'i', 'I':
              begin
                Mode := S[J];
                Inc(J);
                ProcessBackreference(True, False);
              end;
          else
            Mode := Byte(#0);
            ProcessBackreference(False, False);
          end;
        end;
      '$':
        begin
          J := I + 1;
          // We let I stop one character before the end, so J cannot point
          // beyond the end of the UTF8String here
          if J >= Length(S) then
            raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [J]);
          if S[J] = Ord('$') then
          begin
            DeleteBytes(S, J, 1);
            Inc(I);
          end
          else
          begin
            Mode := Byte(#0);
            ProcessBackreference(False, True);
          end
        end;
    else
      Inc(I);
    end
  end;
  Result := TEncoding.UTF8.GetString(S);
end;

constructor TPerlRegEx.Create;
begin
  inherited Create;
  FState := [];
  FCharTable := pcre_maketables;
  FPCREOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
end;

destructor TPerlRegEx.Destroy;
begin
  pcre_dispose(FPattern, FHints, FCharTable);
  inherited Destroy;
end;

{$IFDEF DYNAMIC_LIB}
class constructor TPerlRegEx.Create;
begin
  if not LoadPCRELib then
//    RaiseLastOSError
    ;
end;

class destructor TPerlRegEx.Destroy;
begin
  UnloadPCRELib;
end;
{$ENDIF DYNAMIC_LIB}

class function TPerlRegEx.EscapeRegExChars(const S: string): string;
var
  I, J: Integer;
  Tmp: TCharArray;
begin
  SetLength(Tmp, S.Length * 2);
  J := 0;
  for I := Low(S) to High(S) do
  begin
    case S[I] of
      '\', '[', ']', '^', '$', '.', '|', '?', '*', '+', '-', '(', ')', '{', '}', '&', '<', '>':
        begin
          Tmp[J] := '\';
          Inc(j);
          Tmp[J] := S[I];
        end;
      #0:
        begin
          Tmp[J] := '\';
          Inc(j);
          Tmp[J] := '0';
        end;
      else
        Tmp[J] := S[I];
    end;
    Inc(J);
  end;
  Result := string.Create(Tmp, 0, J);
end;

function TPerlRegEx.GetFoundMatch: Boolean;
begin
  Result := OffsetCount > 0;
end;

function TPerlRegEx.GetMatchedText: string;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroups(0);
end;

function TPerlRegEx.GetReplacement: string;
begin
  Result := TEncoding.UTF8.GetString(FReplacement);
end;

function TPerlRegEx.GetMatchedLength: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroupLengths(0)
end;

function TPerlRegEx.InternalGetMatchedLength: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := InternalGetGroupLengths(0)
end;

function TPerlRegEx.InternalGetMatchedOffset: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := InternalGetGroupOffsets(0);
end;

function TPerlRegEx.GetMatchedOffset: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := GetGroupOffsets(0);
end;

function TPerlRegEx.GetGroupCount: Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  Result := OffsetCount-1
end;

function TPerlRegEx.GetGroupLengths(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := UTF8IndexToUnicode(FSubject, Offsets[Index*2+1]) - UTF8IndexToUnicode(FSubject, Offsets[Index*2])
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

function TPerlRegEx.GetGroupOffsets(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := UTF8IndexToUnicode(FSubject, Offsets[Index*2]) + 1
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

function TPerlRegEx.InternalGetGroupLengths(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := Offsets[Index*2+1]-Offsets[Index*2]
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

function TPerlRegEx.InternalGetGroupOffsets(Index: Integer): Integer;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);

  if (Index >= 0) and (Index <= GroupCount) then
    Result := Offsets[Index*2]
  else
    raise ERegularExpressionError.CreateResFmt(@SRegExIndexOutOfBounds, [Index]);
end;

function TPerlRegEx.GetGroups(Index: Integer): string;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  if Index > GroupCount then
    Result := ''
  else if FHasStoredGroups then
    Result := FStoredGroups[Index]
  else
    Result := TEncoding.UTF8.GetString(CopyBytes(FSubject, Offsets[Index*2], Offsets[Index*2+1]-Offsets[Index*2]));
end;

function TPerlRegEx.GetStart: Integer;
begin
  Result := FStart + 1;
end;

function TPerlRegEx.GetSubject: string;
begin
  Result := TEncoding.UTF8.GetString(FSubject);
end;

function TPerlRegEx.GetSubjectLeft: String;
begin
  Result := TEncoding.UTF8.GetString(CopyBytes(FSubject, 0, Offsets[0]));
end;

function TPerlRegEx.GetSubjectRight: string;
begin
  Result := TEncoding.UTF8.GetString(CopyBytes(FSubject, Offsets[1], MaxInt));
end;

function TPerlRegEx.Match: Boolean;
var
  Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then
    Compile;
  if preNotBOL in State then
    Opts := PCRE_NOTBOL
  else
    Opts := 0;
  if preNotEOL in State then
    Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then
    Opts := Opts or PCRE_NOTEMPTY;
  Opts := Opts or PCRE_NO_UTF8_CHECK;
  if Length(FSubject) = 0 then
    OffsetCount := -1
  else
    OffsetCount := pcre_exec(FPattern, FHints, @FSubject[0], FStop, 0, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into TBytes indices
  if Result then
  begin
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then
      Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then
      OnMatch(Self)
  end;
end;

function TPerlRegEx.MatchAgain: Boolean;
var
  Opts: Integer;
begin
  ClearStoredGroups;
  if not Compiled then
    Compile;
  if preNotBOL in State then
    Opts := PCRE_NOTBOL
  else
    Opts := 0;
  if preNotEOL in State then
    Opts := Opts or PCRE_NOTEOL;
  if preNotEmpty in State then
    Opts := Opts or PCRE_NOTEMPTY;
  Opts := Opts or PCRE_NO_UTF8_CHECK;
  if (Length(FSubject) = 0) or (FStart >= FStop) then
    OffsetCount := -1
  else
    OffsetCount := pcre_exec(FPattern, FHints, @FSubject[0], FStop, FStart, Opts, @Offsets[0], High(Offsets));
  Result := OffsetCount > 0;
  // Convert offsets into TBytes indices
  if Result then
  begin
//    for I := 0 to OffsetCount*2-1 do
//      Inc(Offsets[I]);
    FStart := Offsets[1];
    if Offsets[0] = Offsets[1] then
      Inc(FStart); // Make sure we don't get stuck at the same position
    if Assigned(OnMatch) then
      OnMatch(Self)
  end;
end;

function TPerlRegEx.NamedGroup(const Name: string): Integer;
var
  M: TMarshaller;
begin
  Result := pcre_get_stringnumber(FPattern, M.AsUtf8(Name).ToPointer);
end;

function TPerlRegEx.Replace: string;
var
  Tmp: TBytes;
begin
  if not FoundMatch then
    raise ERegularExpressionError.CreateRes(@SRegExMatchRequired);
  // Substitute backreferences
  Result := ComputeReplacement;
  // Allow for just-in-time substitution determination
  if Assigned(OnReplace) then
    OnReplace(Self, Result);
  Tmp := TEncoding.UTF8.GetBytes(Result);
  // Perform substitution
  DeleteBytes(FSubject, InternalGetMatchedOffset, InternalGetMatchedLength);
  if Result <> '' then
    InsertBytes(Tmp, FSubject, InternalGetMatchedOffset);
  // Position to continue search
  FStart := FStart - InternalGetMatchedLength +  Length(Tmp);
  FStop := FStop - InternalGetMatchedLength + Length(Tmp);
  // Replacement no longer matches regex, we assume
  ClearStoredGroups;
  OffsetCount := 0;
end;

function TPerlRegEx.ReplaceAll: Boolean;
begin
  if Match then      
  begin
    Result := True;         
    repeat
      Replace
    until not MatchAgain;
  end
  else
    Result := False;
end;

procedure TPerlRegEx.SetOptions(Value: TPerlRegExOptions);
begin
  if (FOptions <> Value) then
  begin
    FOptions := Value;
    FPCREOptions := PCRE_UTF8 or PCRE_NEWLINE_ANY;
    if (preCaseLess in Value) then
      FPCREOptions := FPCREOptions or PCRE_CASELESS;
    if (preMultiLine in Value) then
      FPCREOptions := FPCREOptions or PCRE_MULTILINE;
    if (preSingleLine in Value) then
      FPCREOptions := FPCREOptions or PCRE_DOTALL;
    if (preExtended in Value) then
      FPCREOptions := FPCREOptions or PCRE_EXTENDED;
    if (preAnchored in Value) then
      FPCREOptions := FPCREOptions or PCRE_ANCHORED;
    if (preUnGreedy in Value) then
      FPCREOptions := FPCREOptions or PCRE_UNGREEDY;
    if (preNoAutoCapture in Value) then
      FPCREOptions := FPCREOptions or PCRE_NO_AUTO_CAPTURE;
    CleanUp
  end
end;

procedure TPerlRegEx.SetRegEx(const Value: string);
begin
  if FRegEx <> Value then
  begin
    FRegEx := Value;
    CleanUp
  end
end;

procedure TPerlRegEx.SetReplacement(const Value: string);
begin
  FReplacement := TEncoding.UTF8.GetBytes(Value);
end;

procedure TPerlRegEx.SetStart(const Value: Integer);
begin
  if Value < 1 then
    FStart := 0
  else
    FStart := Value - 1;
  // If FStart >= Length(Subject), MatchAgain() will simply return False
end;

procedure TPerlRegEx.SetStop(const Value: Integer);
begin
  if Value > Length(FSubject) then
    FStop := Length(FSubject)
  else
    FStop := Value;
end;

procedure TPerlRegEx.SetSubject(const Value: string);
begin
  if (Length(FSubject) = 0) or (Value <> TEncoding.UTF8.GetString(FSubject)) then
  begin
    FSubject := TEncoding.UTF8.GetBytes(Value);
    FStart := 0;
    FStop := Length(FSubject);
    if not FHasStoredGroups then
      OffsetCount := 0;
  end;
end;

procedure TPerlRegEx.SetFSubject(const Value: TBytes);
begin
  FSubject := Value;
  FStart := 0;
  FStop := Length(FSubject);
  if not FHasStoredGroups then
    OffsetCount := 0;
end;

procedure TPerlRegEx.Split(Strings: TStrings; Limit: Integer);
var
  Offset, Count: Integer;
begin
  if Strings = nil then
    raise ERegularExpressionError.CreateRes(@SRegExStringsRequired);

  if (Limit = 1) or not Match then
    Strings.Add(Subject)
  else
  begin
    Offset := 0;
    Count := 1;
    repeat
      Strings.Add(TEncoding.UTF8.GetString(CopyBytes(FSubject, Offset, InternalGetMatchedOffset - Offset)));
      Inc(Count);
      Offset := InternalGetMatchedOffset + InternalGetMatchedLength ;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(TEncoding.UTF8.GetString(CopyBytes(FSubject, Offset, MaxInt)));
  end
end;

procedure TPerlRegEx.SplitCapture(Strings: TStrings; Limit, Offset: Integer);
var
  Count: Integer;
  LUseOffset: Boolean;
  LOffset: Integer;
begin
  if Strings = nil then
    raise ERegularExpressionError.CreateRes(@SRegExStringsRequired);

  if (Limit = 1) or not Match then
    Strings.Add(Subject)
  else
  begin
    Dec(Offset); // One based to zero based
    LUseOffset := Offset <> 0;
    if Offset <> 0 then
      Dec(Limit);
    LOffset := 0;
    Count := 1;
    repeat
      if LUseOffset then
      begin
        if InternalGetMatchedOffset >= Offset then
        begin
          LUseOffset := False;
          Strings.Add(TEncoding.UTF8.GetString(CopyBytes(FSubject, 0, InternalGetMatchedOffset)));
          if Self.GroupCount > 0 then
            Strings.Add(Groups[GroupCount]);
        end;
      end
      else
      begin
        Strings.Add(TEncoding.UTF8.GetString(CopyBytes(FSubject, LOffset, InternalGetMatchedOffset - LOffset)));
        Inc(Count);
        if Self.GroupCount > 0 then
          Strings.Add(Groups[GroupCount]);
      end;
      LOffset := InternalGetMatchedOffset + InternalGetMatchedLength ;
    until ((Limit > 1) and (Count >= Limit)) or not MatchAgain;
    Strings.Add(TEncoding.UTF8.GetString(CopyBytes(FSubject, LOffset, MaxInt)));
  end
end;

procedure TPerlRegEx.SplitCapture(Strings: TStrings; Limit: Integer);
begin
  SplitCapture(Strings,Limit,1);
end;

procedure TPerlRegEx.StoreGroups;
var
  I: Integer;
begin
  if OffsetCount > 0 then
  begin
    ClearStoredGroups;
    SetLength(FStoredGroups, GroupCount+1);
    for I := GroupCount downto 0 do
      FStoredGroups[I] := Groups[I];
    FHasStoredGroups := True;
  end
end;

procedure TPerlRegEx.Study;
var
  Error: MarshaledAString;
begin
  if not FCompiled then
    Compile;
  FHints := pcre_study(FPattern, 0, @Error);
  if Error <> nil then
    raise ERegularExpressionError.CreateResFmt(@SRegExStudyError, [string(Error)]);
  FStudied := True
end;

{ TPerlRegExList }

function TPerlRegExList.Add(ARegEx: TPerlRegEx): Integer;
begin
  Result := FList.Add(ARegEx);
  UpdateRegEx(ARegEx);
end;

procedure TPerlRegExList.Clear;
begin
  FList.Clear;
end;

constructor TPerlRegExList.Create;
begin
  inherited Create;
  FList := TList<TPerlRegEx>.Create;
end;

procedure TPerlRegExList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TPerlRegExList.Destroy;
begin
  FList.Free;
  inherited
end;

function TPerlRegExList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TPerlRegExList.GetRegEx(Index: Integer): TPerlRegEx;
begin
  Result := TPerlRegEx(Pointer(FList[Index]));
end;

function TPerlRegExList.GetStart: Integer;
begin
  Result := FStart + 1;
end;

function TPerlRegExList.GetStop: Integer;
begin
  Result := FStop + 1;
end;

function TPerlRegExList.GetSubject: string;
begin
  Result := TEncoding.UTF8.GetString(FSubject);
end;

function TPerlRegExList.IndexOf(ARegEx: TPerlRegEx): Integer;
begin
  Result := FList.IndexOf(ARegEx);
end;

procedure TPerlRegExList.Insert(Index: Integer; ARegEx: TPerlRegEx);
begin
  FList.Insert(Index, ARegEx);
  UpdateRegEx(ARegEx);
end;

function TPerlRegExList.Match: Boolean;
begin
  SetStart(1);
  FMatchedRegEx := nil;
  Result := MatchAgain;
end;

function TPerlRegExList.MatchAgain: Boolean;
var
  I, MatchStart, MatchPos: Integer;
  ARegEx: TPerlRegEx;
begin
  if FMatchedRegEx <> nil then
    MatchStart := FMatchedRegEx.InternalGetMatchedOffset + FMatchedRegEx.InternalGetMatchedLength
  else
    MatchStart := FStart;
  FMatchedRegEx := nil;
  MatchPos := MaxInt;
  for I := 0 to Count-1 do
  begin
    ARegEx := RegEx[I];
    if (not ARegEx.FoundMatch) or (ARegEx.InternalGetMatchedOffset < MatchStart) then
    begin
      ARegEx.FStart := MatchStart;
      ARegEx.MatchAgain;
    end;
    if ARegEx.FoundMatch and (ARegEx.InternalGetMatchedOffset < MatchPos) then
    begin
      MatchPos := ARegEx.InternalGetMatchedOffset;
      FMatchedRegEx := ARegEx;
    end;
    if MatchPos = MatchStart then Break;
  end;
  Result := MatchPos < MaxInt;
end;

procedure TPerlRegExList.SetRegEx(Index: Integer; Value: TPerlRegEx);
begin
  FList[Index] := Value;
  UpdateRegEx(Value);
end;

procedure TPerlRegExList.SetStart(const Value: Integer);
var
  I: Integer;
begin
  if FStart <> (Value - 1) then
  begin
    FStart := Value - 1;
    for I := Count-1 downto 0 do
      RegEx[I].Start := Value;
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.SetStop(const Value: Integer);
var
  I: Integer;
begin
  if FStop <> Value then
  begin
    FStop := Value;
    for I := Count-1 downto 0 do
      RegEx[I].Stop := Value;
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.SetSubject(const Value: string);
var
  I: Integer;
begin
  if TEncoding.UTF8.GetString(FSubject) <> Value then
  begin
    FSubject := TEncoding.UTF8.GetBytes(Value);
    for I := Count-1 downto 0 do
      RegEx[I].SetFSubject(FSubject);
    FMatchedRegEx := nil;
  end;
end;

procedure TPerlRegExList.UpdateRegEx(ARegEx: TPerlRegEx);
begin
  ARegEx.SetFSubject(FSubject);
  ARegEx.FStart := FStart;
end;

end.

