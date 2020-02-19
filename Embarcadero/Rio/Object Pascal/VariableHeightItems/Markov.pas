unit Markov;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  System.Generics.Collections, System.Character;


type
  TChain = class
  private type
    TPrefix = class
    private
      FPrefix: TArray<string>;
    public
      constructor Create(const Len: Integer);
      function ToString: string; override;
      procedure Shift(const Word: string);
    end;
  private
    FMap: TDictionary<string, TList<string>>;
    FPrefixLen: Integer;
    FPrefix: TPrefix;
  public
    constructor Create(const PrefixLength: Integer);
    destructor Destroy; override;

    procedure Build(const Reader: TStreamReader);
    function Generate(const Count: Integer): string;

    class function FromResource(const ResourceName: string): TChain;
  end;

implementation

{ TPrefix }

constructor TChain.TPrefix.Create(const Len: Integer);
begin
  SetLength(FPrefix, Len);
end;

procedure TChain.TPrefix.Shift(const Word: string);
var
  I: Integer;
begin
  for I := 1 to High(FPrefix) do
    FPrefix[I - 1] := FPrefix[I];
  FPrefix[High(FPrefix)] := Word;
end;

function TChain.TPrefix.ToString: string;
begin
  Result := ''.Join(' ', FPrefix);
end;

{ TChain }

constructor TChain.Create(const PrefixLength: Integer);
begin
  FMap := TDictionary<string, TList<string>>.Create;
  FPrefixLen := PrefixLength;
end;

destructor TChain.Destroy;
var
  Key: string;
begin
  for Key in FMap.Keys do
    FMap[Key].Free;
  FMap.Free;
  FPrefix.Free;
  inherited;
end;

class function TChain.FromResource(const ResourceName: string): TChain;
var
  Reader: TStreamReader;
  Stream: TResourceStream;
begin
  Result := Markov.TChain.Create(1);
  Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  Reader := TStreamReader.Create(Stream);
  try
    Result.Build(Reader);
  finally
    Reader.Close;
    Reader.Free;
    Stream.Free;
  end;
end;

procedure TChain.Build(const Reader: TStreamReader);
const
  Delimiters: array of char = [' ', #10, #13, '"', '.', ',', ':', ';', '(', ')'];
var
  Str: string;
  Prefix: TPrefix;
  W: string;
  Suffixes: TList<string>;
begin
  Str := Reader.ReadToEnd;

  Prefix := TPrefix.Create(FPrefixLen);
  try
    for W in Str.Split(Delimiters, TStringSplitOptions.ExcludeEmpty) do
    begin
      if not FMap.TryGetValue(Prefix.ToString, Suffixes) then
      begin
        Suffixes := TList<string>.Create;
        FMap.Add(Prefix.ToString, Suffixes);
      end;
      Suffixes.Add(W);
      Prefix.Shift(W);
    end;
  finally
    Prefix.Free;
  end;
end;

function TChain.Generate(const Count: Integer): string;
var
  Builder: TList<string>;
  I: Integer;
  Choices: TList<string>;
  Next: string;
begin
  Result := ''.Empty;

  if FPrefix = nil then
    FPrefix := TPrefix.Create(FPrefixLen);

  Builder := TList<string>.Create;
  try
    repeat
      for I := 0 to Count - 1 do
      begin
        if FMap.TryGetValue(FPrefix.ToString, Choices) then
        begin
          Next := Choices[Random(Choices.Count)];
          Builder.Add(Next);
          FPrefix.Shift(Next);
        end
        else
          Break;
      end;

      if Builder.Count = 0 then
      begin
        FPrefix.Free;
        FPrefix := TPrefix.Create(FPrefixLen);
      end;
    until Builder.Count > 0;
    Builder[0] := Builder[0].Trim.ToCharArray[0].ToUpper() + Builder[0].Substring(1);
    Result := ''.Join(' ', Builder.ToArray).Trim;
  finally
    Builder.Free;
  end;
end;

end.
