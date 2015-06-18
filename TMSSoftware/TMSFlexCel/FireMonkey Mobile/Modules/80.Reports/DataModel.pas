unit DataModel;

interface
uses SysUtils, Generics.Defaults, Generics.Collections;
type
  TVersion = class
  private
    FName: string;
    FCount: integer;
  public
    property Name: string read FName;
    property Count: integer read FCount;

    constructor Create(const aName: string; const aCount: integer);

  end;

  TVersionList = TList<TVersion>;

  TVersionComparer = class(TInterfacedObject, IComparer<TVersion>)
  public
    function Compare(const Left, Right: TVersion): Integer;
  end;

implementation

{ TVersion }

constructor TVersion.Create(const aName: string; const aCount: integer);
begin
  FName := aName;
  FCount := aCount;
end;

{ TVersionComparer }

function TVersionComparer.Compare(const Left, Right: TVersion): Integer;
begin
  if Left.Name.Length < Right.Name.Length then exit(-1);
  if Left.Name.Length > Right.Name.Length then exit(1);
  exit(Left.Name.CompareTo(Right.Name));

end;

end.
