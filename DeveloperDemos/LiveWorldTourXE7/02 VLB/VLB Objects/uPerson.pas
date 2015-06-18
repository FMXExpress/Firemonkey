unit uPerson;

interface

type
  TPerson = class
  private
    FAge: Integer;
    FLastname: string;
    FFirstname: string;
  public
    constructor Create(const Firstname, Lastname : string; Age : Integer); virtual;
    property Firstname : string read FFirstname write FFirstname;
    property Lastname : string read FLastname write FLastname;
    property Age : Integer read FAge write FAge;
  end;

implementation

{ TPerson }

constructor TPerson.Create(const Firstname, Lastname: string; Age: Integer);
begin
  FFirstname := Firstname;
  FLastname := Lastname;
  FAge := Age;
end;

end.
