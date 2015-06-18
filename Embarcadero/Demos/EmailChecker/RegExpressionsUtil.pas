unit RegExpressionsUtil;

interface

type
  TRegularExpressionEngine = class
  public
    class function IsMatch(const Input, Pattern: string): boolean;
    class function IsValidEmail(const EmailAddress: string): boolean;
  end;

implementation

uses
  RegularExpressions;

{ TRegularExpressionEngine }

class function TRegularExpressionEngine.IsMatch(const Input,
  Pattern: string): boolean;
begin
  Result := TRegEx.IsMatch(Input, Pattern);
end;

class function TRegularExpressionEngine.IsValidEmail(
  const EmailAddress: string): boolean;
const
  EMAIL_REGEX = '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*"\x20*)*(?<angle><))?((?!\.)'
             +'(?>\.?[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])'
             +'[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\-]+(?<!-)\.)+[a-zA-Z]'
             +'{2,}|\[(((?(?<!\[)\.)(25[0-5]|2[0-4]\d|[01]?\d?\d))'
             +'{4}|[a-zA-Z\d\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|\\'
             +'[\x01-\x7f])+)\])(?(angle)>)$';
begin
  Result := IsMatch(EmailAddress, EMAIL_REGEX);
end;

end.
