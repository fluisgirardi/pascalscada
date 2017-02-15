unit pascalscada.security.security_texts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SAccessDenied = 'Access denied!';
  SUserHasNotAllowedToAccessObject = 'Access denied!'+LineEnding+'User "%s" isn''t allowed to access object protected by security code "%s"';
  SNoUserLoggedInToAccessObject    = 'Access denied!'+LineEnding+'No user logged in to access object protected by security code "%s". Did you forget to do Login?';

implementation

end.

