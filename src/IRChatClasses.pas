unit IRChatClasses;
{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}
{**
 *  This file is part of the "miniIRChat"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

interface

uses
  Classes, SysUtils,
  typinfo, mnIRCClients;

type

  TServerProfile = record
    Title: string;

    Host: string;
    Port: string;
    UseSSL: Boolean;

    NickNames: string;
    RealName: string;
    Rooms: string;

    AuthType: TIRCAuthType;
    Username: string;
    Password: string;
    CustomAuth: string;
  end;

function AuthToString(EnumValue: TIRCAuthType): string;
function StringToAuth(EnumValue: String): TIRCAuthType;

implementation

function AuthToString(EnumValue: TIRCAuthType): string;
begin
  Result := GetEnumName(TypeInfo(TIRCAuthType), Ord(EnumValue));
end;

function StringToAuth(EnumValue: String): TIRCAuthType;
begin
  Result := TIRCAuthType(GetEnumValue(TypeInfo(TIRCAuthType), EnumValue));
end;


end.

