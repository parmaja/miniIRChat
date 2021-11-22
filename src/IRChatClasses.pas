unit IRChatClasses;
{$mode ObjFPC}{$H+}
{**
 *  This file is part of the "miniIRChat"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

interface

uses
  Classes, SysUtils,
  mnIRCClients;

type
  TServerProfile = record
    Title: string;

    Host: string;
    Port: string;
    UseSSL: Boolean;

    NickNames: string;
    RealName: string;
    Rooms: string;

    Auth: TIRCAuth;
    Username: string;
    Password: string;
    CustomAuth: string;
  end;

implementation

end.

