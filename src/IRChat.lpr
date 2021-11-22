program IRChat;
{**
 *  This file is part of the "Mini Library"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, mnIRCClients, ChatRoomFrames, ServerForm, IRChatClasses;

{$R *.res}
{$R IRChatRes.rc}

begin
  RequireDerivedFormResource :=True;
  Application.Scaled :=True;
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.Run;
end.

