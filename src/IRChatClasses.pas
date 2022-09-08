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
  Classes, SysUtils, typinfo, IniFiles, base64,
  mnIRCClients, mnClasses;

type

  TServerProfile = record
    Title: string;

    Host: string;
    Bind: string;
    Port: string;
    UseSSL: Boolean;

    NickNames: string;
    RealName: string;
    Rooms: string;

    AuthType: TIRCAuthType;
    Username: string;
    Password: string;
    CustomAuth: string;
    AutoConnect: Boolean;
  end;

  { TuiServerProfile }

  TuiServerProfile = class(TObject)
  public
    Profile: TServerProfile;
    procedure LoadProfile(FileName: string; AName: string);
    procedure SaveProfile(FileName: string; AName: string);
  end;

  { TuiServerProfiles }

  TuiServerProfiles = class(specialize TmnObjectList<TuiServerProfile>)
  private
  public
    ProfileFileName: string;
    function IndexOf(AName: string): Integer;
    function Find(AName: string): TuiServerProfile;
    procedure SaveProfiles;
    procedure LoadProfiles;
    procedure AddProfile(AProfile: TServerProfile);
  end;

  { TIRCChatClient }

  TIRCChatClient = class(TmnIRCClient)
  protected
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
  public
    Profile: TServerProfile;
  end;

  { TIRCChatClients }

  TIRCChatClients = class(specialize TmnObjectList<TIRCChatClient>)
  private
    FActive: Boolean;
  public
    Profiles: TuiServerProfiles;
//    Current: TIRCChatClient;
    constructor Create;
    destructor Destroy; override;
    function CreateClient: TIRCChatClient; virtual;
    function Find(AName: string): TIRCChatClient;
    procedure Open(AName: string); overload;
    procedure Open; overload;
    procedure Close;
    property Active: Boolean read FActive write FActive;
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


{ TuiServerProfile }

procedure TuiServerProfile.SaveProfile(FileName: string; AName: string);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(AName, 'Title', Profile.Title);
    Ini.WriteString(AName, 'Username', Profile.Username);
    Ini.WriteString(AName, 'Password', EncodeStringBase64(Profile.Password));
    Ini.WriteString(AName, 'Nicknames', Profile.Nicknames);
    Ini.WriteString(AName, 'RealName', Profile.RealName);
    Ini.WriteString(AName, 'Rooms', Profile.Rooms);
    Ini.WriteString(AName, 'Host', Profile.Host);
    Ini.WriteString(AName, 'Bind', Profile.Bind);
    Ini.WriteString(AName, 'Port', Profile.Port);
    Ini.WriteString(AName, 'AuthType', AuthToString(Profile.AuthType));
    Ini.WriteString(AName, 'CustomAuth', Profile.CustomAuth);
    Ini.WriteBool(AName, 'SSL', Profile.UseSSL);
    Ini.WriteBool(AName, 'AutoConnect', Profile.AutoConnect);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TuiServerProfile.LoadProfile(FileName: string; AName: string);
var
  ini: TIniFile;
begin
  inherited;
  ini := TIniFile.Create(FileName);
  try
    Profile.Title := Ini.ReadString(AName, 'Title', '');
    Profile.Username := Ini.ReadString(AName, 'Username', '');
    Profile.Password := DecodeStringBase64(Ini.ReadString(AName, 'Password', ''));
    Profile.Nicknames := Ini.ReadString(AName, 'Nicknames', '');
    Profile.RealName := Ini.ReadString(AName, 'RealName', '');
    Profile.Rooms := Ini.ReadString(AName, 'Rooms', '');
    Profile.Host := Ini.ReadString(AName, 'Host', '');
    Profile.Bind := Ini.ReadString(AName, 'Bind', '');
    Profile.Port := Ini.ReadString(AName, 'Port', '6667');
    Profile.AuthType := StringToAuth(Ini.ReadString(AName, 'AuthType', ''));
    Profile.CustomAuth := Ini.ReadString(AName, 'CustomAuth', '');
    Profile.UseSSL := Ini.ReadBool(AName, 'SSL', false);
    Profile.AutoConnect := Ini.ReadBool(AName, 'AutoConnect', false);
  finally
    FreeAndNil(ini);
  end;
end;

function TuiServerProfiles.IndexOf(AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if SameText(Self[i].Profile.Title, AName) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TuiServerProfiles.Find(AName: string): TuiServerProfile;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Self[i].Profile.Title, AName) then
    begin
      Result := Self[i];
      break;
    end;
  end;
end;

procedure TuiServerProfiles.SaveProfiles;
var
  itm: TuiServerProfile;
begin
  for itm in Self do
  begin
    itm.SaveProfile(ProfileFileName, itm.Profile.Title);
  end;
end;

procedure TuiServerProfiles.LoadProfiles;
var
  ini: TIniFile;
  Sections: TStringList;
  s: string;
  itm: TuiServerProfile;
begin
  ini := TIniFile.Create(ProfileFileName);
  try
    Sections := TStringList.Create;
    ini.ReadSections(Sections);
    for s in Sections do
    begin
      itm := TuiServerProfile.Create;
      Add(itm);
      itm.LoadProfile(ProfileFileName, s);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TuiServerProfiles.AddProfile(AProfile: TServerProfile);
var
  itm: TuiServerProfile;
begin
  itm := TuiServerProfile.Create;
  Add(itm);
  itm.Profile := AProfile;
  itm.SaveProfile(ProfileFileName, AProfile.Title);
end;

{ TIRCChatClient }

procedure TIRCChatClient.DoBeforeOpen;
begin
  inherited;
  Host := Profile.Host;
  Bind := Profile.Bind;
  Port := Profile.Port;
  UseSSL := Profile.UseSSL;

  Nicks.Clear;
  Nicks.CommaText := Profile.NickNames;
  if Profile.NickNames = '' then
  begin
    Nicks.Add(Profile.Username);
    Nicks.Add(Profile.Username + '_');
    Nicks.Add(Profile.Username + '__');
  end;
  RealName := Profile.RealName;
  if RealName = '' then
    RealName := Profile.Username;

  AuthType := Profile.AuthType;
  Username := Profile.Username;
  Password := Profile.Password;
end;

procedure TIRCChatClient.DoAfterOpen;
var
  Room: string;
  List: TStringList;
begin
  inherited;
  List := TStringList.Create;
  try
    List.CommaText := Profile.Rooms;
    for Room in List do
    begin
      Join(Room);
      //Who(Room); //TODO
    end;
  finally
    List.Free;
  end;
end;

{ TIRCChatClients }

constructor TIRCChatClients.Create;
begin
  inherited;
  Profiles := TuiServerProfiles.Create;
end;

destructor TIRCChatClients.Destroy;
begin
  FreeAndNil(Profiles);
  inherited;
end;

function TIRCChatClients.CreateClient: TIRCChatClient;
begin
  Result := TIRCChatClient.Create;
end;

function TIRCChatClients.Find(AName: string): TIRCChatClient;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Self[i].Profile.Title, AName) then
    begin
      Result := Self[i];
      break;
    end;
  end;
end;

procedure TIRCChatClients.Open(AName: string);
var
  aClient: TIRCChatClient;
  i: Integer;
begin
  i := Profiles.IndexOf(AName);
  if i >=0 then
  begin
    aClient := CreateClient;
    Add(aClient);
    aClient.Profile := Profiles[i].Profile;
    aClient.Open;
  end;
end;

procedure TIRCChatClients.Open;
var
  itm: TuiServerProfile;
  aClient: TIRCChatClient;
begin
  FActive := True;
  for itm in Profiles do
  begin
    if itm.Profile.AutoConnect then
    begin
      aClient := CreateClient;
      Add(aClient);
      aClient.Profile := itm.Profile;
      aClient.Open;
    end;
  end;
end;

procedure TIRCChatClients.Close;
var
  itm: TIRCChatClient;
begin
  for itm in Self do
  begin
    itm.Close;
  end;
  FActive := False;
end;

end.

