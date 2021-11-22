unit MainForm;
{$mode ObjFPC}{$H+}
{**
 *  This file is part of the "miniIRChat"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

interface

uses
  Classes, SysUtils, StrUtils, FileUtil, IpHtml, Forms, Controls, Graphics,
  {$ifdef windows}Windows,{$endif}
  Dialogs, Buttons, IniFiles, StdCtrls, ExtCtrls, ComCtrls, Menus, LCLType, TypInfo, mnRTTIUtils, RttiUtils,
  mnMsgBox, GUIMsgBox, mnLogs, mnClasses, IRChatClasses,
  ChatRoomFrames, ServerForm, mnIRCClients;

type

  { TuiIRCClient }

  TuiIRCClient = class(TmnIRCClient)
  private
  public
    Profile: TServerProfile;
    procedure GetCurrentChannel(out vChannel: string); override;
    procedure DoLog(S: string); override;
    procedure DoMyInfoChanged; override;
    procedure DoConnected; override;
    procedure DoDisconnected; override;
    procedure DoUserChanged(vChannel: string; vUser, vNewNick: string); override;
    procedure DoProgressChanged; override;
    procedure DoUsersChanged(vChannelName: string; vChannel: TIRCChannel); override;
    procedure DoWhoIs(vUser: string); override;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string); override;

    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
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

  { TuiIRCClients }

  TuiIRCClients = class(specialize TmnObjectList<TuiIRCClient>)
  private
    FActive: Boolean;
  public
    Profiles: TuiServerProfiles;
    Current: TuiIRCClient;
    constructor Create;
    destructor Destroy; override;
    function Find(AName: string): TuiIRCClient;
    procedure Open(AName: string); overload;
    procedure Open; overload;
    procedure Close;
    property Active: Boolean read FActive write FActive;
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    AddBtn: TButton;
    MenuItem2: TMenuItem;
    ShowMnu: TMenuItem;
    ExitMnu: TMenuItem;
    DeleteBtn: TButton;
    EditBtn: TButton;
    SendEdit: TMemo;
    TrayIcon: TTrayIcon;
    TrayPopupMenu: TPopupMenu;
    WelcomeHtmlPnl: TIpHtmlPanel;
    OptionsBtn: TButton;
    WelcomePnl: TPanel;
    StatusPnl: TPanel;
    ProfileCbo: TComboBox;
    Label6: TLabel;
    ConnectBtn: TButton;
    JoinBtn: TButton;
    MsgPageControl: TPageControl;
    NicknameBtn: TButton;
    SendPnl: TPanel;
    ChatPnl: TPanel;
    LogEdit: TMemo;
    MenuItem1: TMenuItem;
    LogPopupMenu: TPopupMenu;
    Panel2: TPanel;
    SendBtn: TButton;
    SmallImageList: TImageList;
    Splitter1: TSplitter;
    procedure AddBtnClick(Sender: TObject);
    procedure ExitMnuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormWindowStateChange(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure JoinBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgPageControlChange(Sender: TObject);
    procedure NicknameBtnClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure ProfileCboSelect(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FDestroying: Boolean;
    Body: TIpHtmlNodeBODY;
    Recents: TStringList;
    RecentsIndex: Integer;
    procedure AddMessage(aMsg: string; AClassName: string);
    procedure ForceForegroundWindow;
    procedure HideApp;
    procedure RecentUp;
    procedure RecentDown;
    procedure AddRecent(S: string);
    procedure LogMessage(S: string);
    function CurrentRoom: string;
    procedure SendNow;
    function NeedRoom(vRoomName: string; ActiveIt: Boolean = false): TChatRoomFrame;
    procedure SetNick(ANick: string);
    procedure ShowApp;
  public
    StartMinimized: Boolean;
    ShowTray: Boolean;
    AutoStart: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteProfile(ProfileName: string);
    procedure EnumProfiles;
    procedure SaveConfig;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
    procedure ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
  end;

var
  MainFrm: TMainFrm;
  IRCClients: TuiIRCClients = nil;

implementation

{$R *.lfm}

{ TuiServerProfile }

procedure TuiServerProfile.SaveProfile(FileName: string; AName: string);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(AName, 'Title', Profile.Title);
    Ini.WriteString(AName, 'Username', Profile.Username);
    Ini.WriteString(AName, 'Password', Profile.Password);
    Ini.WriteString(AName, 'Nicknames', Profile.Nicknames);
    Ini.WriteString(AName, 'RealName', Profile.RealName);
    Ini.WriteString(AName, 'Rooms', Profile.Rooms);
    Ini.WriteString(AName, 'Host', Profile.Host);
    Ini.WriteString(AName, 'Port', Profile.Port);
    Ini.WriteString(AName, 'AuthType', AuthToString(Profile.AuthType));
    Ini.WriteString(AName, 'CustomAuth', Profile.CustomAuth);
    Ini.WriteBool(AName, 'SSL', Profile.UseSSL);
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
    Profile.Password := Ini.ReadString(AName, 'Password', '');
    Profile.Nicknames := Ini.ReadString(AName, 'Nicknames', '');
    Profile.RealName := Ini.ReadString(AName, 'RealName', '');
    Profile.Rooms := Ini.ReadString(AName, 'Rooms', '');
    Profile.Host := Ini.ReadString(AName, 'Host', '');
    Profile.Port := Ini.ReadString(AName, 'Port', '6667');
    Profile.AuthType := StringToAuth(Ini.ReadString(AName, 'AuthType', ''));
    Profile.CustomAuth := Ini.ReadString(AName, 'CustomAuth', '');
    Profile.UseSSL := Ini.ReadBool(AName, 'SSL', false);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TuiIRCClient.DoBeforeOpen;
begin
  inherited;
  Host := Profile.Host;
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

  AuthType := Profile.AuthType; //authPASS
  //IRC.Auth := authIDENTIFY;
  Username := Profile.Username;
  Password := Profile.Password;

end;
procedure TuiIRCClient.DoAfterOpen;
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

{ TuiIRCClients }

constructor TuiIRCClients.Create;
begin
  inherited;
  Profiles := TuiServerProfiles.Create;
end;

destructor TuiIRCClients.Destroy;
begin
  FreeAndNil(Profiles);
  inherited;
end;

function TuiIRCClients.Find(AName: string): TuiIRCClient;
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

procedure TuiIRCClients.Open(AName: string);
var
  aClient: TuiIRCClient;
  i: Integer;
begin
  i := Profiles.IndexOf(AName);
  if i >=0 then
  begin
    aClient := TuiIRCClient.Create;
    Add(aClient);
    aClient.Profile := Profiles[i].Profile;
    aClient.Open;
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

procedure TuiIRCClients.Open;
var
  itm: TuiServerProfile;
  aClient: TuiIRCClient;
begin
  FActive := True;
  for itm in Profiles do
  begin
    aClient := TuiIRCClient.Create;
    Add(aClient);
    aClient.Profile := itm.Profile;
    aClient.Open;
  end;
end;

procedure TuiIRCClients.Close;
var
  itm: TuiIRCClient;
begin
  for itm in Self do
  begin
    itm.Close;
  end;
  FActive := False;
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

{ TuiIRCClient }

procedure TuiIRCClient.GetCurrentChannel(out vChannel: string);
begin
  vChannel := MainFrm.CurrentRoom;
end;

procedure TuiIRCClient.DoLog(S: string);
begin
  inherited;
  MainFrm.LogMessage(S);
end;

procedure TuiIRCClient.DoMyInfoChanged;
begin
  inherited;
  MainFrm.SetNick(Session.Nick);
end;

procedure TuiIRCClient.DoConnected;
begin
  inherited DoConnected;
  MainFrm.LogMessage('Yes it is connected');
end;

procedure TuiIRCClient.DoDisconnected;
begin
  MainFrm.LogMessage('Yes it is disconnected');
  inherited;
end;

procedure TuiIRCClient.DoUserChanged(vChannel: string; vUser, vNewNick: string);
begin
  inherited;
  //TODO
end;

procedure TuiIRCClient.DoProgressChanged;
begin
  inherited;
  case Progress of
    prgDisconnected:
    begin
      MainFrm.ChatPnl.Visible := False;
      MainFrm.WelcomePnl.Visible := True;
      while MainFrm.MsgPageControl.PageCount > 0 do
        MainFrm.MsgPageControl.Page[0].Free;
    end;
    prgConnecting:;
      //MainFrm.ConnectBtn.Caption := 'Connecting';
    prgConnected:
    begin
      MainFrm.WelcomePnl.Visible := False;
      MainFrm.ChatPnl.Visible := True;
      MainFrm.SendEdit.SetFocus;
    end;
    prgReady:
    begin
      //MainFrm.ConnectBtn.Caption := 'Disconnect';
    end;
  end;
end;

procedure TuiIRCClient.DoUsersChanged(vChannelName: string; vChannel: TIRCChannel);
begin
  inherited;
  MainFrm.ReceiveNames(vChannelName, vChannel);
end;

procedure TuiIRCClient.DoWhoIs(vUser: string);
var
  aUser: TIRCUser;
begin
  inherited;
  aUser := IRCClients.Current.Session.Channels.FindUser('', vUser);
  if aUser <> nil then
  begin
    MainFrm.LogMessage(aUser.WhoIs.RealName);
    MainFrm.LogMessage(aUser.WhoIs.Server);
    MainFrm.LogMessage(aUser.WhoIs.Channels);
  end;
end;

procedure TuiIRCClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string);
begin
  MainFrm.DoReceive(vMsgType, vChannel, vUser, vMsg);
end;

{ TMainFrm }

procedure TMainFrm.ConnectBtnClick(Sender: TObject);
begin
  if IRCClients.Active then
  begin
    IRCClients.Close;
    ConnectBtn.Caption := 'Disconnect';
  end
  else
  begin
    ConnectBtn.Caption := 'Connect';
    IRCClients.Open;
  end;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
end;

procedure TMainFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_TAB) then
  begin
    if Shift = [ssCtrl] then
      MsgPageControl.SelectNextPage(True)
    else if Shift = [ssCtrl, ssShift] then
      MsgPageControl.SelectNextPage(False);
  end;
end;

procedure TMainFrm.AddBtnClick(Sender: TObject);
var
  Profile: TServerProfile;
begin
  Profile := Default(TServerProfile);
  if ShowServerProfile(Profile) then
  begin
    IRCClients.Profiles.AddProfile(Profile);
    EnumProfiles;
    ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(Profile.Title);
  end;
end;

procedure TMainFrm.ExitMnuClick(Sender: TObject);
begin
  FDestroying := True;
  Close;
end;

procedure TMainFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  ini: TIniFile;
begin
  if FDestroying then
  begin
    CloseAction := caFree;
    ini := TIniFile.Create(Application.Location + Application.Name + '.ini');
    try
      ini.WriteInteger('Size', 'Width', Width);
      ini.WriteInteger('Size', 'Height', Height);
    finally
      ini.Free;
    end;
  end
  else
  begin
    //Hide;
    CloseAction := caHide;
    HideApp;
  end;
end;

procedure TMainFrm.DeleteBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if ProfileCbo.Text <> '' then
  begin
    if not MsgBox.No('Are you sure want to delete ' + ProfileCbo.Text) then
    begin
      DeleteProfile(ProfileCbo.Text);
      i := IRCClients.Profiles.IndexOf(ProfileCbo.Text);
      if i >= 0 then
        IRCClients.Profiles.Delete(i);
      EnumProfiles;
    end;
  end;
end;

procedure TMainFrm.JoinBtnClick(Sender: TObject);
var
  Rooms: string;
begin
  Rooms := '';
  if MsgBox.Input(Rooms, 'Join Rooms') and (Rooms <> '') then
    IRCClients.Current.Join(Rooms);
end;

procedure TMainFrm.MenuItem1Click(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainFrm.MsgPageControlChange(Sender: TObject);
begin

end;

procedure TMainFrm.NicknameBtnClick(Sender: TObject);
var
  aNick: string;
begin
  aNick := IRCClients.Current.Session.Nick;
  if MsgBox.Input(aNick, 'New Nickname?') then
  begin
    IRCClients.Current.SetNick(aNick);
  end;
end;

procedure TMainFrm.OptionsBtnClick(Sender: TObject);
var
  aProfile: TServerProfile;
  itm: TuiServerProfile;
begin
  itm := IRCClients.Profiles.Find(ProfileCbo.Text);
  if itm <> nil then
  begin
    aProfile := itm.Profile;
    if ShowServerProfile(aProfile) then
    begin
      itm.Profile := aProfile;
      DeleteProfile(ProfileCbo.Text);
      itm.SaveProfile(IRCClients.Profiles.ProfileFileName, itm.Profile.Title);
      EnumProfiles;
      ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(itm.Profile.Title);
    end;
  end;
end;

procedure TMainFrm.PasswordEditChange(Sender: TObject);
begin
end;

procedure TMainFrm.ProfileCboSelect(Sender: TObject);
begin
end;

procedure TMainFrm.SendBtnClick(Sender: TObject);
begin
  SendNow;
end;

procedure TMainFrm.SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_TAB: //TODO
      begin
        if copy(SendEdit.Text, SendEdit.SelStart, 1) = '@' then
        begin
          Key := 0;
        end;
      end;
      VK_UP: RecentUp;
      VK_DOWN: RecentDown;
      VK_RETURN:
      begin
        Key := 0;
        SendNow;
      end;
    end;
  end;
end;

procedure TMainFrm.TrayIconClick(Sender: TObject);
begin
  ForceForegroundWindow;
end;

procedure TMainFrm.TrayIconDblClick(Sender: TObject);
begin
  ShowApp;
end;

procedure TMainFrm.SendNow;
var
  s: string;
begin
  s := TrimRight(SendEdit.Text);
  if s <> '' then
    if IRCClients.Current.Online then
    begin
      IRCClients.Current.SendMsg(CurrentRoom, SendEdit.Text);
      AddRecent(SendEdit.Text);
      SendEdit.Text := '';
    end;
end;

function TMainFrm.NeedRoom(vRoomName: string; ActiveIt: Boolean): TChatRoomFrame;
var
  i, Index: Integer;
  TabSheet: TTabSheet;
  ARoomName: string;
  AIsRoom: Boolean;
begin
  if vRoomName = '*' then
    vRoomName := '';

  ARoomName := vRoomName;
  if LeftStr(ARoomName, 1) = '#' then
  begin
    ARoomName := MidStr(ARoomName, 2, MaxInt);
    AIsRoom := True;
  end
  else
    AIsRoom := False;

  index := -1;
  for i := 0 to MsgPageControl.PageCount - 1 do
  begin
    if SameText((MsgPageControl.Pages[i].Controls[0] as TChatRoomFrame).RoomName, vRoomName) then
    begin
      Index := i;
      break;
    end;
  end;

  if Index < 0 then
  begin
    TabSheet := MsgPageControl.AddTabSheet;
    with TChatRoomFrame.CreateParented(TabSheet.Handle) do
    begin
      Parent := TabSheet;
      Align := alClient;
      RoomName := vRoomName;
      IsRoom := AIsRoom;
      Visible := True;
    end;
    ActiveIt := True; //force to focus it
    //TabSheet.Name := ARoomName + '_Room';
    if ARoomName = '' then
      TabSheet.Caption := '[Server]'
    else
      TabSheet.Caption := vRoomName;
    Result := TabSheet.Controls[0] as TChatRoomFrame;
  end
  else
  begin
    TabSheet := MsgPageControl.Pages[Index];
    Result := TabSheet.Controls[0] as TChatRoomFrame;
  end;

  if (TabSheet <> nil) and ActiveIt then
    MsgPageControl.PageIndex := TabSheet.PageIndex;
end;

procedure TMainFrm.SetNick(ANick: string);
begin
  NicknameBtn.Caption := ANick;
 //TODO change it in the list
end;

procedure TMainFrm.RecentUp;
begin
  if Recents.Count > 0 then
  begin
    if RecentsIndex > 0 then
    begin
      RecentsIndex := RecentsIndex - 1;
      if RecentsIndex = 0 then
        SendEdit.Text := ''
      else
        SendEdit.Text := Recents[RecentsIndex - 1];
    end
    else
    begin
      RecentsIndex := Recents.Count;
      SendEdit.Text := Recents[RecentsIndex - 1];
    end;
  end;
end;

procedure TMainFrm.RecentDown;
begin
  if Recents.Count > 0 then
  begin
    if RecentsIndex < Recents.Count then
    begin
      RecentsIndex := RecentsIndex + 1;
      SendEdit.Text := Recents[RecentsIndex - 1];
    end
    else
    begin
      RecentsIndex := 0;
      SendEdit.Text := '';
    end;
  end;
end;

procedure TMainFrm.AddRecent(S: string);
var
  i: Integer;
begin
  i := Recents.IndexOf(S);
  if i >= 0 then
    Recents.Delete(i);
  Recents.Add(S);
  RecentsIndex := 0;
end;

procedure TMainFrm.LogMessage(S: string);
begin
  LogEdit.Lines.Add(S)
end;

function TMainFrm.CurrentRoom: string;
begin
  if MsgPageControl.ActivePage <> nil then
  begin
    Result := (MsgPageControl.ActivePage.Controls[0] as TChatRoomFrame).RoomName;
  end
  else
    Result := '';
end;

constructor TMainFrm.Create(TheOwner: TComponent);
var
  i: Integer;
  ini: TIniFile;
  aProfile: string;
  aStream: TStream;
begin
  inherited;
  InstallFileLog('log.txt');
  InstallEventLog(@LogMessage);
  {$ifdef DEBUG}
  InstallConsoleLog;
  InstallDebugOutputLog;
  {$endif}
  {$macro on}
  WelcomePnl.Align := alClient;
  ChatPnl.Align := alClient;
  Caption := Caption + ' ' + IntToStr(FPC_FULLVERSION);
  Recents := TStringList.Create;
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    aProfile := Ini.ReadString('Options', 'Profile', '');
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);
    LogEdit.Height := Ini.ReadInteger('Window', 'LogHeight', LogEdit.Height);
  finally
    FreeAndNil(ini);
  end;
  IRCClients := TuiIRCClients.Create;
  IRCClients.Profiles.ProfileFileName := Application.Location + 'profiles.ini';
  MsgPageControl.ActivePageIndex := 0;
  LogEdit.Clear;
  IRCClients.Profiles.LoadProfiles;
  EnumProfiles;
  i := ProfileCbo.Items.IndexOf(aProfile);
  if i < 0 then
    i := 0;
  ProfileCbo.ItemIndex := i;
  aStream := CreateChatHTMLStream;
  try
    WelcomeHtmlPnl.SetHtmlFromStream(aStream);
    //WelcomeHtmlPnl.SetHtmlFromFile(Application.Location + 'chat.html');
  finally
    FreeAndNil(aStream);
  end;
  //Find Body
  //Viewer.EnumDocuments(@HtmlEnumerator);
  for i :=0 to WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildCount - 1 do
    if WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildNode[i] is TIpHtmlNodeBODY then
    begin
      Body := TIpHtmlNodeBODY(WelcomeHtmlPnl.MasterFrame.Html.HtmlNode.ChildNode[i]);
      break;
    end;
  //AddMessage('Welcome to irc, click connect', 'action');
  if StartMinimized then
    HideApp
  else if ShowTray then
    TrayIcon.Show;
end;

destructor TMainFrm.Destroy;
begin
  FDestroying := True;
  IRCClients.Close;
  IRCClients.Free;
  SaveConfig;
  FreeAndNil(Recents);
  inherited;
end;

procedure TMainFrm.DeleteProfile(ProfileName: string);
var
  ini: TIniFile;
begin
  inherited;
  ini := TIniFile.Create(Application.Location + 'profiles.ini');
  try
    ini.EraseSection(ProfileName);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainFrm.EnumProfiles;
var
  i: Integer;
  Old: string;
begin
  Old := ProfileCbo.Text;
  ProfileCbo.Clear;
  for i := 0 to IRCClients.Profiles.Count -1 do
  begin
    ProfileCbo.Items.Add(IRCClients.Profiles[i].Profile.Title);
  end;
  if Old <> '' then
    i := ProfileCbo.Items.IndexOf(Old)
  else
    i := 0;
  if i < 0 then
    i := 0;

  ProfileCbo.ItemIndex := i;

end;

procedure TMainFrm.SaveConfig;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Application.Location + 'setting.ini');
  try
    if WindowState <> wsMaximized then
    begin
      Ini.WriteInteger('Window', 'Width', Width);
      Ini.WriteInteger('Window', 'Height', Height);
      Ini.WriteInteger('Window', 'LogHeight', LogEdit.Height);
      Ini.WriteString('Options', 'Profile', ProfileCbo.Text);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TMainFrm.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
var
  ChatRoom: TChatRoomFrame;
  aItem: TListItem;
  oUser: TIRCUser;
begin
  if vChannel = '' then
    LogMessage(vMSG)
  else
  begin
    ChatRoom := NeedRoom(vChannel);
    if ChatRoom <> nil then
      with ChatRoom do
        begin
          case vMsgType of
            mtWelcome:
            begin
              AddMessage(vMSG);
              TopicEdit.Text := vMSG;
            end;
            mtMOTD:
              AddMessage(vMSG);
            mtTopic:
            begin
              TopicEdit.Text := vMSG;
              AddMessage(vMSG, '', True);
            end;
            mtJoin:
            begin
              AddMessage(vUser + ' is joined', 'hint');
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem = nil then
              begin
                aItem := UserListBox.Items.Add;
                aItem.Caption := vUser;
                oUser := IRCClients.Current.Session.Channels.FindUser(vChannel, vUser);
                if oUser <> nil then
                begin
                  if ([umAdmin, umOwner] * oUser.Mode <> []) then
                    aItem.ImageIndex := 3
                  else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
                    aItem.ImageIndex := 2
                  else if ([umVoice] * oUser.Mode <> []) then
                    aItem.ImageIndex := 1
                  else
                    aItem.ImageIndex := 0;
                end;
              end;
            end;
            mtLeft:
            begin
              //if me close the tab
              AddMessage(vUser + ' is left: ' + vMsg, 'hint');
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem <> nil then
                UserListBox.items.Delete(aItem.Index);
            end;
            mtUserMode:
            begin
              aItem := UserListBox.Items.FindCaption(0, vUser, False, True, False, False);
              if aItem = nil then
              begin
                aItem := UserListBox.Items.Add;
                aItem.Caption := vUser;
                aItem.ImageIndex := 0;
              end;
              oUser := IRCClients.Current.Session.Channels.FindUser(vChannel, vUser);
              if oUser <> nil then
              begin
                if ([umAdmin, umOwner] * oUser.Mode <> []) then
                  aItem.ImageIndex := 3
                else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
                  aItem.ImageIndex := 2
                else if ([umVoice] * oUser.Mode <> []) then
                  aItem.ImageIndex := 1
                else
                  aItem.ImageIndex := 0;
              end;
            end;
            mtNotice:
              AddMessage('[' + vUser + '] ' + vMSG, 'notice');
            mtMessage:
            begin
              AddMessage(vUser + ': ' + vMSG, ' received');
            end;
            mtSend:
            begin
              AddMessage(vUser + ': ' + vMSG, 'self');
            end;
            mtAction:
            begin
              AddMessage('* ' + vUser + ': -' + vMSG + '-', 'action');
            end;
          else
              AddMessage(vUser + ': ' + vMSG, 'log');
          end;
        end;
   end;
end;

procedure TMainFrm.ReceiveNames(vChannel: string; vUserNames: TIRCChannel);
var
  oUser: TIRCUser;
  ChatRoom: TChatRoomFrame;
  aItem: TListItem;
  i: Integer;
begin
  ChatRoom := NeedRoom(vChannel);
  if ChatRoom <> nil then
    with ChatRoom do
  begin
    UserListBox.Clear;
    for i := 0 to vUserNames.Count -1 do
    begin
      aItem := UserListBox.Items.Add;
      aItem.Caption := vUserNames[i].Name;
      oUser := vUserNames[i];
      if oUser <> nil then
      begin
        if ([umAdmin, umOwner] * oUser.Mode <> []) then
          aItem.ImageIndex := 3
        else if ([umHalfOp, umOp, umWallOp] * oUser.Mode <> []) then
          aItem.ImageIndex := 2
        else if ([umVoice] * oUser.Mode <> []) then
          aItem.ImageIndex := 1
        else
          aItem.ImageIndex := 0;
      end;
    end;
  end;
end;

procedure TMainFrm.AddMessage(aMsg: string; AClassName: string);
var
  TextNode: TIpHtmlNodeText;
  PNode: TIpHtmlNodeP;
  //DivNode: TIpHtmlNodeDIV;
begin
  {DivNode := TIpHtmlNodeDIV.Create(Body);
  DivNode.ClassId := AClassName;}
  PNode := TIpHtmlNodeP.Create(Body);
  PNode.ClassId := AClassName;
  TextNode := TIpHtmlNodeText.Create(PNode);
  TextNode.EscapedText := aMsg;

  with TIpHtmlNodeBR.Create(Body) do
  begin
  end;

  WelcomeHtmlPnl.Update;
  WelcomeHtmlPnl.Scroll(hsaEnd);
end;

procedure TMainFrm.ForceForegroundWindow;
{$ifdef windows}
var
  aForeThread, aAppThread: DWORD;
  aProcessID: DWORD;
  {$endif}
begin
  ShowApp;
  {$ifdef windows}
  aProcessID := 0;
  aForeThread := GetWindowThreadProcessId(GetForegroundWindow(), aProcessID);
  aAppThread := GetCurrentThreadId();

  if (aForeThread <> aAppThread) then
  begin
    AttachThreadInput(aForeThread, aAppThread, True);
    BringWindowToTop(Handle);
    AttachThreadInput(aForeThread, aAppThread, False);
  end
  else
    BringWindowToTop(Handle);
  {$endif}
  BringToFront;
end;

procedure TMainFrm.ShowApp;
begin
  Visible := True;
  WindowState := wsNormal;
  ShowInTaskBar := stDefault;
  Show;
  if ShowTray then
    TrayIcon.Show
  else
    TrayIcon.Hide;
end;

procedure TMainFrm.HideApp;
begin
  ShowInTaskBar := stNever;
  Hide;
  TrayIcon.Show;
end;

procedure TMainFrm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    HideApp;
end;

end.

