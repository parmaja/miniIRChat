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
  Dialogs, Buttons, IniFiles, StdCtrls, ExtCtrls, ComCtrls, Menus, LCLType,
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

    procedure LoadProfile(AName: string);
    procedure SaveProfile(AName: string);

    procedure ConnectProfile;
  end;

  { TuiIRCClients }

  TuiIRCClients = class(specialize TmnObjectList<TuiIRCClient>)
  private
    FActive: Boolean;
  public
    Current: TuiIRCClient;
    procedure Open;
    procedure Close;
    property Active: Boolean read FActive write FActive;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure AddProfile(AProfile: TServerProfile);
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ServersBtn: TButton;
    SendEdit: TMemo;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HostEditKeyPress(Sender: TObject; var Key: char);
    procedure JoinBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MsgPageControlChange(Sender: TObject);
    procedure NicknameBtnClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure PasswordEditChange(Sender: TObject);
    procedure ProfileCboSelect(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    Body: TIpHtmlNodeBODY;
    Recents: TStringList;
    RecentsIndex: Integer;
    procedure AddMessage(aMsg: string; AClassName: string);
    procedure RecentUp;
    procedure RecentDown;
    procedure AddRecent(S: string);
    procedure LogMessage(S: string);
    function CurrentRoom: string;
    procedure ConnectNow;
    procedure SendNow;
    function NeedRoom(vRoomName: string; ActiveIt: Boolean = false): TChatRoomFrame;
    procedure SetNick(ANick: string);
  public
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

procedure TuiIRCClient.SaveProfile(AName: string);
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(Application.Location + 'profiles.ini');
  try
    Ini.WriteString(AName, 'Title', Profile.Title);
    Ini.WriteString(AName, 'Username', Profile.Username);
    Ini.WriteString(AName, 'Password', Profile.Password);
    Ini.WriteString(AName, 'Nicknames', Profile.Nicknames);
    Ini.WriteString(AName, 'RealName', Profile.RealName);
    Ini.WriteString(AName, 'Rooms', Profile.Rooms);
    Ini.WriteString(AName, 'Host', Profile.Host);
    Ini.WriteString(AName, 'Port', Profile.Port);
    Ini.WriteString(AName, 'CustomAuth', Profile.CustomAuth);
    Ini.WriteBool(AName, 'SSL', Profile.UseSSL);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TuiIRCClient.ConnectProfile;
var
  Room: string;
  List: TStringList;
begin
  Host := Profile.Host;
  Port := Profile.Port;
  UseSSL := Profile.UseSSL;

  Nicks.Clear;
  Nicks.Delimiter := ',';
  Nicks.DelimitedText := Profile.NickNames;
  if Profile.NickNames = '' then
  begin
    Nicks.Add(Profile.Username);
    Nicks.Add(Profile.Username + '_');
    Nicks.Add(Profile.Username + '__');
  end;
  RealName := Profile.RealName;
  if RealName = '' then
    RealName := Profile.Username;

  Auth := Profile.Auth; //authPASS
  //IRC.Auth := authIDENTIFY;
  Username := Profile.Username;
  Password := Profile.Password;

  Open;
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

procedure TuiIRCClient.LoadProfile(AName: string);
var
  ini: TIniFile;
begin
  inherited;
  ini := TIniFile.Create(Application.Location + 'profiles.ini');
  try
    Profile.Title := Ini.ReadString(AName, 'Title', '');
    Profile.Username := Ini.ReadString(AName, 'Username', '');
    Profile.Password := Ini.ReadString(AName, 'Password', '');
    Profile.Nicknames := Ini.ReadString(AName, 'Nicknames', '');
    Profile.RealName := Ini.ReadString(AName, 'RealName', '');
    Profile.Rooms := Ini.ReadString(AName, 'Rooms', '');
    Profile.Host := Ini.ReadString(AName, 'Host', '');
    Profile.Port := Ini.ReadString(AName, 'Port', '6667');
    Profile.CustomAuth := Ini.ReadString(AName, 'CustomAuth', '');
    Profile.UseSSL := Ini.ReadBool(AName, 'SSL', false);
  finally
    FreeAndNil(ini);
  end;
end;

{ TuiIRCClients }

procedure TuiIRCClients.Open;
var
  itm: TuiIRCClient;
begin
  for itm in Self do
  begin
    itm.Open;
  end;
end;

procedure TuiIRCClients.Close;
var
  itm: TuiIRCClient;
begin
  for itm in Self do
  begin
    itm.Open;
  end;
end;

procedure TuiIRCClients.SaveToFile(FileName: string);
var
  itm: TuiIRCClient;
begin
  for itm in Self do
  begin
    itm.SaveProfile(itm.Profile.Title);
  end;
end;

procedure TuiIRCClients.LoadFromFile(FileName: string);
var
  ini: TIniFile;
  Sections: TStringList;
  s: string;
begin
  ini := TIniFile.Create(FileName);
  try
    Sections := TStringList.Create;
    ini.ReadSections(Sections);
    for s in Sections do
    begin
      with TuiIRCClient.Create do
        LoadProfile(s);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

procedure TuiIRCClients.AddProfile(AProfile: TServerProfile);
begin
  with TuiIRCClient.Create do
  begin
    Profile := AProfile;
  end;
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
      MainFrm.ConnectBtn.Caption := 'Connect';
      while MainFrm.MsgPageControl.PageCount > 0 do
        MainFrm.MsgPageControl.Page[0].Free;
    end;
    prgConnecting:
      MainFrm.ConnectBtn.Caption := 'Connecting';
    prgConnected:
    begin
      MainFrm.ConnectBtn.Caption := 'Disconnect';
      MainFrm.WelcomePnl.Visible := False;
      MainFrm.ChatPnl.Visible := True;
      MainFrm.SendEdit.SetFocus;
    end;
    prgReady:
    begin
      MainFrm.ConnectBtn.Caption := 'Disconnect';
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
    IRCClients.Close
  else
  begin
    ConnectNow;
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

procedure TMainFrm.Button1Click(Sender: TObject);
var
  Profile: TServerProfile;
begin
  if ShowServerProfile(Profile) then
  begin
    IRCClients.AddProfile(Profile);
    EnumProfiles;
    ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(Profile.Title);
  end;
end;

procedure TMainFrm.Button2Click(Sender: TObject);
begin
  if ProfileCbo.Text <> '' then
  begin
    DeleteProfile(ProfileCbo.Text);
    EnumProfiles;
  end;
end;

procedure TMainFrm.ConnectNow;
begin
  SaveConfig;
  IRCClients.Open;
end;

procedure TMainFrm.HostEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ConnectNow;
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
begin

end;

procedure TMainFrm.PasswordEditChange(Sender: TObject);
begin

end;

procedure TMainFrm.ProfileCboSelect(Sender: TObject);
begin
  //LoadProfile(ProfileCbo.Text);
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
  MsgPageControl.ActivePageIndex := 0;
  LogEdit.Clear;
  IRCClients.LoadFromFile(Application.Location + 'profiles.ini');
  EnumProfiles;
  ProfileCbo.ItemIndex := ProfileCbo.Items.IndexOf(aProfile);
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
end;

destructor TMainFrm.Destroy;
begin
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
begin
  ProfileCbo.Clear;
  for i := 0 to IRCClients.Count -1 do
  begin
    ProfileCbo.Items.Add(IRCClients[i].Profile.Title);
  end;
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

end.

