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
  Dialogs, Buttons, IniFiles, StdCtrls, ExtCtrls, ComCtrls, Menus, LCLType,
  ActnList, TypInfo, mnMsgBox, GUIMsgBox, ntvPanels, mnLogs, mnClasses,
  IRChatClasses, ChatRoomFrames, ServerForm, mnIRCClients;

type

  { TuiIRCClient }

  TuiIRCChatClient = class(TIRCChatClient)
  private
  protected
    procedure DoLog(S: string); override;
    procedure DoMyInfoChanged; override;
    procedure DoConnected; override;
    procedure DoDisconnected; override;
    procedure DoUserChanged(vChannel: string; vUser, vNewNick: string); override;
    procedure DoProgressChanged; override;
    procedure DoUsersChanged(vChannelName: string; vChannel: TIRCChannel); override;
    procedure DoWhoIs(vUser: string); override;
    procedure DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string); override;
  public
  end;

  { TMainFrm }

  TMainFrm = class(TForm)
    ChatPnl: TPanel;
    ConnectBtn1: TButton;
    ExitAct: TAction;
    ActionList: TActionList;
    AddBtn: TButton;
    ExitBtn: TButton;
    LogEdit: TMemo;
    MenuItem2: TMenuItem;
    MsgPageControl: TPageControl;
    NicknameBtn: TButton;
    LogPnl: TntvPanel;
    SendBtn: TButton;
    SendEdit: TMemo;
    SendPnl: TPanel;
    ShowMnu: TMenuItem;
    ExitMnu: TMenuItem;
    DeleteBtn: TButton;
    EditBtn: TButton;
    StatusPnl: TPanel;
    TrayIcon: TTrayIcon;
    TrayPopupMenu: TPopupMenu;
    OptionsBtn: TButton;
    ProfileCbo: TComboBox;
    Label6: TLabel;
    ConnectBtn: TButton;
    JoinBtn: TButton;
    MenuItem1: TMenuItem;
    LogPopupMenu: TPopupMenu;
    Panel2: TPanel;
    SmallImageList: TImageList;
    procedure AddBtnClick(Sender: TObject);
    procedure ConnectBtn1Click(Sender: TObject);
    procedure EditBtnClick(Sender: TObject);
    procedure ExitActExecute(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
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
    procedure PasswordEditChange(Sender: TObject);
    procedure ProfileCboSelect(Sender: TObject);
    procedure SendBtnClick(Sender: TObject);
    procedure SendEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    FDestroying: Boolean;
    Recents: TStringList;
    RecentsIndex: Integer;
    procedure ForceForegroundWindow;
    procedure HideApp;
    procedure RecentUp;
    procedure RecentDown;
    procedure AddRecent(S: string);
    procedure SendNow;
    function NeedRoom(vClient: TuiIRCChatClient; vRoomName: string; ActiveIt: Boolean = false): TChatRoomFrame;
    procedure SetNick(ANick: string);
    procedure ShowApp;

    function CurrentChannelFrame: TChatRoomFrame;
    function CurrentChannelName: string;
  public
    StartMinimized: Boolean;
    ShowTray: Boolean;
    AutoStart: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteProfile(ProfileName: string);
    procedure EnumProfiles;
    procedure SaveConfig;

    procedure LogMsg(S: string);
    procedure IRCStatusChanged(Sender: TuiIRCChatClient);
    procedure IRCLogMessage(Sender: TuiIRCChatClient; S: string);
    procedure IRCReceive(Sender: TuiIRCChatClient; vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
    procedure IRCReceiveNames(Sender: TuiIRCChatClient; vChannel: string; vUserNames: TIRCChannel);

  end;

  { TuiIRCChatClients }

  TuiIRCChatClients = class(TIRCChatClients)
  public
    function CreateClient: TIRCChatClient; override;
  end;

var
  MainFrm: TMainFrm;
  IRCClients: TuiIRCChatClients = nil;

implementation

{$R *.lfm}

{ TuiIRCChatClients }

function TuiIRCChatClients.CreateClient: TIRCChatClient;
begin
  Result := TuiIRCChatClient.Create;
end;

{ TuiIRCChatClient }

procedure TuiIRCChatClient.DoLog(S: string);
begin
  inherited;
  MainFrm.IRCLogMessage(Self, S);
end;

procedure TuiIRCChatClient.DoMyInfoChanged;
begin
  inherited;
  MainFrm.IRCStatusChanged(Self);
end;

procedure TuiIRCChatClient.DoConnected;
begin
  inherited DoConnected;
  MainFrm.IRCLogMessage(Self, 'Yes it is connected');
end;

procedure TuiIRCChatClient.DoDisconnected;
begin
  MainFrm.IRCLogMessage(Self, 'Yes it is disconnected');
  inherited;
end;

procedure TuiIRCChatClient.DoUserChanged(vChannel: string; vUser, vNewNick: string);
begin
  inherited;
  //TODO
end;

procedure TuiIRCChatClient.DoProgressChanged;
begin
  inherited;
  case Progress of
    prgDisconnected:
    begin
      while MainFrm.MsgPageControl.PageCount > 0 do
        MainFrm.MsgPageControl.Page[0].Free;
    end;
    prgConnecting:;
      //MainFrm.ConnectBtn.Caption := 'Connecting';
    prgConnected:
    begin
      MainFrm.SendEdit.SetFocus;
    end;
    prgReady:
    begin
      //MainFrm.ConnectBtn.Caption := 'Disconnect';
    end;
  end;
end;

procedure TuiIRCChatClient.DoUsersChanged(vChannelName: string; vChannel: TIRCChannel);
begin
  inherited;
  MainFrm.IRCReceiveNames(Self, vChannelName, vChannel);
end;

procedure TuiIRCChatClient.DoWhoIs(vUser: string);
var
  aUser: TIRCUser;
begin
  inherited;
  aUser := Session.Channels.FindUser('', vUser);
  if aUser <> nil then
  begin
  //TODO should in server page
    MainFrm.IRCLogMessage(Self, aUser.WhoIs.RealName);
    MainFrm.IRCLogMessage(Self, aUser.WhoIs.Server);
    MainFrm.IRCLogMessage(Self, aUser.WhoIs.Channels);
  end;
end;

procedure TuiIRCChatClient.DoReceive(vMsgType: TIRCMsgType; vChannel, vUser, vMsg: string);
begin
  MainFrm.IRCReceive(Self, vMsgType, vChannel, vUser, vMsg);
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
  {$ifdef D+}
  LogPnl.Visible := true;
  {$endif}
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

procedure TMainFrm.ConnectBtn1Click(Sender: TObject);
begin
  IRCClients.Open(ProfileCbo.Text);
end;

procedure TMainFrm.EditBtnClick(Sender: TObject);
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

procedure TMainFrm.ExitActExecute(Sender: TObject);
begin
  FDestroying := True;
  Close;
end;

procedure TMainFrm.ExitBtnClick(Sender: TObject);
begin

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
  if CurrentChannelFrame <> nil then
  begin
    Rooms := '';
    if MsgBox.Input(Rooms, 'Join Rooms') and (Rooms <> '') then
      CurrentChannelFrame.IRCClient.Join(Rooms);
  end;
end;

procedure TMainFrm.MenuItem1Click(Sender: TObject);
begin
  LogEdit.Clear;
end;

procedure TMainFrm.MsgPageControlChange(Sender: TObject);
begin
  if CurrentChannelFrame <> nil then
  begin
    NicknameBtn.Caption := CurrentChannelFrame.IRCClient.Session.Nick;
  end;
end;

procedure TMainFrm.NicknameBtnClick(Sender: TObject);
var
  aNick: string;
begin
  if CurrentChannelFrame <> nil then
  begin
    aNick := CurrentChannelFrame.IRCClient.Session.Nick;
    if MsgBox.Input(aNick, 'New Nickname?') then
    begin
      CurrentChannelFrame.IRCClient.SetNick(aNick);
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
  if CurrentChannelFrame <> nil then
  begin
    s := TrimRight(SendEdit.Text);
    if s <> '' then
      if CurrentChannelFrame.SendMessage(s) then
      begin
        AddRecent(SendEdit.Text);
        SendEdit.Text := '';
      end;
  end;
end;

function TMainFrm.NeedRoom(vClient: TuiIRCChatClient; vRoomName: string; ActiveIt: Boolean): TChatRoomFrame;
var
  i, Index: Integer;
  TabSheet: TTabSheet;
  ARoomName: string;
  AIsChannel: Boolean;
  aChatFrame: TChatRoomFrame;
begin
  if vRoomName = '*' then
    vRoomName := '';

  ARoomName := vRoomName;
  if LeftStr(ARoomName, 1) = '#' then
  begin
    ARoomName := MidStr(ARoomName, 2, MaxInt);
    AIsChannel := True;
  end
  else
    AIsChannel := False;

  index := -1;
  for i := 0 to MsgPageControl.PageCount - 1 do
  begin
    aChatFrame := (MsgPageControl.Pages[i].Controls[0] as TChatRoomFrame);
    if (aChatFrame.IRCClient = vClient) and SameText(aChatFrame.ChannelName, vRoomName) then
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
      IRCClient := vClient;
      Parent := TabSheet;
      Align := alClient;
      ChannelName := vRoomName;
      IsChannel := AIsChannel;
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

procedure TMainFrm.IRCLogMessage(Sender: TuiIRCChatClient; S: string);
begin
  LogMsg(S);
end;

function TMainFrm.CurrentChannelFrame: TChatRoomFrame;
begin
  if MsgPageControl.ActivePage <> nil then
  begin
    if (MsgPageControl.ActivePage.Controls[0] is TChatRoomFrame) then
      Result := (MsgPageControl.ActivePage.Controls[0] as TChatRoomFrame)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

function TMainFrm.CurrentChannelName: string;
begin
  if MsgPageControl.ActivePage <> nil then
  begin
    Result := (MsgPageControl.ActivePage.Controls[0] as TChatRoomFrame).ChannelName;
  end
  else
    Result := '';
end;

constructor TMainFrm.Create(TheOwner: TComponent);
var
  i: Integer;
  ini: TIniFile;
  aProfile: string;
  //aStream: TStream;
begin
  inherited;
  InstallFileLog('log.txt');
  InstallEventLog(@LogMsg);
  {$ifdef DEBUG}
  InstallConsoleLog;
  InstallDebugOutputLog;
  {$endif}
  {$macro on}
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
  IRCClients := TuiIRCChatClients.Create;
  IRCClients.Profiles.ProfileFileName := Application.Location + 'profiles.ini';
  MsgPageControl.ActivePageIndex := 0;
  LogEdit.Clear;
  IRCClients.Profiles.LoadProfiles;
  EnumProfiles;
  i := ProfileCbo.Items.IndexOf(aProfile);
  if i < 0 then
    i := 0;
  ProfileCbo.ItemIndex := i;
{
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
}
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

procedure TMainFrm.LogMsg(S: string);
begin
  LogEdit.Lines.Add(S)
end;

procedure TMainFrm.IRCStatusChanged(Sender: TuiIRCChatClient);
begin
  if CurrentChannelFrame <> nil then
  begin
    NicknameBtn.Caption := CurrentChannelFrame.IRCClient.Session.Nick;
  end;
end;

procedure TMainFrm.IRCReceive(Sender: TuiIRCChatClient; vMsgType: TIRCMsgType; vChannel, vUser, vMsg: String);
var
  ChatFrame: TChatRoomFrame;
begin
  if vChannel = '' then
    IRCLogMessage(Sender, vMSG)
  else
  begin
    ChatFrame := NeedRoom(Sender, vChannel);
    if ChatFrame <> nil then
      begin
        case vMsgType of
          mtWelcome:
          begin
            ChatFrame.AddMessage(vMSG);
            ChatFrame.SetTopic(vMSG);
          end;
          mtMOTD:
            ChatFrame.AddMessage(vMSG, 'code');
          mtTopic:
          begin
            ChatFrame.AddMessage('Topic:  ' + vMSG, '', True);
            ChatFrame.SetTopic(vMSG);
          end;
          mtJoin:
          begin
            ChatFrame.AddMessage(vUser + ' is joined', 'hint');
            ChatFrame.UserJoin(vUser);
          end;
          mtLeft:
          begin
            //if me close the tab
            ChatFrame.AddMessage(vUser + ' is left: ' + vMsg, 'hint');
            ChatFrame.UserLeft(vUser);
          end;
          mtUserMode:
          begin
            ChatFrame.UserModeChanged(vUser);
          end;
          mtNotice:
            ChatFrame.AddMessage('[' + vUser + '] ' + vMSG, 'notice');
          mtMessage:
          begin
            ChatFrame.AddMessage(vUser + ': ' + vMSG, ' received');
          end;
          mtSend:
          begin
            ChatFrame.AddMessage(vUser + ': ' + vMSG, 'self');
          end;
          mtAction:
          begin
            ChatFrame.AddMessage('* ' + vUser + ': -' + vMSG + '-', 'action');
          end;
        else
            ChatFrame.AddMessage(vUser + ': ' + vMSG, 'log');
        end;
      end;
   end;
end;

procedure TMainFrm.IRCReceiveNames(Sender: TuiIRCChatClient; vChannel: string; vUserNames: TIRCChannel);
var
  ChatFrame: TChatRoomFrame;
begin
  ChatFrame := NeedRoom(Sender, vChannel);
  ChatFrame.ReceiveNames(vUserNames);
end;

(*
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
end; *)

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

