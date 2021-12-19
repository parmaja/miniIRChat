unit ChatRoomFrames;
{$mode objfpc}{$H+}
{$define use_webbrowser}
{**
 *  This file is part of the "miniIRChat"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus, Graphics,
  LCLType, PairSplitter,
  {$ifdef use_webbrowser}
  IpHtml,
  //HtmlView, HTMLSubs,
  {$endif}
  SynEdit, SynHighlighterMulti,
  mnIRCClients, ntvPanels;

type

  { TChatRoomFrame }

  TChatRoomFrame = class(TFrame)
    ChangeTopicBtn: TButton;
    MenuItem1: TMenuItem;
    AdminPnl: TPanel;
    UsersPnl: TntvPanel;
    SaveAsHtmlMnu: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    TopicEdit: TEdit;
    UserListBox: TListView;
    WhoIsMnu: TMenuItem;
    OpMnu: TMenuItem;
    MenuItem2: TMenuItem;
    UsersPopupMenu: TPopupMenu;
    procedure ChangeTopicBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure OpMnuClick(Sender: TObject);
    procedure SaveAsHtmlMnuClick(Sender: TObject);
    procedure WhoIsMnuClick(Sender: TObject);
  private
    function GetCurrentUser: string;
  protected
    {$ifdef use_webbrowser}
    //Viewer: THtmlViewer;
    Body: TIpHtmlNodeBODY;
    Viewer: TIpHtmlPanel;
    procedure HtmlEnumerator(Document: TIpHtml);
    {$else}
    MsgEdit: TSynEdit;
    {$endif}
  public
    IRCClient: TmnIRCClient;
    ChannelName: string;
    IsChannel: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure AddMessage(aMsg: string; AClassName: string = ''; IsHeader: Boolean = False);
    procedure SetTopic(Topic: string);
    procedure UserJoin(UserName: string);
    procedure UserLeft(UserName: string);
    procedure UserModeChanged(UserName: string);
    procedure ReceiveNames(vUserNames: TIRCChannel);
    function SendMessage(vMsg: string): Boolean;
  end;

function CreateChatHTMLStream: TStream;

implementation

function CreateChatHTMLStream: TStream;
begin
  Result := TResourceStream.Create(hInstance, 'ChatHtml', RT_RCDATA);
end;

{$R *.lfm}

{ TChatRoomFrame }

procedure TChatRoomFrame.OpMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRCClient.OpUser(ChannelName, aUser);
end;

procedure TChatRoomFrame.MenuItem1Click(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRCClient.OpUser(ChannelName, aUser);
end;

procedure TChatRoomFrame.SaveAsHtmlMnuClick(Sender: TObject);
begin
end;

procedure TChatRoomFrame.WhoIsMnuClick(Sender: TObject);
var
  aUser: string;
begin
  aUser := GetCurrentUser;
  if aUser <> '' then
    IRCClient.WhoIs(aUser);
end;

function TChatRoomFrame.GetCurrentUser: string;
begin
  Result := '';
  if UserListBox.Items.Count > 0 then
  begin
    Result := UserListBox.Selected.Caption;
  end;
end;

{$ifdef use_webbrowser}
procedure TChatRoomFrame.HtmlEnumerator(Document: TIpHtml);
var
   n: TIpHtmlNode;
//   nb: TIpHtmlNodeBODY;
   i: Integer;
begin
   if not Assigned(Document.HtmlNode) then begin
      Exit;
   end;
   if Document.HtmlNode.ChildCount < 1 then begin
      Exit;
   end;
   for i := 0 to Document.HtmlNode.ChildCount -1 do
   begin
     n := Document.HtmlNode.ChildNode[i];
     if (n is TIpHtmlNodeBODY) then
     begin
       Body := TIpHtmlNodeBODY(n);
       exit;
     end;
   end;
end;
{$endif}

procedure TChatRoomFrame.ChangeTopicBtnClick(Sender: TObject);
begin

end;

constructor TChatRoomFrame.Create(TheOwner: TComponent);
var
  i: Integer;
  aStream: TStream;
begin
  inherited Create(TheOwner);
  {$ifdef use_webbrowser}
  HandleAllocated;
  //Viewer := THtmlViewer.Create(Self);
  Viewer := TIpHtmlPanel.Create(Self);
  with Viewer do
  begin
    Parent := Self;
    Align := alClient;
    BorderStyle := bsNone;
    MarginHeight := 10;
    MarginWidth := 10;
    Visible := True;
    Font.Name := 'Courier New';
    Font.Size := 10;
    //ScrollBars := ssAutoVertical;
  end;
  //Viewer.LoadFromFile(Application.Location + 'chat.html');
  aStream:= CreateChatHTMLStream;
  try
    Viewer.Font.Name := 'Courier New';
    Viewer.Font.Size := 8;
    Viewer.SetHtmlFromStream(aStream);
    //Viewer.SetHtmlFromFile(Application.Location + 'chat.html');
  finally
    FreeAndNil(aStream);
  end;
  //Find Body
  //Viewer.EnumDocuments(@HtmlEnumerator);
  for i :=0 to Viewer.MasterFrame.Html.HtmlNode.ChildCount - 1 do
    if Viewer.MasterFrame.Html.HtmlNode.ChildNode[i] is TIpHtmlNodeBODY then
    begin
      Body := TIpHtmlNodeBODY(Viewer.MasterFrame.Html.HtmlNode.ChildNode[i]);
      break;
    end;
  {$else}
  MsgEdit := TSynEdit.Create(TheOwner);
  with MsgEdit do
  begin
    Parent := Self;
    //ParentWindow := Handle;
    Align := alClient;
    ScrollBars := ssAutoVertical;
    ReadOnly := True;
    Gutter.Visible := False;
    Options := Options + [eoHideRightMargin];
  end;
  {$endif}
end;

procedure TChatRoomFrame.AddMessage(aMsg: string; AClassName: string; IsHeader: Boolean);
{$ifdef use_webbrowser}
var
  TextNode: TIpHtmlNodeText;
  Node: TIpHtmlNodeInline;
{$endif}
begin
  {$ifdef use_webbrowser}
  if IsHeader then
  begin
    Node := TIpHtmlNodeHeader.Create(Body);
    (Node as TIpHtmlNodeHeader).Size := 4;
  end
  else
    Node := TIpHtmlNodeP.Create(Body);

  Node.ClassId := AClassName;

  TextNode := TIpHtmlNodeText.Create(Node);
  TextNode.AnsiText := aMsg;

  with TIpHtmlNodeBR.Create(Body) do
  begin
  end;

  Viewer.Update;
  Viewer.Scroll(hsaEnd);
  {$else}
  MsgEdit.Lines.Add(aMSG);
  MsgEdit.CaretY := MsgEdit.Lines.Count;
  //MsgEdit.ScrollBy(0, 1);
  {$endif}
end;

procedure TChatRoomFrame.SetTopic(Topic: string);
begin
  TopicEdit.Text := Topic;
end;

procedure TChatRoomFrame.UserJoin(UserName: string);
var
  aItem: TListItem;
  oUser: TIRCUser;
begin
  aItem := UserListBox.Items.FindCaption(0, UserName, False, True, False, False);
  if aItem = nil then
  begin
    aItem := UserListBox.Items.Add;
    aItem.Caption := UserName;
    oUser := IRCClient.Session.Channels.FindUser(ChannelName, UserName);
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

procedure TChatRoomFrame.UserLeft(UserName: string);
var
  aItem: TListItem;
begin
  aItem := UserListBox.Items.FindCaption(0, UserName, False, True, False, False);
  if aItem <> nil then
    UserListBox.items.Delete(aItem.Index);
end;

procedure TChatRoomFrame.ReceiveNames(vUserNames: TIRCChannel);
var
  oUser: TIRCUser;
  aItem: TListItem;
  i: Integer;
begin
  UsersPnl.Visible := True;
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

function TChatRoomFrame.SendMessage(vMsg: string): Boolean;
begin
  if IRCClient.Online then
  begin
    IRCClient.SendMsg(ChannelName, vMsg);
    Result := True;
  end
  else
    Result :=False;
end;

procedure TChatRoomFrame.UserModeChanged(UserName: string);
var
  aItem: TListItem;
  oUser: TIRCUser;
begin
  aItem := UserListBox.Items.FindCaption(0, UserName, False, True, False, False);
  if aItem = nil then
  begin
    aItem := UserListBox.Items.Add;
    aItem.Caption := UserName;
    aItem.ImageIndex := 0;
  end;
  oUser := IRCClient.Session.Channels.FindUser(ChannelName, UserName);
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

end.

