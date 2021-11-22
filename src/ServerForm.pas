unit ServerForm;
{$mode ObjFPC}{$H+}
{**
 *  This file is part of the "Mini Library"
 *  @license  MIT (https://opensource.org/licenses/MIT)
 *  @author by Zaher Dirkey <zaher, zaherdirkey>
*}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  mnIRCClients, IRChatClasses;

type

  { TServerForm }

  TServerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    HostEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    NicknameEdit: TEdit;
    AuthCbo: TComboBox;
    CustomAuthEdit: TEdit;
    RealNameEdit: TEdit;
    TitleEdit: TEdit;
    PasswordEdit: TEdit;
    PortEdit: TEdit;
    RoomsEdit: TEdit;
    UserEdit: TEdit;
    UseSSLChk: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function ShowServerProfile(var Profile: TServerProfile): Boolean;

implementation

function ShowServerProfile(var Profile: TServerProfile): Boolean;
var
  i: Integer;
begin
  with TServerForm.Create(Application) do
  begin
    TitleEdit.Text := Profile.Title;

    HostEdit.Text := Profile.Host;
    PortEdit.Text := Profile.Port;
    UseSSLChk.Checked := Profile.UseSSL;

    NicknameEdit.Text := Profile.NickNames;
    RealNameEdit.Text := Profile.RealName;

    AuthCbo.Items.AddObject('None/Custom', TObject(Ord(authNone)));
    AuthCbo.Items.AddObject('PASS', TObject(Ord(authPASS)));
    AuthCbo.Items.AddObject('IDENTIFY', TObject(Ord(authIDENTIFY)));
    i := AuthCbo.Items.IndexOfObject(TObject(Ord(Profile.Auth)));

    if i < 0 then
      AuthCbo.ItemIndex := 0
    else
      AuthCbo.ItemIndex := i;

    UserEdit.Text := Profile.Username;
    PasswordEdit.Text := Profile.Password;
    CustomAuthEdit.Text := Profile.CustomAuth;

    Result := ShowModal = mrOk;
    if Result then
    begin
      Profile.Title := TitleEdit.Text;

      Profile.Host := HostEdit.Text;
      Profile.Port := PortEdit.Text;
      Profile.UseSSL := UseSSLChk.Checked;

      Profile.NickNames := NicknameEdit.Text;
      Profile.RealName := RealNameEdit.Text;

      Profile.Auth := TIRCAuth(Integer(AuthCbo.Items.Objects[AuthCbo.ItemIndex]));

      Profile.Username := UserEdit.Text;
      Profile.Password := PasswordEdit.Text;
      Profile.CustomAuth := CustomAuthEdit.Text;
    end;
  end;
end;

{$R *.lfm}

{ TServerForm }

procedure TServerForm.FormCreate(Sender: TObject);
begin

end;

end.

