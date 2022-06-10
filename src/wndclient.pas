unit WndClient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  WSControl;

const
  {
  UseSLL = False;
  ServerUrl =  '127.0.0.1';
  ServerPort = 8080;
  ServerPath = '';
  }
  UseSLL = True;
  ServerUrl =  'XXXXX.execute-api.YYYY.amazonaws.com';
  ServerPort = 443;
  ServerPath = '/production';

type

  { TForm1 }

  TForm1 = class(TForm)  
    PaLeft: TPanel; 
    PaTop: TPanel;       
    LUser: TLabel;   
    EdUser: TEdit; 
    BConnect: TButton;  
    LbUsers: TListBox;  
    Splitter1: TSplitter;   
    PaClient: TPanel;   
    EdLog: TMemo;
    PaMessage: TPanel; 
    LMessage: TLabel;
    BSendAll: TButton;
    BSendSel: TButton;
    EdMessage: TMemo;
    procedure BConnectClick(Sender: TObject);
    procedure BSendAllClick(Sender: TObject);
    procedure BSendSelClick(Sender: TObject);
  private
    chat: TSimpleChat;
  public
    procedure Log(s: String);     
    procedure JsonIn(s: String);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

// solo para no usar ninguna librería de Json
function JsonStrValue(key, s: String): String;
var
  i, i1, i2: Integer;
begin
  key := '"' + key + '":';
  i := Pos(key, s);
  if i > 0 then
  begin
    Inc(i, Length(key));
    i1 := Pos('"', s, i);
    if i1 > 0 then
    begin
      i2 := Pos('"', s, i1 + 1);
      if i2 > 0 then
      begin
        Result := s.Substring(i1, i2 - i1 - 1);
        Exit;
      end;
    end;
  end;
  Result := '';
end;

function JsonArrValue(key, s: String): String;
var
  i, i1, i2: Integer;
begin
  key := '"' + key + '":';
  i := Pos(key, s);
  if i > 0 then
  begin
    Inc(i, Length(key));
    i1 := Pos('[', s, i);
    if i1 > 0 then
    begin
      i2 := Pos(']', s, i1 + 1);
      if i2 > 0 then
      begin
        Result := s.Substring(i1, i2 - i1 - 1);
        Exit;
      end;
    end;
  end;
  Result := '';
end;

{ TForm1 }

procedure TForm1.Log(s: String);
begin
  EdLog.Lines.Add(s);
end;

procedure TForm1.JsonIn(s: String);
var
  i2: Integer;
  sAction, s2: String;
begin
  if s.StartsWith('{"action":') then
  begin
    sAction := JsonStrValue('action', s);
    if sAction = 'listUsers' then
    begin
      s2 := JsonArrValue('users', s);     
      LbUsers.Items.CommaText := s2;
    end
    else if sAction = 'login' then
    begin
      s2 := JsonStrValue('name', s);
      i2 := LbUsers.Items.IndexOf(s2);
      if i2 = -1 then
        LbUsers.Items.Add(s2);
    end
    else if sAction = 'logout' then
    begin
      s2 := JsonStrValue('name', s);
      i2 := LbUsers.Items.IndexOf(s2);
      if i2 <> -1 then
        LbUsers.Items.Delete(i2);
    end
  end;
end;

procedure TForm1.BConnectClick(Sender: TObject);
var
  UserName: String;
begin
  if not Assigned(chat) then
    chat := InitChat(ServerUrl, ServerPort, ServerPath, UseSLL, @Log, @JsonIn);
  UserName := EdUser.Text;
  chat.Login(UserName);
  Sleep(200);
  chat.ListUsers;
end;

procedure TForm1.BSendAllClick(Sender: TObject);
var
  s, Message: String;
begin
  s := EdMessage.Lines.Text;
  Message := '{"action": "sendMessage", "message": "' + s + '"}';
  chat.Message(Message);
end;

procedure TForm1.BSendSelClick(Sender: TObject);
var
  i: Integer;       
  s, User, Message: String;
begin
  i := LbUsers.ItemIndex;
  if i >= 0 then
  begin
    User := LbUsers.Items[i];
    s := EdMessage.Lines.Text;
    Message := '{"action": "sendMessage", "to": "' + User + '", "message": "' + s + '"}';
    chat.Message(Message);
  end;
end;

end.

