unit WSControl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  sysutils,
  wsutils,
  wsmessages,
  wsstream,
  ssockets,
  opensslsockets, sslsockets,
  WebsocketsClient;

type
  TStrOut = procedure (s: string) of object;

  { TSimpleChat }

  TSimpleChat = class
  private
    FCommunicator: TWebsocketCommunicator;
    procedure ReceiveMessage(Sender: TObject);
    procedure StreamClosed(Sender: TObject);
  public
    StrOut: TStrOut;
    JsonOut: TStrOut;
    procedure Message(s: String);
    procedure Login(UserName: String);
    procedure ListUsers;
    procedure Execute;
    constructor Create(ACommunicator: TWebsocketCommunicator; AStrOut, AJsonOut: TStrOut);
    destructor Destroy; override;
  end;

function InitChat(const ServerUrl: String; const ServerPort: Integer = 80;
  const ServerPath: String = '/'; UseSLL: Boolean = False;
  StrOut: TStrOut = nil; JsonOut: TStrOut = nil): TSimpleChat;

implementation

{ TSimpleChat }

procedure TSimpleChat.StreamClosed(Sender: TObject);
begin
  StrOut('Connection to ' + FCommunicator.SocketStream.RemoteAddress.Address + ' closed');
end;

procedure TSimpleChat.ReceiveMessage(Sender: TObject);
var
  MsgList: TWebsocketMessageOwnerList;
  m: TWebsocketMessage;
  s: String;
begin
  MsgList := TWebsocketMessageOwnerList.Create(True);
  try
    FCommunicator.GetUnprocessedMessages(MsgList);
    for m in MsgList do
      if m is TWebsocketStringMessage then
      begin
        s := TWebsocketStringMessage(m).Data;
        if s.StartsWith('{"') then
          JsonOut(s);
        StrOut('Message from ' + FCommunicator.SocketStream.RemoteAddress.Address + ': ' + s)
      end
      else if m is TWebsocketPongMessage then
        StrOut('Pong from ' + FCommunicator.SocketStream.RemoteAddress.Address + ': ' + TWebsocketPongMessage(m).Data);
  finally
    MsgList.Free;
  end;
end;

procedure TSimpleChat.Message(s: String);
begin         
  FCommunicator.WriteStringMessage(s);
end;

procedure TSimpleChat.Login(UserName: String);
begin
  Message('{"action": "login", "name": "' + UserName + '"}');
end;

procedure TSimpleChat.ListUsers;
begin
  Message('{"action": "getUsers"}');
end;

procedure TSimpleChat.Execute;
var
  str: String;
begin
  while FCommunicator.Open do
  begin
    ReadLn(str);
    if not FCommunicator.Open then
      Exit;
    if str = 'exit' then
    begin
      FCommunicator.WriteMessage(wmtClose).Free;
      while FCommunicator.Open do
        Sleep(100);
    end
    else if str.StartsWith('ping') then
      with FCommunicator.WriteMessage(wmtPing) do
      try
        WriteRaw(str.Substring(5));
      finally
        Free;
      end
    else
      FCommunicator.WriteStringMessage(str);
  end;
end;

constructor TSimpleChat.Create(ACommunicator: TWebsocketCommunicator; AStrOut, AJsonOut: TStrOut);
begin
  StrOut := AStrOut;
  JsonOut := AJsonOut;
  FCommunicator := ACommunicator;
  FCommunicator.OnClose:=@StreamClosed;
  FCommunicator.OnReceiveMessage:=@ReceiveMessage;
  FCommunicator.StartReceiveMessageThread;
end;

destructor TSimpleChat.Destroy;
begin
  FCommunicator.StopReceiveMessageThread;
  while FCommunicator.ReceiveMessageThreadRunning do
    Sleep(10);
  FCommunicator.Free;
  inherited Destroy;
end;

function InitChat(const ServerUrl: String; const ServerPort: Integer = 80;
  const ServerPath: String = '/'; UseSLL: Boolean = False;
  StrOut: TStrOut = nil; JsonOut: TStrOut = nil): TSimpleChat;
var
  client: TWebsocketClient;
  SocketHandler: TSocketHandler;
  chat: TSimpleChat;
begin
  client := TWebsocketClient.Create(ServerUrl, ServerPort, ServerPath);
  try
    if UseSLL then
      SocketHandler := TSSLSocketHandler.GetDefaultHandler
    else
      SocketHandler := TSocketHandler.Create;
    chat := TSimpleChat.Create(client.Connect(SocketHandler), StrOut, JsonOut);
    Result := chat;
    {try
      chat.Execute;
    finally
      chat.Free;
    end;}
  finally
    //client.Free;
  end;
end;

end.

