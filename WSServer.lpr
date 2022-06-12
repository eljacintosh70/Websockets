program WSServer;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  classes,
  SysUtils,
  fgl,
  wsutils,
  wsmessages,
  wsstream,
  websocketserver,
  WSServerControl;

type

  TWSFunction = procedure (connectionId: TWebsocketCommunicator; body: UTF8String) of object;
  TConnectionMap = class(specialize TFPGMap<UTF8String, TWebsocketCommunicator>);
  TFnMap = class(specialize TFPGMap<UTF8String, TWSFunction>);

  { TSocketHandler2 }

  TSocketHandler2 = class(TSocketHandler)     
  protected
    names: TConnectionMap;
    FnList: TFnMap;
    procedure dataTo(connectionId: TWebsocketCommunicator; data: UTF8String);
    procedure messageToId(connectionId: TWebsocketCommunicator; obj: UTF8String);
    procedure messageToName(name, obj: UTF8String);
    procedure messageToAll(obj: UTF8String);

    procedure loginFn(connectionId: TWebsocketCommunicator; body: UTF8String);
    procedure getUsersFn(connectionId: TWebsocketCommunicator; body: UTF8String);
    procedure sendMessageFn(connectionId: TWebsocketCommunicator; body: UTF8String);
              
    procedure disconnectFn(connectionId: TWebsocketCommunicator); override;
    procedure HandleMessageFn(connectionId: TWebsocketCommunicator; body: UTF8String); override;
  public
    constructor Create; override;
  end;


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

procedure TSocketHandler2.dataTo(connectionId: TWebsocketCommunicator;
  data: UTF8String);
begin                            
  connectionId.WriteStringMessage(data);
end;

procedure TSocketHandler2.messageToId(connectionId: TWebsocketCommunicator;
  obj: UTF8String);
begin
  connectionId.WriteStringMessage(obj);
end;

procedure TSocketHandler2.messageToName(name, obj: UTF8String);
var
  id: TWebsocketCommunicator;
begin
  if names.TryGetData(name, id) then
    messageToId(id, obj)
end;

procedure TSocketHandler2.messageToAll(obj: UTF8String);
var
  i, n: Integer;
  Id: TWebsocketCommunicator;
begin
  n := names.Count;
  for i := 0 to n - 1 do
  begin
    Id := names.Data[i];
    messageToId(Id, obj);
  end;
end;

procedure TSocketHandler2.loginFn(connectionId: TWebsocketCommunicator;
  body: UTF8String);
var
  name: String;
begin
  name := JsonStrValue('name', body);
  if names.IndexOfData(connectionId) >= 0 then
    messageToAll(Format('{"action": "logout", "name": "%s", "connectionId": "%p"}', [
      name, Pointer(connectionId)]));
  names.Add(name, connectionId);
  messageToAll(Format('{"action": "login", "name": "%s", "connectionId": "%p"}', [
    name, Pointer(connectionId)]))
end;

procedure TSocketHandler2.getUsersFn(connectionId: TWebsocketCommunicator;
  body: UTF8String);
var
  name, s: String;
  i, n: Integer;
begin
  s := '{"action": "listUsers", "users": [';
  n := names.Count;
  if n > 0 then
    s := s + '"' + names.Keys[0] + '"';
  for i := 1 to n - 1 do
    s := s + ', "' + names.Keys[i] + '"';
  s := s + ']}';
  messageToId(connectionId, s);
end;

procedure TSocketHandler2.sendMessageFn(connectionId: TWebsocketCommunicator;
  body: UTF8String);
var
  target, message: String;
begin
  target := JsonStrValue('to', body);
  message := JsonStrValue('message', body);
  if target = '' then
      messageToAll(message)
  else
      messageToName(target, message)
end;

procedure TSocketHandler2.disconnectFn(connectionId: TWebsocketCommunicator);
var
  i: Integer;        
  name: String;
begin
  i := names.IndexOfData(connectionId);
  if i >= 0 then
  begin
    name := names.Keys[i];
    names.Delete(i);
    messageToAll(Format('{"action": "logout", "name": "%s", "connectionId": "%p"}', [
      name, Pointer(connectionId)]))
  end;
end;

procedure TSocketHandler2.HandleMessageFn(connectionId: TWebsocketCommunicator;
  body: UTF8String);
var
  action: String;
  Fn: TWSFunction;
begin
  action := JsonStrValue('action', body);
  if FnList.TryGetData(action, Fn) then
    Fn(connectionId, body)
end;

constructor TSocketHandler2.Create;
begin
  names := TConnectionMap.Create;
  FnList := TFnMap.Create;
  FnList.Add('login',       @loginFn);
  FnList.Add('getUsers',    @getUsersFn);
  FnList.Add('sendMessage', @sendMessageFn);
  inherited Create;
end;


begin
  RunServer(TSocketHandler2);
end.
