import json
import urllib3
import boto3

ApiURL = 'https://XXXXX.execute-api.YYYY.amazonaws.com/production'

ok = {'statusCode': 200}
client = boto3.client('apigatewaymanagementapi', endpoint_url=ApiURL)
names = {}

def dataTo(connectionId, data):
    respons = client.post_to_connection(ConnectionId=connectionId, Data=data)
    return respons
    
def messageToId(connectionId, obj):
    data = json.dumps(obj).encode('utf-8')
    return dataTo(connectionId, data)
        
def messageToName(name, obj):
    for id in names:
        if names[id] == name:
            return messageToId(id, obj)
    return ok

def messageToAll(obj):
    data = json.dumps(obj).encode('utf-8')
    respons = ok
    for connectionId in names:
      respons = dataTo(connectionId, data)
    return respons
    
def loginFn(connectionId, body):
    name = body['name']
    names[connectionId] = name
    # print (body)
    messageToAll({'action': 'login',
            'name': name, 'connectionId': connectionId})
    return ok

def getUsersFn(connectionId, body):
    # list = [k for k in names]
    list = [names[k] for k in names]
    messageToId(connectionId, {'action': 'listUsers', 'users': list})
    return ok

def sendMessageFn(connectionId, body):
    target = body.get('to', None)
    message = body['message']
    if target is None: 
        messageToAll(message)
    else:
        messageToName(target, message)
    return ok
    
FnList = {
    'login':       loginFn,
    'getUsers':    getUsersFn,
    'sendMessage': sendMessageFn
}

def unknownFn(connectionId, action, event):
    print('UNKNOWN: ', action)
    print('--- ', event)
    return ok

def defaultFn(connectionId, event):
    print('**', event)
    body = json.loads(event['body'])
    action = body['action']
    print('->action:', action)
    Fn = FnList.get(action, None)
    if Fn is not None:
        return Fn(connectionId, body)
    return unknownFn(connectionId, action, event)
    
def connectFn(connectionId, event):
    return ok 
    
def disconnectFn(connectionId, event):
    name = names[connectionId]
    names.pop(connectionId, None)
    messageToAll({'action': 'logout',
            'name': name, 'connectionId': connectionId})
    return ok
    
RouteFn = {
    '$connect':    connectFn,
    '$disconnect': disconnectFn
}

def lambda_handler(event, context):
    requestContext = event['requestContext'] 
    connectionId = requestContext['connectionId']
    routeKey = requestContext['routeKey']
    
    print('connectionId: ', connectionId)
    print('routeKey: ', routeKey)
    
    Fn = RouteFn.get(routeKey, defaultFn)
    return Fn(connectionId, event) 
